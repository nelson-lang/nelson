//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useMemo, useRef, useState } from 'react';
import type { DrawPrimitive, FigureWebGlMesh } from '../rpc/types';
import { FigureRasterWebGlCanvas } from './FigureRasterWebGlCanvas';
import { FigureWebGlCanvas } from './FigureWebGlCanvas';
import type { FigureWebGlBackend } from './figureWebGlRenderer';
import {
  canCreateRgbaImageBitmap,
  closeCachedImageSource,
  figureBaseLayerPrimitives,
  figureImageCacheKeys,
  figureRasterBaseLayerPrimitives,
  figureRasterOverlayPrimitives,
  figureTextOverlayPrimitives,
  imageCacheKey,
  isRasterTextureFrameCandidate,
  paintFigure2D,
  rgbaBytes,
  rgbaImageData,
  shouldRefreshRgbaImage,
  type CachedImageSource,
} from './figureCanvasPainter';
import {
  incrementFigureCanvasCounter,
  measureFigureCanvas,
  measureFigureCanvasAsync,
  recordFigureCanvasMeasurement,
  recordFigurePrimitiveProfile,
} from './figureCanvasProfile';
import {
  bitmapTransferFrame,
  clonedTransferFrame,
  closeTransferFrame,
  type FigureCanvasTransferMode,
} from './figureCanvasTransfer';
//=============================================================================
export {
  base64Bytes,
  canCreateRgbaImageBitmap,
  figureBaseLayerPrimitives,
  figureImageCacheKeys,
  figureRasterBaseLayerPrimitives,
  figureRasterOverlayPrimitives,
  figureTextOverlayPrimitives,
  isRasterTextureFrameCandidate,
  rgbaImageData,
  shouldRefreshRgbaImage,
} from './figureCanvasPainter';
//=============================================================================
export type FigureCanvas2DRenderer = 'auto' | 'main' | 'worker';
export type FigureRasterRenderer = 'canvas' | 'webgl';
export type FigureTextOverlayRenderer = 'main' | 'worker';
type FigureCanvasWorkerSession = {
  postFrame(
    primitives: DrawPrimitive[],
    overlayPrimitives: DrawPrimitive[],
    width: number,
    height: number,
    dpr: number,
    panOffset: { dx: number; dy: number } | null,
    transferList?: Transferable[]
  ): void;
  attachTextOverlay(canvas: HTMLCanvasElement): boolean;
  terminate(): void;
};
//=============================================================================
const isKnownCanvas2DRenderer = (value: string | null): value is FigureCanvas2DRenderer =>
  value === 'auto' || value === 'main' || value === 'worker';
const isKnownRasterRenderer = (value: string | null): value is FigureRasterRenderer =>
  value === 'canvas' || value === 'webgl';
const isKnownTextOverlayRenderer = (value: string | null): value is FigureTextOverlayRenderer =>
  value === 'main' || value === 'worker';
const isKnownCanvasTransferMode = (value: string | null): value is FigureCanvasTransferMode =>
  value === 'structured' || value === 'clone' || value === 'bitmap';
export function requestedFigureCanvas2DRenderer(
  queryValue: string | null,
  storedValue: string | null
): FigureCanvas2DRenderer {
  if (isKnownCanvas2DRenderer(queryValue)) return queryValue;
  if (isKnownCanvas2DRenderer(storedValue)) return storedValue;
  return 'auto';
}
//=============================================================================
export function requestedFigureRasterRenderer(
  queryValue: string | null,
  storedValue: string | null
): FigureRasterRenderer {
  if (isKnownRasterRenderer(queryValue)) return queryValue;
  if (isKnownRasterRenderer(storedValue)) return storedValue;
  return 'webgl';
}
//=============================================================================
export function requestedFigureTextOverlayRenderer(
  queryValue: string | null,
  storedValue: string | null
): FigureTextOverlayRenderer {
  if (isKnownTextOverlayRenderer(queryValue)) return queryValue;
  if (isKnownTextOverlayRenderer(storedValue)) return storedValue;
  return 'main';
}
//=============================================================================
export function requestedFigureCanvasTransferMode(
  queryValue: string | null,
  storedValue: string | null
): FigureCanvasTransferMode {
  if (isKnownCanvasTransferMode(queryValue)) return queryValue;
  if (isKnownCanvasTransferMode(storedValue)) return storedValue;
  return 'structured';
}
//=============================================================================
function requestedCanvas2DRenderer(): FigureCanvas2DRenderer {
  const query = new URLSearchParams(window.location.search).get('canvas2d');
  const stored = window.localStorage.getItem('nelson.figureCanvas2DRenderer');
  return requestedFigureCanvas2DRenderer(query, stored);
}
//=============================================================================
function requestedRasterRenderer(): FigureRasterRenderer {
  const query = new URLSearchParams(window.location.search).get('raster');
  const stored = window.localStorage.getItem('nelson.figureRasterRenderer');
  return requestedFigureRasterRenderer(query, stored);
}
//=============================================================================
function requestedTextOverlayRenderer(): FigureTextOverlayRenderer {
  const query = new URLSearchParams(window.location.search).get('textOverlay');
  const stored = window.localStorage.getItem('nelson.figureTextOverlayRenderer');
  return requestedFigureTextOverlayRenderer(query, stored);
}
//=============================================================================
function requestedCanvasTransferMode(): FigureCanvasTransferMode {
  const query = new URLSearchParams(window.location.search).get('canvasTransfer');
  const stored = window.localStorage.getItem('nelson.figureCanvasTransferMode');
  return requestedFigureCanvasTransferMode(query, stored);
}
//=============================================================================
function createFigureCanvasWorkerSession(
  canvas: HTMLCanvasElement,
  onPainted: (elapsedMs: number, refreshElapsedMs?: number) => void
): FigureCanvasWorkerSession | null {
  if (requestedCanvas2DRenderer() === 'main') return null;
  if (typeof Worker !== 'function' || typeof canvas.transferControlToOffscreen !== 'function') {
    return null;
  }
  let worker: Worker | null = null;
  let textOverlayCanvas: HTMLCanvasElement | null = null;
  try {
    worker = new Worker(new URL('./figureCanvasWorker.ts', import.meta.url), {
      type: 'module',
    });
    const offscreen = canvas.transferControlToOffscreen();
    worker.addEventListener(
      'message',
      (event: MessageEvent<{ type?: string; elapsedMs?: number; refreshElapsedMs?: number }>) => {
        if (event.data?.type === 'painted' && typeof event.data.elapsedMs === 'number') {
          onPainted(event.data.elapsedMs, event.data.refreshElapsedMs);
        }
      }
    );
    worker.postMessage({ type: 'init', canvas: offscreen }, [offscreen]);
    return {
      postFrame(primitives, overlayPrimitives, width, height, dpr, panOffset, transferList = []) {
        const message = {
          type: 'frame',
          primitives,
          overlayPrimitives,
          width,
          height,
          dpr,
          panOffset,
        };
        if (transferList.length > 0) {
          worker?.postMessage(message, transferList);
        } else {
          worker?.postMessage(message);
        }
      },
      attachTextOverlay(canvas) {
        if (textOverlayCanvas === canvas) return true;
        if (typeof canvas.transferControlToOffscreen !== 'function') return false;
        try {
          const overlay = canvas.transferControlToOffscreen();
          textOverlayCanvas = canvas;
          worker?.postMessage({ type: 'initOverlay', canvas: overlay }, [overlay]);
          return true;
        } catch {
          return false;
        }
      },
      terminate() {
        worker?.terminate();
        worker = null;
        textOverlayCanvas = null;
      },
    };
  } catch {
    worker?.terminate();
    return null;
  }
}
//=============================================================================
async function rgbaBitmap(
  primitive: Extract<DrawPrimitive, { kind: 'image' }>
): Promise<CachedImageSource | null> {
  if (!canCreateRgbaImageBitmap()) return null;
  return measureFigureCanvasAsync('rgbaBitmapCalls', 'rgbaBitmapMs', async () => {
    const imageData = rgbaImageData(primitive);
    if (!imageData) return null;
    const bitmap = await createImageBitmap(imageData);
    return { source: bitmap, sourceId: primitive.imageId };
  });
}
//=============================================================================
function rgbaCanvas(
  primitive: Extract<DrawPrimitive, { kind: 'image' }>,
  cached?: CachedImageSource
): CachedImageSource | null {
  return measureFigureCanvas('rgbaCanvasCalls', 'rgbaCanvasMs', () => {
    if (
      (!primitive.rgba && !primitive.rgbaBytes) ||
      !primitive.imageWidth ||
      !primitive.imageHeight
    ) {
      return null;
    }
    const bytes = rgbaBytes(primitive);
    const expected = primitive.imageWidth * primitive.imageHeight * 4;
    if (!bytes || bytes.byteLength !== expected) return null;
    const canvas =
      cached?.source instanceof HTMLCanvasElement &&
      cached.source.width === primitive.imageWidth &&
      cached.source.height === primitive.imageHeight
        ? cached.source
        : document.createElement('canvas');
    if (canvas.width !== primitive.imageWidth) canvas.width = primitive.imageWidth;
    if (canvas.height !== primitive.imageHeight) canvas.height = primitive.imageHeight;
    const ctx = canvas.getContext('2d');
    if (!ctx) return null;
    const imageData =
      cached?.rgbaImageData?.width === primitive.imageWidth &&
      cached.rgbaImageData.height === primitive.imageHeight
        ? cached.rgbaImageData
        : ctx.createImageData(primitive.imageWidth, primitive.imageHeight);
    imageData.data.set(bytes);
    ctx.putImageData(imageData, 0, 0);
    return { source: canvas, sourceId: primitive.imageId, rgbaImageData: imageData };
  });
}
//=============================================================================
export function FigureCanvas({
  handle,
  primitives,
  webglMeshes = [],
  softwareFallbackImage,
  width,
  height,
  panOffset = null,
  onWebGlBackendChange,
}: {
  handle: number;
  primitives: DrawPrimitive[];
  webglMeshes?: FigureWebGlMesh[];
  softwareFallbackImage?: string;
  width: number;
  height: number;
  panOffset?: { dx: number; dy: number } | null;
  onWebGlBackendChange?: (backend: FigureWebGlBackend | 'software') => void;
}): JSX.Element {
  const ref = useRef<HTMLCanvasElement>(null);
  const textOverlayRef = useRef<HTMLCanvasElement>(null);
  const imageCacheRef = useRef(new Map<string, CachedImageSource>());
  const workerSessionRef = useRef<FigureCanvasWorkerSession | null>(null);
  const workerDisposalTimer = useRef<number | null>(null);
  const workerUnavailableRef = useRef(false);
  const workerTextOverlayUnavailableRef = useRef(false);
  const rasterRenderer = useMemo(() => requestedRasterRenderer(), []);
  const textOverlayRenderer = useMemo(() => requestedTextOverlayRenderer(), []);
  const canvasTransferMode = useMemo(() => requestedCanvasTransferMode(), []);
  const rasterCandidate = useMemo(() => isRasterTextureFrameCandidate(primitives), [primitives]);
  const rasterLayerCandidate = rasterRenderer === 'webgl' && rasterCandidate;
  const geometryLayerCandidate = webglMeshes.length > 0;
  const overlayPrimitives = useMemo(
    () =>
      rasterLayerCandidate
        ? figureRasterOverlayPrimitives(primitives)
        : figureTextOverlayPrimitives(primitives),
    [primitives, rasterLayerCandidate]
  );
  // Keep labels on the same layer when a filled surface becomes wireframe.
  // Dense raster frames also carry browser-rendered text above their image.
  const needsTextOverlay = overlayPrimitives.length > 0;
  const basePrimitives = useMemo(
    () =>
      needsTextOverlay
        ? rasterLayerCandidate
          ? figureRasterBaseLayerPrimitives(primitives)
          : figureBaseLayerPrimitives(primitives)
        : primitives,
    [needsTextOverlay, primitives, rasterLayerCandidate]
  );
  const [rasterWebGlAvailable, setRasterWebGlAvailable] = useState(false);
  const rasterWebGlActive = rasterLayerCandidate && rasterWebGlAvailable;

  useEffect(() => {
    // Strict Mode replays setup/cleanup on the same canvas. Its offscreen
    // ownership cannot be transferred twice, so keep the session for that replay.
    if (workerDisposalTimer.current !== null) {
      window.clearTimeout(workerDisposalTimer.current);
      workerDisposalTimer.current = null;
    }
    return () => {
      for (const cached of imageCacheRef.current.values()) closeCachedImageSource(cached);
      imageCacheRef.current.clear();
      workerDisposalTimer.current = window.setTimeout(() => {
        workerSessionRef.current?.terminate();
        workerSessionRef.current = null;
        workerDisposalTimer.current = null;
      }, 0);
    };
  }, []);

  useEffect(() => {
    const canvas = ref.current;
    if (!canvas) return;
    const dpr = window.devicePixelRatio || 1;
    const targetWidth = Math.max(1, Math.round(width * dpr));
    const targetHeight = Math.max(1, Math.round(height * dpr));
    const textOverlay = textOverlayRef.current;
    const textOverlayContext = (): CanvasRenderingContext2D | null => {
      if (!textOverlay) return null;
      if (textOverlay.width !== targetWidth) textOverlay.width = targetWidth;
      if (textOverlay.height !== targetHeight) textOverlay.height = targetHeight;
      return textOverlay.getContext('2d');
    };

    const effectStart = performance.now();
    recordFigurePrimitiveProfile(primitives, rasterCandidate, webglMeshes.length);
    if (rasterWebGlActive) {
      const overlayContext = textOverlayContext();
      if (overlayContext && overlayPrimitives.length > 0) {
        measureFigureCanvas('paintCalls', 'paintMs', () =>
          paintFigure2D(overlayContext, overlayPrimitives, width, height, dpr, new Map(), panOffset)
        );
      }
      recordFigureCanvasMeasurement('effectRuns', 'effectMs', performance.now() - effectStart);
      return;
    }
    if (!workerSessionRef.current && !workerUnavailableRef.current) {
      workerSessionRef.current = createFigureCanvasWorkerSession(
        canvas,
        (elapsedMs, refreshElapsedMs) => {
          if (typeof refreshElapsedMs === 'number') {
            recordFigureCanvasMeasurement(
              'workerRefreshCalls',
              'workerRefreshMs',
              refreshElapsedMs
            );
          }
          recordFigureCanvasMeasurement('workerPaintCalls', 'workerPaintMs', elapsedMs);
        }
      );
      workerUnavailableRef.current = !workerSessionRef.current;
    }
    if (workerSessionRef.current) {
      let cancelled = false;
      const workerPaintsTextOverlay =
        textOverlayRenderer === 'worker' &&
        needsTextOverlay &&
        textOverlay !== null &&
        !workerTextOverlayUnavailableRef.current &&
        workerSessionRef.current.attachTextOverlay(textOverlay);
      if (needsTextOverlay && textOverlay && !workerPaintsTextOverlay) {
        workerTextOverlayUnavailableRef.current = true;
      }
      incrementFigureCanvasCounter('workerFramePosts');
      if (canvasTransferMode === 'bitmap') {
        void measureFigureCanvasAsync('workerPostCalls', 'workerPostMs', async () => {
          const overlayForWorker = workerPaintsTextOverlay ? overlayPrimitives : [];
          const frame = (await bitmapTransferFrame(basePrimitives, overlayForWorker)) ?? {
            primitives: basePrimitives,
            overlayPrimitives: overlayForWorker,
            transferList: [],
          };
          if (cancelled) {
            closeTransferFrame(frame);
            return;
          }
          workerSessionRef.current?.postFrame(
            frame.primitives,
            frame.overlayPrimitives,
            width,
            height,
            dpr,
            panOffset,
            frame.transferList
          );
        });
      } else {
        measureFigureCanvas('workerPostCalls', 'workerPostMs', () =>
          (() => {
            const overlayForWorker = workerPaintsTextOverlay ? overlayPrimitives : [];
            const frame =
              canvasTransferMode === 'clone'
                ? clonedTransferFrame(basePrimitives, overlayForWorker)
                : {
                    primitives: basePrimitives,
                    overlayPrimitives: overlayForWorker,
                    transferList: [],
                  };
            workerSessionRef.current?.postFrame(
              frame.primitives,
              frame.overlayPrimitives,
              width,
              height,
              dpr,
              panOffset,
              frame.transferList
            );
          })()
        );
      }
      const overlayContext = workerPaintsTextOverlay ? null : textOverlayContext();
      if (overlayContext && needsTextOverlay) {
        measureFigureCanvas('paintCalls', 'paintMs', () =>
          paintFigure2D(overlayContext, overlayPrimitives, width, height, dpr, new Map(), panOffset)
        );
      }
      recordFigureCanvasMeasurement('effectRuns', 'effectMs', performance.now() - effectStart);
      return () => {
        cancelled = true;
      };
    }

    if (canvas.width !== targetWidth) {
      canvas.width = targetWidth;
    }
    if (canvas.height !== targetHeight) {
      canvas.height = targetHeight;
    }
    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    let cancelled = false;
    const imageCache = imageCacheRef.current;
    const imagePrimitives = primitives.filter((p) => p.kind === 'image');
    const liveKeySet = new Set(figureImageCacheKeys(primitives));
    for (const key of imageCache.keys()) {
      if (!liveKeySet.has(key)) {
        closeCachedImageSource(imageCache.get(key));
        imageCache.delete(key);
      }
    }
    const rgbaImageBitmapJobs: Promise<{
      key: string | null;
      source: CachedImageSource | null;
      previous: CachedImageSource | undefined;
    }>[] = [];
    for (let index = 0; index < imagePrimitives.length; index++) {
      const primitive = imagePrimitives[index];
      const key = imageCacheKey(primitive, index);
      if (!key || (!primitive.rgba && !primitive.rgbaBytes)) continue;
      const cached = imageCache.get(key);
      if (!shouldRefreshRgbaImage(primitive, cached?.sourceId)) continue;
      if (primitive.rgbaBytes && canCreateRgbaImageBitmap()) {
        rgbaImageBitmapJobs.push(
          rgbaBitmap(primitive).then((source) => ({
            key,
            source,
            previous: cached,
          }))
        );
      } else {
        const imageSource = rgbaCanvas(primitive, cached);
        if (imageSource) {
          closeCachedImageSource(cached);
          imageCache.set(key, imageSource);
        }
      }
    }
    const urlByKey = new Map<string, string>();
    for (let index = 0; index < imagePrimitives.length; index++) {
      const primitive = imagePrimitives[index];
      if (primitive.rgba || !primitive.image) continue;
      const key = imageCacheKey(primitive, index);
      if (key && !urlByKey.has(key)) urlByKey.set(key, primitive.image);
    }
    const urls = [...urlByKey.entries()];
    const missingUrls = urls.filter(([key]) => key && !imageCache.has(key));
    // Draw immediately so the figure never blinks, then repaint once the
    // missing rasters (data URIs, so effectively instant) have decoded.
    measureFigureCanvas('paintCalls', 'paintMs', () =>
      paintFigure2D(ctx, basePrimitives, width, height, dpr, imageCache, panOffset)
    );
    const overlayContext = textOverlayContext();
    if (overlayContext && needsTextOverlay) {
      measureFigureCanvas('paintCalls', 'paintMs', () =>
        paintFigure2D(overlayContext, overlayPrimitives, width, height, dpr, imageCache, panOffset)
      );
    }
    if (missingUrls.length > 0) {
      Promise.all(
        missingUrls.map(
          ([key, url]) =>
            new Promise<[string | null, HTMLImageElement | null]>((resolve) => {
              const imageStart = performance.now();
              const img = new Image();
              img.onload = () => {
                recordFigureCanvasMeasurement(
                  'urlImageLoads',
                  'urlImageLoadMs',
                  performance.now() - imageStart
                );
                resolve([key, img]);
              };
              img.onerror = () => resolve([key, null]);
              img.src = url ?? '';
            })
        )
      ).then((pairs) => {
        if (cancelled) return;
        for (const [key, img] of pairs) {
          if (key && img && liveKeySet.has(key)) {
            imageCache.set(key, { source: img });
          }
        }
        measureFigureCanvas('paintCalls', 'paintMs', () =>
          paintFigure2D(ctx, basePrimitives, width, height, dpr, imageCache, panOffset)
        );
      });
    }
    if (rgbaImageBitmapJobs.length > 0) {
      Promise.all(rgbaImageBitmapJobs).then((pairs) => {
        if (cancelled) {
          for (const pair of pairs) closeCachedImageSource(pair.source ?? undefined);
          return;
        }
        for (const { key, source, previous } of pairs) {
          if (!key || !source || !liveKeySet.has(key)) {
            closeCachedImageSource(source ?? undefined);
            continue;
          }
          closeCachedImageSource(previous);
          imageCache.set(key, source);
        }
        measureFigureCanvas('paintCalls', 'paintMs', () =>
          paintFigure2D(ctx, basePrimitives, width, height, dpr, imageCache, panOffset)
        );
        const overlayContextAfterRgba = textOverlayContext();
        if (overlayContextAfterRgba && needsTextOverlay) {
          measureFigureCanvas('paintCalls', 'paintMs', () =>
            paintFigure2D(
              overlayContextAfterRgba,
              overlayPrimitives,
              width,
              height,
              dpr,
              imageCache,
              panOffset
            )
          );
        }
      });
    }
    recordFigureCanvasMeasurement('effectRuns', 'effectMs', performance.now() - effectStart);
    return () => {
      cancelled = true;
    };
  }, [
    primitives,
    basePrimitives,
    overlayPrimitives,
    webglMeshes.length,
    width,
    height,
    panOffset,
    needsTextOverlay,
    textOverlayRenderer,
    canvasTransferMode,
    rasterLayerCandidate,
    rasterWebGlActive,
  ]);

  return (
    <div style={{ position: 'relative', display: 'block', width, height }}>
      <canvas
        ref={ref}
        style={{ display: 'block', width, height, maxWidth: '100%', maxHeight: '100%' }}
      />
      {rasterLayerCandidate && (
        <FigureRasterWebGlCanvas
          primitives={primitives}
          width={width}
          height={height}
          panOffset={panOffset}
          onAvailable={setRasterWebGlAvailable}
        />
      )}
      {geometryLayerCandidate && (
        <FigureWebGlCanvas
          handle={handle}
          meshes={webglMeshes}
          width={width}
          height={height}
          softwareFallbackImage={rasterLayerCandidate ? undefined : softwareFallbackImage}
          onBackendChange={onWebGlBackendChange}
        />
      )}
      {needsTextOverlay && (
        <canvas
          ref={textOverlayRef}
          data-figure-layer="text-overlay"
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            display: 'block',
            width,
            height,
            pointerEvents: 'none',
          }}
        />
      )}
    </div>
  );
}
//=============================================================================
