//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DrawPrimitive } from '../rpc/types';
import {
  closeCachedImageSource,
  figureImageCacheKeys,
  imageCacheKey,
  paintFigure2D,
  rgbaBytes,
  rgbaImageData,
  shouldRefreshRgbaImage,
  type CachedImageSource,
} from './figureCanvasPainter';
//=============================================================================
type InitMessage = {
  type: 'init';
  canvas: OffscreenCanvas;
};
//=============================================================================
type InitOverlayMessage = {
  type: 'initOverlay';
  canvas: OffscreenCanvas;
};
//=============================================================================
type FrameMessage = {
  type: 'frame';
  primitives: DrawPrimitive[];
  overlayPrimitives: DrawPrimitive[];
  width: number;
  height: number;
  dpr: number;
  panOffset: { dx: number; dy: number } | null;
};
//=============================================================================
type WorkerMessage = InitMessage | InitOverlayMessage | FrameMessage;
let canvas: OffscreenCanvas | null = null;
let context: OffscreenCanvasRenderingContext2D | null = null;
let overlayCanvas: OffscreenCanvas | null = null;
let overlayContext: OffscreenCanvasRenderingContext2D | null = null;
let pendingFrame: FrameMessage | null = null;
let flushHandle: ReturnType<typeof setTimeout> | number | null = null;
let flushing = false;
const imageCache = new Map<string, CachedImageSource>();
function closeImageCache(): void {
  for (const cached of imageCache.values()) closeCachedImageSource(cached);
  imageCache.clear();
}
//=============================================================================
function rgbaCanvasSource(
  primitive: Extract<DrawPrimitive, { kind: 'image' }>,
  cached?: CachedImageSource
): CachedImageSource | null {
  if (!primitive.imageWidth || !primitive.imageHeight) return null;
  const bytes = rgbaBytes(primitive);
  const expected = primitive.imageWidth * primitive.imageHeight * 4;
  if (!bytes || bytes.byteLength !== expected) return null;
  const target =
    cached?.source instanceof OffscreenCanvas &&
    cached.source.width === primitive.imageWidth &&
    cached.source.height === primitive.imageHeight
      ? cached.source
      : new OffscreenCanvas(primitive.imageWidth, primitive.imageHeight);
  const ctx = target.getContext('2d');
  if (!ctx) return null;
  const imageData =
    cached?.rgbaImageData?.width === primitive.imageWidth &&
    cached.rgbaImageData.height === primitive.imageHeight
      ? cached.rgbaImageData
      : ctx.createImageData(primitive.imageWidth, primitive.imageHeight);
  imageData.data.set(bytes);
  ctx.putImageData(imageData, 0, 0);
  return { source: target, sourceId: primitive.imageId, rgbaImageData: imageData };
}
//=============================================================================
async function rgbaBitmapSource(
  primitive: Extract<DrawPrimitive, { kind: 'image' }>,
  cached?: CachedImageSource
): Promise<CachedImageSource | null> {
  const imageData = rgbaImageData(primitive);
  if (!imageData) return null;
  if (typeof createImageBitmap !== 'function') return rgbaCanvasSource(primitive, cached);
  const bitmap = await createImageBitmap(imageData);
  return { source: bitmap, sourceId: primitive.imageId };
}
//=============================================================================
async function urlImageSource(url: string): Promise<CachedImageSource | null> {
  if (typeof fetch !== 'function' || typeof createImageBitmap !== 'function') return null;
  const response = await fetch(url);
  if (!response.ok) return null;
  return { source: await createImageBitmap(await response.blob()) };
}
//=============================================================================
async function refreshImageCache(
  primitives: DrawPrimitive[],
  shouldAbort: () => boolean
): Promise<boolean> {
  if (shouldAbort()) return false;
  const imagePrimitives = primitives.filter((p) => p.kind === 'image');
  const liveKeySet = new Set(figureImageCacheKeys(primitives));
  for (const key of imageCache.keys()) {
    if (shouldAbort()) return false;
    if (!liveKeySet.has(key)) {
      closeCachedImageSource(imageCache.get(key));
      imageCache.delete(key);
    }
  }
  for (let index = 0; index < imagePrimitives.length; index++) {
    if (shouldAbort()) return false;
    const primitive = imagePrimitives[index];
    const key = imageCacheKey(primitive, index);
    if (!key) continue;
    const cached = imageCache.get(key);
    let source: CachedImageSource | null = null;
    if (primitive.bitmap) {
      source = { source: primitive.bitmap, sourceId: primitive.imageId };
    } else if (primitive.rgba || primitive.rgbaBytes) {
      if (!shouldRefreshRgbaImage(primitive, cached?.sourceId)) continue;
      source = await rgbaBitmapSource(primitive, cached);
    } else if (primitive.image && !cached) {
      source = await urlImageSource(primitive.image);
    }
    if (shouldAbort()) {
      closeCachedImageSource(source ?? undefined);
      return false;
    }
    if (!source) continue;
    closeCachedImageSource(cached);
    imageCache.set(key, source);
  }
  return true;
}
//=============================================================================
function scheduleFlush(): void {
  if (flushHandle !== null) return;
  const workerScope = self as typeof self & {
    requestAnimationFrame?: (callback: FrameRequestCallback) => number;
  };
  if (typeof workerScope.requestAnimationFrame === 'function') {
    flushHandle = workerScope.requestAnimationFrame(() => void flushPendingFrame());
    return;
  }
  flushHandle = setTimeout(() => void flushPendingFrame(), 0);
}
//=============================================================================
async function flushPendingFrame(): Promise<void> {
  flushHandle = null;
  if (flushing) return;
  flushing = true;
  try {
    while (pendingFrame && context && canvas) {
      const frame = pendingFrame;
      pendingFrame = null;
      const targetWidth = Math.max(1, Math.round(frame.width * frame.dpr));
      const targetHeight = Math.max(1, Math.round(frame.height * frame.dpr));
      if (canvas.width !== targetWidth) canvas.width = targetWidth;
      if (canvas.height !== targetHeight) canvas.height = targetHeight;
      if (overlayCanvas) {
        if (overlayCanvas.width !== targetWidth) overlayCanvas.width = targetWidth;
        if (overlayCanvas.height !== targetHeight) overlayCanvas.height = targetHeight;
      }
      const refreshStarted = performance.now();
      const refreshed = await refreshImageCache(frame.primitives, () => pendingFrame !== null);
      const refreshElapsedMs = performance.now() - refreshStarted;
      if (!refreshed || pendingFrame) continue;
      const started = performance.now();
      paintFigure2D(
        context,
        frame.primitives,
        frame.width,
        frame.height,
        frame.dpr,
        imageCache,
        frame.panOffset
      );
      if (overlayContext) {
        paintFigure2D(
          overlayContext,
          frame.overlayPrimitives,
          frame.width,
          frame.height,
          frame.dpr,
          imageCache,
          frame.panOffset
        );
      }
      self.postMessage({
        type: 'painted',
        elapsedMs: performance.now() - started,
        refreshElapsedMs,
      });
    }
  } finally {
    flushing = false;
    if (pendingFrame) scheduleFlush();
  }
}
//=============================================================================
self.addEventListener('message', (event: MessageEvent<WorkerMessage>) => {
  const message = event.data;
  if (message.type === 'init') {
    closeImageCache();
    canvas = message.canvas;
    context = canvas.getContext('2d');
    return;
  }
  if (message.type === 'initOverlay') {
    overlayCanvas = message.canvas;
    overlayContext = overlayCanvas.getContext('2d');
    return;
  }
  if (message.type === 'frame') {
    pendingFrame = message;
    scheduleFlush();
  }
});
//=============================================================================
