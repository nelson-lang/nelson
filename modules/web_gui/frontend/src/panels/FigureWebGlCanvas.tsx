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
import type { FigureWebGlMesh } from '../rpc/types';
import {
  createFigureWebGlRenderer,
  resolveRequestedBackend,
  type FigureWebGlBackend,
  type FigureWebGlRenderer,
} from './figureWebGlRenderer';
import {
  hasCompleteFigureWebGlGeometry,
  queueLatestWebGlAnimationFrame,
  retainFigureWebGlGeometry,
  takePendingFigureWebGlAnimationFrame,
  type FigureWebGlGeometry,
} from './figureWebGlGeometry';
import { recordFigureCanvasMeasurement } from './figureCanvasProfile';
//=============================================================================
export function shouldRetryWebGlPresentation(
  userAgent = window.navigator.userAgent,
  vendor = window.navigator.vendor
): boolean {
  const ua = userAgent.toLowerCase();
  const isFirefox = ua.includes('firefox') || ua.includes('fxios');
  if (isFirefox) return true;
  const isChromium =
    ua.includes('chrome') || ua.includes('chromium') || ua.includes('crios') || ua.includes('edg/');
  const isSafari = ua.includes('safari') && !isChromium;
  return isSafari || vendor.toLowerCase().includes('apple computer');
}
//=============================================================================
export function FigureWebGlCanvas({
  handle,
  meshes,
  width,
  height,
  softwareFallbackImage,
  onBackendChange,
}: {
  handle: number;
  meshes: FigureWebGlMesh[];
  width: number;
  height: number;
  softwareFallbackImage?: string;
  onBackendChange?: (backend: FigureWebGlBackend | 'software') => void;
}): JSX.Element {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const rendererRef = useRef<FigureWebGlRenderer | null>(null);
  const geometryRef = useRef(new Map<number, FigureWebGlGeometry>());
  const animationQueueRef = useRef<FigureWebGlMesh[][]>([]);
  const animationFrameRef = useRef<number | null>(null);
  const presentationFrameRef = useRef<number | null>(null);
  const renderSerialRef = useRef(0);
  const webGlFailedRef = useRef(false);
  const hasRenderedFrameRef = useRef(false);
  const [backend, setBackend] = useState<FigureWebGlBackend | 'pending'>('pending');
  const [hasRenderedFrame, setHasRenderedFrame] = useState(false);
  // The effective backend can be downgraded at runtime (WebGPU device loss or
  // a failed WebGPU frame falls back to WebGL2). Bumping the generation remounts
  // a fresh <canvas> so the create effect re-runs with the downgraded backend;
  // a canvas element cannot swap its context type once acquired.
  const [canvasGeneration, setCanvasGeneration] = useState(0);
  const effectiveBackendRef = useRef<FigureWebGlBackend | 'auto'>(resolveRequestedBackend());
  // Keep the callback in a ref so the renderer effect never re-runs (and never
  // tears down the WebGL context) because a parent passed a fresh closure.
  const onBackendChangeRef = useRef(onBackendChange);
  onBackendChangeRef.current = onBackendChange;
  const retainedMeshes = useMemo(
    () => retainFigureWebGlGeometry(meshes, geometryRef.current),
    [meshes]
  );

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;
    let cancelled = false;
    createFigureWebGlRenderer(canvas, effectiveBackendRef.current)
      .then((renderer) => {
        if (cancelled) {
          renderer?.destroy();
          return;
        }
        rendererRef.current = renderer;
        webGlFailedRef.current = renderer === null;
        hasRenderedFrameRef.current = false;
        setHasRenderedFrame(false);
        const nextBackend = renderer?.backend ?? 'software';
        setBackend(nextBackend);
        onBackendChangeRef.current?.(nextBackend);
      })
      .catch(() => {
        if (!cancelled) {
          webGlFailedRef.current = true;
          hasRenderedFrameRef.current = false;
          setHasRenderedFrame(false);
          setBackend('software');
          onBackendChangeRef.current?.('software');
        }
      });
    return () => {
      cancelled = true;
      rendererRef.current?.destroy();
      rendererRef.current = null;
      hasRenderedFrameRef.current = false;
      if (presentationFrameRef.current !== null) {
        window.cancelAnimationFrame(presentationFrameRef.current);
        presentationFrameRef.current = null;
      }
    };
  }, [canvasGeneration]);

  const failToSoftware = (): void => {
    webGlFailedRef.current = true;
    rendererRef.current?.destroy();
    rendererRef.current = null;
    hasRenderedFrameRef.current = false;
    setHasRenderedFrame(false);
    setBackend('software');
    onBackendChangeRef.current?.('software');
  };

  // WebGPU is opt-in and experimental: a lost device or a failed WebGPU frame
  // steps down to WebGL2 (via a canvas remount) rather than straight to the CPU
  // fallback. Any other backend failure goes to software.
  const failToNextBackend = (): void => {
    if (rendererRef.current?.backend === 'webgpu') {
      rendererRef.current.destroy();
      rendererRef.current = null;
      hasRenderedFrameRef.current = false;
      setHasRenderedFrame(false);
      webGlFailedRef.current = false;
      effectiveBackendRef.current = 'webgl2';
      setBackend('pending');
      setCanvasGeneration((generation) => generation + 1);
      return;
    }
    failToSoftware();
  };

  const schedulePresentationRetry = (
    frame: FigureWebGlMesh[],
    frameWidth: number,
    frameHeight: number,
    serial: number
  ): void => {
    if (presentationFrameRef.current !== null) {
      window.cancelAnimationFrame(presentationFrameRef.current);
    }
    presentationFrameRef.current = window.requestAnimationFrame(() => {
      presentationFrameRef.current = null;
      if (renderSerialRef.current !== serial) return;
      const renderer = rendererRef.current;
      if (!renderer || webGlFailedRef.current) return;
      try {
        const started = performance.now();
        renderer.render(frame, frameWidth, frameHeight, window.devicePixelRatio || 1, true);
        recordFigureCanvasMeasurement(
          'webGlRenderCalls',
          'webGlRenderMs',
          performance.now() - started
        );
      } catch {
        failToNextBackend();
      }
    });
  };

  const renderWebGlFrame = (
    frame: FigureWebGlMesh[],
    frameWidth: number,
    frameHeight: number
  ): 'rendered' | 'pending' | 'skipped' | 'failed' => {
    if (!hasCompleteFigureWebGlGeometry(frame)) return 'skipped';
    const renderer = rendererRef.current;
    if (!renderer) return webGlFailedRef.current ? 'failed' : 'pending';
    try {
      const started = performance.now();
      const firstFrame = !hasRenderedFrameRef.current;
      const serial = ++renderSerialRef.current;
      renderer.render(frame, frameWidth, frameHeight, window.devicePixelRatio || 1, firstFrame);
      recordFigureCanvasMeasurement(
        'webGlRenderCalls',
        'webGlRenderMs',
        performance.now() - started
      );
      if (!hasRenderedFrameRef.current) {
        hasRenderedFrameRef.current = true;
        setHasRenderedFrame(true);
        if (shouldRetryWebGlPresentation()) {
          schedulePresentationRetry(frame, frameWidth, frameHeight, serial);
        }
      }
      return 'rendered';
    } catch {
      failToNextBackend();
      return 'failed';
    }
  };

  useEffect(() => {
    const eventName = `nelson:figure-webgl-frame:${handle}`;
    const renderNext = () => {
      animationFrameRef.current = null;
      const next = animationQueueRef.current.shift();
      if (next) {
        if (renderWebGlFrame(next, width, height) === 'pending') {
          // Renderer not created yet: hold the frame, the backend effect
          // renders the retained meshes when the renderer resolves. Do not
          // reschedule here, that would spin a rAF loop until resolution.
          animationQueueRef.current = queueLatestWebGlAnimationFrame(
            animationQueueRef.current,
            next
          );
          return;
        }
      }
      if (animationQueueRef.current.length > 0) {
        animationFrameRef.current = window.requestAnimationFrame(renderNext);
      }
    };
    const receive = (event: Event) => {
      const meshes = (event as CustomEvent<FigureWebGlMesh[]>).detail;
      const retained = retainFigureWebGlGeometry(meshes, geometryRef.current);
      if (!hasCompleteFigureWebGlGeometry(retained)) return;
      animationQueueRef.current = queueLatestWebGlAnimationFrame(
        animationQueueRef.current,
        retained
      );
      if (animationFrameRef.current !== null) return;
      animationFrameRef.current = window.requestAnimationFrame(renderNext);
    };
    window.addEventListener(eventName, receive);
    const pending = takePendingFigureWebGlAnimationFrame(handle);
    if (pending) {
      receive(new CustomEvent(eventName, { detail: pending }));
    }
    return () => {
      window.removeEventListener(eventName, receive);
      if (animationFrameRef.current !== null) {
        window.cancelAnimationFrame(animationFrameRef.current);
        animationFrameRef.current = null;
      }
      animationQueueRef.current = [];
    };
  }, [handle, height, width]);

  useEffect(() => {
    if (backend === 'pending' || backend === 'software') return;
    const frame = window.requestAnimationFrame(() => {
      // A frame parked while the renderer was still resolving supersedes the
      // props: it is the most recent animation state for this figure.
      const parked = animationQueueRef.current.shift();
      renderWebGlFrame(parked ?? retainedMeshes, width, height);
    });
    return () => window.cancelAnimationFrame(frame);
  }, [backend, height, retainedMeshes, width]);

  const layerStyle = {
    position: 'absolute' as const,
    left: 0,
    top: 0,
    width,
    height,
    pointerEvents: 'none' as const,
  };
  return (
    <>
      <canvas
        key={canvasGeneration}
        ref={canvasRef}
        data-figure-renderer={backend}
        style={{ ...layerStyle, display: backend === 'software' ? 'none' : 'block' }}
      />
      {(backend === 'software' || !hasRenderedFrame) && softwareFallbackImage && (
        <img
          src={softwareFallbackImage}
          alt=""
          data-figure-renderer="software"
          style={layerStyle}
        />
      )}
    </>
  );
}
//=============================================================================
