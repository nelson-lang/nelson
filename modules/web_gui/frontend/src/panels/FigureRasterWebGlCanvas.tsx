//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef } from 'react';
import type { DrawPrimitive } from '../rpc/types';
import {
  createFigureRasterWebGlRenderer,
  type FigureRasterWebGlRenderer,
} from './figureRasterWebGlRenderer';
import { measureFigureCanvas } from './figureCanvasProfile';
//=============================================================================
export function FigureRasterWebGlCanvas({
  primitives,
  width,
  height,
  panOffset = null,
  onAvailable,
}: {
  primitives: DrawPrimitive[];
  width: number;
  height: number;
  panOffset?: { dx: number; dy: number } | null;
  onAvailable?: (available: boolean) => void;
}): JSX.Element {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const rendererRef = useRef<FigureRasterWebGlRenderer | null>(null);
  const failedRef = useRef(false);
  const onAvailableRef = useRef(onAvailable);
  onAvailableRef.current = onAvailable;

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;
    const renderer = createFigureRasterWebGlRenderer(canvas);
    rendererRef.current = renderer;
    failedRef.current = renderer === null;
    onAvailableRef.current?.(renderer !== null);
    return () => {
      rendererRef.current?.destroy();
      rendererRef.current = null;
      onAvailableRef.current?.(false);
    };
  }, []);

  useEffect(() => {
    const renderer = rendererRef.current;
    if (!renderer || failedRef.current) return;
    try {
      measureFigureCanvas('rasterWebGlCalls', 'rasterWebGlMs', () =>
        renderer.render(primitives, width, height, window.devicePixelRatio || 1, panOffset)
      );
    } catch {
      failedRef.current = true;
      renderer.destroy();
      rendererRef.current = null;
      onAvailableRef.current?.(false);
    }
  }, [primitives, width, height, panOffset]);

  return (
    <canvas
      ref={canvasRef}
      data-figure-layer="raster-webgl"
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
  );
}
//=============================================================================
