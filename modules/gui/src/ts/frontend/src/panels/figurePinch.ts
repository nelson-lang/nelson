//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { RotatableAxes } from '../rpc/types';
import { hitFigureAxes } from './figureAxes';
//=============================================================================
interface Options {
  stage: HTMLElement;
  axes: () => RotatableAxes[] | undefined;
  selectAxes: (handle: number) => void;
  action: (request: Record<string, unknown>) => Promise<void>;
}
//=============================================================================
interface Point {
  x: number;
  y: number;
}
//=============================================================================
const distance = (a: Point, b: Point) => Math.hypot(a.x - b.x, a.y - b.y);
const midpoint = (a: Point, b: Point): Point => ({ x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 });
// Two-finger pinch zoom for touch (phones, tablets, the WebAssembly build). The
// zoom is anchored at the pinch midpoint so that point stays under the fingers,
// reusing the same focal-point `zoom` request as the mouse wheel.
export function createFigurePinch(options: Options) {
  const pointers = new Map<number, Point>();
  let active = false,
    lastDist = 0,
    axesHandle = 0,
    axRect: [number, number, number, number] = [0, 0, 1, 1],
    rect: DOMRect | null = null,
    pendingFactor = 1,
    focusXFrac = 0.5,
    focusYFrac = 0.5,
    frame: number | null = null,
    inFlight = false,
    disposed = false;

  const begin = () => {
    const pts = [...pointers.values()];
    rect = options.stage.getBoundingClientRect();
    lastDist = distance(pts[0], pts[1]);
    const mid = midpoint(pts[0], pts[1]);
    const axis = hitFigureAxes(options.axes(), rect, mid.x, mid.y);
    axesHandle = axis?.handle ?? 0;
    axRect = axis?.rect ?? [0, 0, 1, 1];
    if (axesHandle) options.selectAxes(axesHandle);
    active = true;
    pendingFactor = 1;
  };
  const send = () => {
    const factor = pendingFactor;
    pendingFactor = 1;
    if (Math.abs(factor - 1) < 1e-4) return;
    inFlight = true;
    void options
      .action({ action: 'zoom', axesHandle, factor, focusXFrac, focusYFrac, compactView: true })
      .catch(() => undefined)
      .finally(() => {
        inFlight = false;
        if (!disposed && Math.abs(pendingFactor - 1) > 1e-4) schedule();
      });
  };
  const schedule = () => {
    if (disposed || inFlight || frame !== null) return;
    frame = requestAnimationFrame(() => {
      frame = null;
      send();
    });
  };
  const down = (event: PointerEvent) => {
    if (disposed || event.pointerType === 'mouse') return;
    pointers.set(event.pointerId, { x: event.clientX, y: event.clientY });
    if (pointers.size === 2) begin();
  };
  const move = (event: PointerEvent) => {
    if (!pointers.has(event.pointerId)) return;
    pointers.set(event.pointerId, { x: event.clientX, y: event.clientY });
    if (!active || pointers.size < 2 || !rect) return;
    event.preventDefault();
    const pts = [...pointers.values()];
    const dist = distance(pts[0], pts[1]);
    const mid = midpoint(pts[0], pts[1]);
    if (lastDist > 0 && dist > 0) pendingFactor *= dist / lastDist;
    lastDist = dist;
    const fx = rect.width > 0 ? (mid.x - rect.left) / rect.width : 0.5;
    const fy = rect.height > 0 ? (mid.y - rect.top) / rect.height : 0.5;
    focusXFrac = axRect[2] > 0 ? (fx - axRect[0]) / axRect[2] : 0.5;
    focusYFrac = axRect[3] > 0 ? (fy - axRect[1]) / axRect[3] : 0.5;
    schedule();
  };
  const up = (event: PointerEvent) => {
    pointers.delete(event.pointerId);
    if (pointers.size < 2) {
      active = false;
      lastDist = 0;
    }
  };
  return {
    down,
    move,
    up,
    dispose: () => {
      disposed = true;
      pointers.clear();
      if (frame !== null) cancelAnimationFrame(frame);
    },
  };
}
//=============================================================================
