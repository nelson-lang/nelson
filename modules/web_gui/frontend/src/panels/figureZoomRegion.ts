//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { MouseEvent as ReactMouseEvent } from 'react';
import type { FigureDisplayListResult } from '../rpc/types';
import { pickRotatableAxes } from './figureAxes';
//=============================================================================
interface ZoomOptions {
  dl: FigureDisplayListResult;
  axesHandle: number;
  selectAxes: (handle: number) => void;
  action: (request: Record<string, unknown>) => Promise<void>;
  cancelDrag: { current: (() => void) | null };
}
//=============================================================================
// Below this drag size (px) a zoom gesture is a click: zoom in at the point
// (Shift/Alt zooms out) instead of zooming into a rubber-band box.
const MIN_BOX = 6;
export function startFigureZoomRegion(event: ReactMouseEvent, options: ZoomOptions) {
  if (event.button !== 0) return;
  event.preventDefault();
  options.cancelDrag.current?.();
  const overlay = event.currentTarget as HTMLElement;
  const rect0 = overlay.getBoundingClientRect();
  if (rect0.width <= 0 || rect0.height <= 0) return;
  const picked = pickRotatableAxes(options.dl.rotatableAxes, rect0, event.clientX, event.clientY);
  const axesHandle = picked?.handle ?? options.axesHandle;
  const [axLeft, axTop, axWidth, axHeight] = picked?.rect ?? [0, 0, 1, 1];
  if (axesHandle) options.selectAxes(axesHandle);
  const zoomOut = event.shiftKey || event.altKey;
  const region = !options.dl.is3d;

  // Everything is kept in fractions of the overlay (0..1), and the overlay rect
  // is read fresh on every frame. The stage can carry a CSS scale transform (the
  // panel preview before the engine repaints at the new size) and it can even
  // change mid-drag; fractions are invariant to any of that, where absolute
  // pixels captured once would desync and the band would stop tracking.
  const frac = (clientX: number, clientY: number) => {
    const r = overlay.getBoundingClientRect();
    return {
      fx: r.width > 0 ? (clientX - r.left) / r.width : 0,
      fy: r.height > 0 ? (clientY - r.top) / r.height : 0,
      w: r.width,
      h: r.height,
    };
  };
  const origin = frac(event.clientX, event.clientY);
  let cur = origin;
  const band = document.createElement('div');
  band.className = 'figure-panel__zoomband';
  overlay.appendChild(band);
  const clampX = (f: number) => Math.min(Math.max(f, axLeft), axLeft + axWidth);
  const clampY = (f: number) => Math.min(Math.max(f, axTop), axTop + axHeight);
  const draw = () => {
    // The band tracks the drag as a rectangle in both 2D and 3D; only the action
    // on release differs (2D zooms into the data region, 3D dollies the camera).
    const x0 = clampX(Math.min(origin.fx, cur.fx));
    const x1 = clampX(Math.max(origin.fx, cur.fx));
    const y0 = clampY(Math.min(origin.fy, cur.fy));
    const y1 = clampY(Math.max(origin.fy, cur.fy));
    band.style.left = `${x0 * 100}%`;
    band.style.width = `${Math.max(0, x1 - x0) * 100}%`;
    band.style.top = `${y0 * 100}%`;
    band.style.height = `${Math.max(0, y1 - y0) * 100}%`;
  };
  draw();
  const move = (next: MouseEvent) => {
    cur = frac(next.clientX, next.clientY);
    draw();
  };
  const detach = () => {
    window.removeEventListener('mousemove', move);
    window.removeEventListener('mouseup', up);
    window.removeEventListener('blur', blur);
    band.remove();
    if (options.cancelDrag.current === detach) options.cancelDrag.current = null;
  };
  // Fraction within the axes rectangle (0..1), from an overlay fraction.
  const axX = (f: number) => (axWidth > 0 ? (clampX(f) - axLeft) / axWidth : 0);
  const axY = (f: number) => (axHeight > 0 ? (clampY(f) - axTop) / axHeight : 0);
  const up = (next: MouseEvent) => {
    cur = frac(next.clientX, next.clientY);
    detach();
    if (axWidth <= 0 || axHeight <= 0) return;
    const dxPx = Math.abs(cur.fx - origin.fx) * cur.w;
    const dyPx = Math.abs(cur.fy - origin.fy) * cur.h;
    if (dxPx < MIN_BOX || dyPx < MIN_BOX) {
      void options.action({
        action: 'zoom',
        axesHandle,
        factor: zoomOut ? 0.5 : 2,
        focusXFrac: axX(cur.fx),
        focusYFrac: axY(cur.fy),
      });
      return;
    }
    if (region) {
      // 2D: zoom into the boxed data region.
      void options.action({
        action: 'zoom',
        axesHandle,
        regionXFrac: [axX(Math.min(origin.fx, cur.fx)), axX(Math.max(origin.fx, cur.fx))],
        regionYFrac: [axY(Math.min(origin.fy, cur.fy)), axY(Math.max(origin.fy, cur.fy))],
      });
      return;
    }
    // 3D: dolly the camera by how much the box shrinks the view (a smaller box
    // zooms in more); the engine zooms the camera, so no focal point applies.
    const boxW = Math.abs(axX(cur.fx) - axX(origin.fx));
    const boxH = Math.abs(axY(cur.fy) - axY(origin.fy));
    const factor = Math.max(1, Math.min(10, 1 / Math.max(boxW, boxH, 0.1)));
    void options.action({ action: 'zoom', axesHandle, factor: zoomOut ? 1 / factor : factor });
  };
  const blur = () => detach();
  options.cancelDrag.current = detach;
  window.addEventListener('mousemove', move);
  window.addEventListener('mouseup', up);
  window.addEventListener('blur', blur);
}
//=============================================================================
