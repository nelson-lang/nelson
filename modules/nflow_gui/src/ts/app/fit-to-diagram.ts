//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { state } from './state.js';
import {
  setViewBox,
  setZoomScale,
  initViewBox,
  clampZoomScale,
  getViewportSize,
} from './workspace-handlers.js';
//=============================================================================
// Zooms and centres the view on the whole model.
export function fitToDiagram(): void {
  if (state.blocks.size === 0) {
    initViewBox();
    return;
  }
  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;
  const include = (x: number, y: number): void => {
    // A block with an unresolved template has a non-finite width/height. One NaN would poison
    // every bound and give a NaN viewBox, which the SVG ignores. Skip such points.
    if (!Number.isFinite(x) || !Number.isFinite(y)) return;
    if (x < minX) minX = x;
    if (y < minY) minY = y;
    if (x > maxX) maxX = x;
    if (y > maxY) maxY = y;
  };
  state.blocks.forEach((block: any) => {
    const x = Number(block.x);
    const y = Number(block.y);
    const w = Number(block.width);
    const h = Number(block.height);
    include(x, y);
    include(x + (Number.isFinite(w) ? w : 0), y + (Number.isFinite(h) ? h : 0));
  });
  // Wires can route well outside the block cluster (feedback loops, long detours).
  // Include their routing points so the reset frames the whole model.
  const connections = state.connections as any[];
  if (Array.isArray(connections)) {
    connections.forEach((conn: any) => {
      const points = conn?.points;
      if (!Array.isArray(points)) return;
      points.forEach((p: any) => {
        if (Array.isArray(p)) include(Number(p[0]), Number(p[1]));
        else if (p) include(Number(p.x), Number(p.y));
      });
    });
  }
  // No finite geometry at all (every block unresolved): fall back rather than
  // writing a NaN viewBox that would freeze the view.
  if (!Number.isFinite(minX) || !Number.isFinite(maxX)) {
    initViewBox();
    return;
  }
  const pad = 60;
  minX -= pad;
  minY -= pad;
  maxX += pad;
  maxY += pad;
  const boundsW = Math.max(1, maxX - minX);
  const boundsH = Math.max(1, maxY - minY);
  const { w, h } = getViewportSize();
  // Cap zoom-in at 3x so a tiny model is not blown up. There is no lower floor, so a
  // large model still fits the viewport.
  const scale = clampZoomScale(Math.min(w / boundsW, h / boundsH));
  setZoomScale(scale);
  const center = { x: minX + boundsW / 2, y: minY + boundsH / 2 };
  const newW = w / scale;
  const newH = h / scale;
  setViewBox({ x: center.x - newW / 2, y: center.y - newH / 2, w: newW, h: newH });
}
//=============================================================================
