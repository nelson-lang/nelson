//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { alignToGrid } from '../geometry.js';
import { collectSharedSourceSplitPoints } from './wire-paths.js';
import { createSvgElement } from '../svg-elements.js';
import { collectPhysicalNetJunctionPoints } from '../../router/physical-net.js';
import type { RendererCtx, Conn } from '../core/renderer-types.js';
//=============================================================================
const WIRE_CORNER_HANDLE_RADIUS = 6;
export function makeWireCornerHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as {
    segmentOrientation?: (
      a: { x: number; y: number },
      b: { x: number; y: number }
    ) => 'H' | 'V' | null;
    dedupePoints: (p: { x: number; y: number }[]) => { x: number; y: number }[];
    removeColinearPoints?: (p: { x: number; y: number }[]) => { x: number; y: number }[];
    getTurnIndices?: (p: { x: number; y: number }[]) => number[];
    wireCornerDrag?: {
      active?: boolean;
      conn?: Conn | null;
      index?: number;
      pointerId?: number;
    } | null;
    svg?: SVGSVGElement | null;
  };
  function clearWireCornerHandles() {
    while (ctx.wireCornerLayer && ctx.wireCornerLayer.firstChild)
      ctx.wireCornerLayer.removeChild(ctx.wireCornerLayer.firstChild);
  }
  function clearWireJunctionDots() {
    while (ctx.wireJunctionLayer && ctx.wireJunctionLayer.firstChild)
      ctx.wireJunctionLayer.removeChild(ctx.wireJunctionLayer.firstChild);
  }
  function renderSharedSourceJunctionDots() {
    clearWireJunctionDots();
    const points = collectSharedSourceSplitPoints(ctx.state.connections, (c: unknown) => {
      const cc = c as { debugRenderPoints?: unknown; points?: unknown };
      return Array.isArray(cc.debugRenderPoints) && (cc.debugRenderPoints as unknown[]).length >= 2
        ? cc.debugRenderPoints
        : cc.points;
    });
    points.forEach((pt) => {
      if (ctx.wireJunctionLayer)
        ctx.wireJunctionLayer.appendChild(
          createSvgElement('circle', { cx: pt.x, cy: pt.y, r: 3, class: 'wire-junction-dot' })
        );
    });
    // Physical (acausal) nets: a dot where three or more branches meet.
    collectPhysicalNetJunctionPoints(ctx.state.connections, ctx.state.blocks).forEach((pt) => {
      if (ctx.wireJunctionLayer)
        ctx.wireJunctionLayer.appendChild(
          createSvgElement('circle', { cx: pt.x, cy: pt.y, r: 3, class: 'wire-junction-dot' })
        );
    });
    // Pinned waypoints get anchor dots so pinned wires are identifiable.
    for (const conn of ctx.state.connections as Array<{ points?: unknown }>) {
      const pts = conn.points;
      if (!Array.isArray(pts)) continue;
      for (const pt of pts as Array<{ x: number; y: number; pin?: boolean }>) {
        if (!pt?.pin) continue;
        if (ctx.wireJunctionLayer)
          ctx.wireJunctionLayer.appendChild(
            createSvgElement('circle', { cx: pt.x, cy: pt.y, r: 3.5, class: 'wire-pin-dot' })
          );
      }
    }
  }
  function moveWireCorner(conn: Conn | null, index: number, target: { x: number; y: number }) {
    if (!conn || !Array.isArray(conn.points) || index <= 0 || index >= conn.points.length - 1)
      return false;
    const points = conn.points.map((pt) => alignToGrid(pt));
    const oriIn = C.segmentOrientation
      ? C.segmentOrientation(points[index - 1], points[index])
      : null;
    const oriOut = C.segmentOrientation
      ? C.segmentOrientation(points[index], points[index + 1])
      : null;
    if (!oriIn || !oriOut || oriIn === oriOut) return false;
    let inStart = index;
    while (
      inStart > 0 &&
      C.segmentOrientation &&
      C.segmentOrientation(points[inStart - 1], points[inStart]) === oriIn
    )
      inStart -= 1;
    let outEnd = index;
    while (
      outEnd < points.length - 1 &&
      C.segmentOrientation &&
      C.segmentOrientation(points[outEnd], points[outEnd + 1]) === oriOut
    )
      outEnd += 1;
    const { x: tx, y: ty } = alignToGrid(target);
    if (oriIn === 'H' && inStart > 0) {
      for (let i = inStart; i <= index; i += 1) points[i].y = ty;
    } else if (oriIn === 'V' && inStart > 0) {
      for (let i = inStart; i <= index; i += 1) points[i].x = tx;
    }
    if (oriOut === 'H' && outEnd < points.length - 1) {
      for (let i = index; i <= outEnd; i += 1) points[i].y = ty;
    } else if (oriOut === 'V' && outEnd < points.length - 1) {
      for (let i = index; i <= outEnd; i += 1) points[i].x = tx;
    }
    const next = C.removeColinearPoints
      ? C.removeColinearPoints(C.dedupePoints(points))
      : C.dedupePoints(points);
    if (!next || next.length < 2) return false;
    conn.points = next;
    return true;
  }
  function updateWireCornerHandles() {
    clearWireCornerHandles();
    if (ctx.state.autoRoute !== false) return;
    const conn = (ctx as unknown as { state: { selectedConnection?: unknown } }).state
      .selectedConnection as Conn | undefined;
    if (!conn || !Array.isArray(conn.points) || conn.points.length < 3) return;
    const points = conn.points as { x: number; y: number }[];
    const turnIndices = C.getTurnIndices ? C.getTurnIndices(points) : [];
    turnIndices.forEach((index) => {
      const pt = points[index];
      const handle = createSvgElement('circle', {
        class: 'wire-corner-handle',
        cx: pt.x,
        cy: pt.y,
        r: WIRE_CORNER_HANDLE_RADIUS,
      });
      handle.addEventListener('pointerdown', (event: PointerEvent) => {
        event.preventDefault();
        event.stopPropagation();
        if (!C.wireCornerDrag) return;
        C.wireCornerDrag.active = true;
        C.wireCornerDrag.conn = conn;
        C.wireCornerDrag.index = index;
        C.wireCornerDrag.pointerId = event.pointerId as unknown as number;
        try {
          C.svg?.setPointerCapture?.(event.pointerId as unknown as number);
        } catch (_err) {}
      });
      if (ctx.wireCornerLayer) ctx.wireCornerLayer.appendChild(handle);
    });
  }
  return {
    clearWireCornerHandles,
    clearWireJunctionDots,
    renderSharedSourceJunctionDots,
    moveWireCorner,
    updateWireCornerHandles,
  };
}
//=============================================================================
