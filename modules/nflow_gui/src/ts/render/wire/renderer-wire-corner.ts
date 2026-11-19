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
const WIRE_ENDPOINT_HANDLE_RADIUS = 6;
// Pull the endpoint handle in off the port so it sits on the wire, clear of the
// port's own hit circle: the port stays free to start a new (fan-out) wire.
const WIRE_ENDPOINT_HANDLE_INSET = 16;
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
  function clearWireEndpointHandles() {
    while (ctx.wireEndpointLayer && ctx.wireEndpointLayer.firstChild)
      ctx.wireEndpointLayer.removeChild(ctx.wireEndpointLayer.firstChild);
  }
  // A point `d` away from `a` toward `b` (clamped when the ends are closer).
  function insetAlong(
    a: { x: number; y: number },
    b: { x: number; y: number },
    d: number
  ): { x: number; y: number } {
    const dx = b.x - a.x;
    const dy = b.y - a.y;
    const len = Math.hypot(dx, dy);
    if (len <= d) return { x: a.x, y: a.y };
    return { x: a.x + (dx / len) * d, y: a.y + (dy / len) * d };
  }
  // The point at half the polyline's length (its visual middle).
  function midpointAlong(pts: { x: number; y: number }[]): { x: number; y: number } | null {
    if (!Array.isArray(pts) || pts.length < 2) return null;
    let total = 0;
    for (let i = 0; i < pts.length - 1; i += 1)
      total += Math.hypot(pts[i + 1].x - pts[i].x, pts[i + 1].y - pts[i].y);
    if (total === 0) return { x: pts[0].x, y: pts[0].y };
    const half = total / 2;
    let acc = 0;
    for (let i = 0; i < pts.length - 1; i += 1) {
      const seg = Math.hypot(pts[i + 1].x - pts[i].x, pts[i + 1].y - pts[i].y);
      if (acc + seg >= half) {
        const t = (half - acc) / seg;
        return { x: pts[i].x + (pts[i + 1].x - pts[i].x) * t, y: pts[i].y + (pts[i + 1].y - pts[i].y) * t };
      }
      acc += seg;
    }
    return { x: pts[pts.length - 1].x, y: pts[pts.length - 1].y };
  }
  // Draggable handles at both ends of the selected wire. Grabbing a handle
  // re-targets that end (same flow for the source and destination, Simulink
  // style). Handles sit inset on the wire, so the ports stay free for fan-out.
  function updateWireEndpointHandles() {
    clearWireEndpointHandles();
    const state = ctx.state as unknown as { selectedConnection?: Conn | null; locked?: boolean };
    const conn = state.selectedConnection;
    if (!conn || state.locked) return;
    const Cx = ctx as any;
    const pts = (conn.points as { x: number; y: number }[] | undefined) ?? [];
    const endPoint = (end: 'from' | 'to'): { x: number; y: number } | null => {
      if (Array.isArray(pts) && pts.length >= 2) {
        return end === 'from'
          ? insetAlong(pts[0], pts[1], WIRE_ENDPOINT_HANDLE_INSET)
          : insetAlong(pts[pts.length - 1], pts[pts.length - 2], WIRE_ENDPOINT_HANDLE_INSET);
      }
      return end === 'from'
        ? (Cx.getPortPosition?.(conn.from, 'out', Number(conn.fromIndex ?? 0)) ?? null)
        : (Cx.getPortPosition?.(conn.to, 'in', Number(conn.toIndex ?? 0)) ?? null);
    };
    (['from', 'to'] as const).forEach((end) => {
      const pos = endPoint(end);
      if (!pos) return;
      const handle = createSvgElement('circle', {
        class: 'wire-endpoint-handle',
        cx: pos.x,
        cy: pos.y,
        r: WIRE_ENDPOINT_HANDLE_RADIUS,
        'data-end': end,
      });
      handle.addEventListener('pointerdown', (event: PointerEvent) => {
        event.preventDefault();
        event.stopPropagation();
        Cx.armWireEndpointFromHandle?.(event, conn, end);
      });
      if (ctx.wireEndpointLayer) ctx.wireEndpointLayer.appendChild(handle);
    });

    // Branch handle at the wire's middle: drag it to a free input to fan the
    // source out to a second wire, without the Ctrl/Cmd modifier.
    const mid = midpointAlong(pts);
    if (mid) {
      const group = createSvgElement('g', { class: 'wire-branch-handle', 'data-role': 'branch' });
      group.appendChild(
        createSvgElement('circle', { class: 'wire-branch-dot', cx: mid.x, cy: mid.y, r: 7 })
      );
      group.appendChild(
        createSvgElement('path', {
          class: 'wire-branch-plus',
          d: `M ${mid.x - 3} ${mid.y} h 6 M ${mid.x} ${mid.y - 3} v 6`,
        })
      );
      group.addEventListener('pointerdown', (event: PointerEvent) => {
        event.preventDefault();
        event.stopPropagation();
        Cx.armWireBranchFromHandle?.(event, conn);
      });
      if (ctx.wireEndpointLayer) ctx.wireEndpointLayer.appendChild(group);
    }
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
    // Endpoint handles show for any selected wire (both routing modes); the
    // corner handles below are only for manually-routed (autoRoute off) wires.
    updateWireEndpointHandles();
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
    clearWireEndpointHandles,
    renderSharedSourceJunctionDots,
    moveWireCorner,
    updateWireCornerHandles,
    updateWireEndpointHandles,
  };
}
//=============================================================================
