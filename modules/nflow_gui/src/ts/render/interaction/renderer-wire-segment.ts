//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Wire-segment editing: grab an interior segment of an auto-routed wire and
// slide it perpendicular to itself; neighbors stretch, the rest is frozen.
// On release the moved segment's endpoints become PINNED (user-anchored)
// waypoints, persisted in the .nflow file and treated as a static route by
// the router. Double-click a pinned wire to release its pins (auto-route).
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { GRID_SIZE, alignToGrid } from '../geometry.js';
import { dedupePoints, removeColinearPoints } from '../wire/wire-helpers.js';
import type { RendererCtx, Conn } from '../core/renderer-types.js';
//=============================================================================
type Point = { x: number; y: number; pin?: boolean };
export type WireSegmentDrag = {
  phase: 'idle' | 'armed' | 'dragging';
  conn: Conn | null;
  segIndex: number;
  orient: 'H' | 'V' | null;
  pointerId: number | null;
  startPoint: Point | null;
  prePoints: Point[] | null; // for Esc/cancel restore
  workPoints: Point[] | null; // mutable working copy during the drag
};
//=============================================================================
const ARM_THRESHOLD_PX = 4;
const HIT_THRESHOLD_PX = 6;
function distancePointToSegment(p: Point, a: Point, b: Point): number {
  const dx = b.x - a.x;
  const dy = b.y - a.y;
  const lenSq = dx * dx + dy * dy;
  if (lenSq === 0) return Math.hypot(p.x - a.x, p.y - a.y);
  let t = ((p.x - a.x) * dx + (p.y - a.y) * dy) / lenSq;
  t = Math.max(0, Math.min(1, t));
  return Math.hypot(p.x - (a.x + t * dx), p.y - (a.y + t * dy));
}
//=============================================================================
function clonePoints(points: Point[]): Point[] {
  return points.map((p) => (p.pin ? { x: p.x, y: p.y, pin: true } : { x: p.x, y: p.y }));
}
//=============================================================================
export function makeWireSegmentHandlers(ctx: RendererCtx) {
  const C = ctx as any;

  const wireSegmentDrag: WireSegmentDrag = {
    phase: 'idle',
    conn: null,
    segIndex: -1,
    orient: null,
    pointerId: null,
    startPoint: null,
    prePoints: null,
    workPoints: null,
  };

  function editingAllowed(): boolean {
    const S = ctx.state as any;
    return !S.locked && S.autoRoute !== false;
  }

  /**
   * Resolve the interior, draggable segment nearest to `point`.
   * Terminal segments (port stubs) are excluded.
   */
  function resolveWireSegmentAt(
    point: Point
  ): { conn: Conn; segIndex: number; orient: 'H' | 'V' } | null {
    let best: { conn: Conn; segIndex: number; orient: 'H' | 'V' } | null = null;
    let bestDist = Infinity;
    for (const conn of ctx.state.connections as any[]) {
      const pts = conn.points as Point[] | undefined;
      if (!Array.isArray(pts) || pts.length < 4) continue;
      for (let i = 1; i <= pts.length - 3; i += 1) {
        const a = pts[i];
        const b = pts[i + 1];
        if (!a || !b) continue;
        const orient = a.x === b.x && a.y !== b.y ? 'V' : a.y === b.y && a.x !== b.x ? 'H' : null;
        if (!orient) continue;
        const len = Math.abs(b.x - a.x) + Math.abs(b.y - a.y);
        if (len < GRID_SIZE) continue;
        const dist = distancePointToSegment(point, a, b);
        if (dist < bestDist) {
          bestDist = dist;
          best = { conn, segIndex: i, orient };
        }
      }
    }
    return bestDist <= HIT_THRESHOLD_PX ? best : null;
  }

  function moveWireSegment(points: Point[], segIndex: number, target: Point): boolean {
    const a = points[segIndex];
    const b = points[segIndex + 1];
    if (!a || !b) return false;
    const orient = a.x === b.x ? 'V' : 'H';
    const aligned = alignToGrid({ x: target.x, y: target.y });
    if (orient === 'H') {
      if (a.y === aligned.y) return false;
      a.y = aligned.y;
      b.y = aligned.y;
    } else {
      if (a.x === aligned.x) return false;
      a.x = aligned.x;
      b.x = aligned.x;
    }
    return true;
  }

  function armWireSegmentDrag(event: PointerEvent): boolean {
    if (!editingAllowed()) return false;
    if (wireSegmentDrag.phase !== 'idle') return false;
    const target = event.target as Element | null;
    if (!target || !(target.classList?.contains('wire-hit') || target.classList?.contains('wire')))
      return false;
    const point = C.clientToSvg(event.clientX, event.clientY);
    const hit = resolveWireSegmentAt(point);
    if (!hit) return false;
    wireSegmentDrag.phase = 'armed';
    wireSegmentDrag.conn = hit.conn;
    wireSegmentDrag.segIndex = hit.segIndex;
    wireSegmentDrag.orient = hit.orient;
    wireSegmentDrag.pointerId = event.pointerId as unknown as number;
    wireSegmentDrag.startPoint = point;
    wireSegmentDrag.prePoints = clonePoints((hit.conn as any).points);
    wireSegmentDrag.workPoints = null;
    try {
      C.svg?.setPointerCapture?.(event.pointerId as unknown as number);
    } catch (_err) {}
    // Do NOT stop propagation: a plain click must still select the wire.
    return true;
  }

  function beginDragging() {
    const conn = wireSegmentDrag.conn as any;
    if (!conn) return;
    const pts = clonePoints(conn.points as Point[]);
    let segIndex = wireSegmentDrag.segIndex;
    // Stub protection: when grabbing the segment adjacent to a port stub,
    // duplicate the shared bend so the stub keeps its anchor and the new
    // bend absorbs the motion.
    if (segIndex === 1) {
      pts.splice(1, 0, { x: pts[1].x, y: pts[1].y });
      segIndex += 1;
    }
    if (segIndex === pts.length - 3) {
      pts.splice(pts.length - 2, 0, { x: pts[pts.length - 2].x, y: pts[pts.length - 2].y });
    }
    wireSegmentDrag.workPoints = pts;
    wireSegmentDrag.segIndex = segIndex;
    wireSegmentDrag.phase = 'dragging';
    if (C.svg) C.svg.style.cursor = wireSegmentDrag.orient === 'H' ? 'ns-resize' : 'ew-resize';
  }

  /** Returns true when the event was consumed by an active segment drag. */
  function updateWireSegmentDrag(event: PointerEvent): boolean {
    if (wireSegmentDrag.phase === 'idle') return false;
    if (event.pointerId !== wireSegmentDrag.pointerId) return false;
    const point = C.clientToSvg(event.clientX, event.clientY);

    if (wireSegmentDrag.phase === 'armed') {
      const start = wireSegmentDrag.startPoint as Point;
      if (Math.hypot(point.x - start.x, point.y - start.y) < ARM_THRESHOLD_PX) return true;
      beginDragging();
    }

    const conn = wireSegmentDrag.conn as any;
    const pts = wireSegmentDrag.workPoints;
    if (!conn || !pts) return true;
    if (moveWireSegment(pts, wireSegmentDrag.segIndex, point)) {
      conn.points = clonePoints(pts);
      C.applyWirePathsFast?.(new Set([conn]));
      C.updateSelectionBox?.();
    }
    return true;
  }

  function resetDragState() {
    wireSegmentDrag.phase = 'idle';
    wireSegmentDrag.conn = null;
    wireSegmentDrag.segIndex = -1;
    wireSegmentDrag.orient = null;
    wireSegmentDrag.pointerId = null;
    wireSegmentDrag.startPoint = null;
    wireSegmentDrag.prePoints = null;
    wireSegmentDrag.workPoints = null;
    if (C.svg) C.svg.style.cursor = '';
  }

  /** Returns true when the event was consumed by an active segment drag. */
  function commitWireSegmentDrag(event: PointerEvent): boolean {
    if (wireSegmentDrag.phase === 'idle') return false;
    if (event.pointerId !== wireSegmentDrag.pointerId) return false;
    const wasDragging = wireSegmentDrag.phase === 'dragging';
    const conn = wireSegmentDrag.conn as any;

    if (wasDragging && conn && wireSegmentDrag.workPoints) {
      const pts = clonePoints(wireSegmentDrag.workPoints);
      const i = wireSegmentDrag.segIndex;
      if (pts[i]) pts[i].pin = true;
      if (pts[i + 1]) pts[i + 1].pin = true;
      // Inserted duplicates that never diverged merge back; pinned points survive.
      conn.points = removeColinearPoints(dedupePoints(pts as any)) as any;

      const S = ctx.state as any;
      S.routingDirty = true;
      S.dirtyConnections = S.dirtyConnections || new Set();
      S.dirtyConnections.add(conn);
      C.updateConnections?.(true);
      if (typeof window !== 'undefined') {
        window.dispatchEvent(new CustomEvent('diagramChanged'));
        window.dispatchEvent(new CustomEvent('diagramCommitted'));
      }
    }
    resetDragState();
    return wasDragging;
  }

  function cancelWireSegmentDrag(event?: PointerEvent | null): boolean {
    if (wireSegmentDrag.phase === 'idle') return false;
    if (event && event.pointerId !== wireSegmentDrag.pointerId) return false;
    const conn = wireSegmentDrag.conn as any;
    if (wireSegmentDrag.phase === 'dragging' && conn && wireSegmentDrag.prePoints) {
      conn.points = clonePoints(wireSegmentDrag.prePoints);
      C.applyWirePathsFast?.(new Set([conn]));
      C.updateSelectionBox?.();
    }
    resetDragState();
    return true;
  }

  function releaseWirePins(conn: Conn): boolean {
    const pts = (conn as any).points as Point[] | undefined;
    if (!Array.isArray(pts) || !pts.some((p) => p.pin)) return false;
    for (const p of pts) delete p.pin;
    const S = ctx.state as any;
    S.routingDirty = true;
    S.dirtyConnections = S.dirtyConnections || new Set();
    S.dirtyConnections.add(conn);
    C.updateConnections?.(true);
    if (typeof window !== 'undefined') {
      window.dispatchEvent(new CustomEvent('diagramChanged'));
      window.dispatchEvent(new CustomEvent('diagramCommitted'));
    }
    return true;
  }

  let hoverCursorSet = false;

  function handleWireSegmentHover(event: PointerEvent): void {
    if (wireSegmentDrag.phase !== 'idle' || !C.svg) return;
    const target = event.target as Element | null;
    const onWire =
      target && (target.classList?.contains('wire-hit') || target.classList?.contains('wire'));
    if (!onWire || !editingAllowed()) {
      if (hoverCursorSet) {
        C.svg.style.cursor = '';
        hoverCursorSet = false;
      }
      return;
    }
    const point = C.clientToSvg(event.clientX, event.clientY);
    const hit = resolveWireSegmentAt(point);
    if (hit) {
      C.svg.style.cursor = hit.orient === 'H' ? 'ns-resize' : 'ew-resize';
      hoverCursorSet = true;
    } else if (hoverCursorSet) {
      C.svg.style.cursor = '';
      hoverCursorSet = false;
    }
  }

  function handleWireDoubleClick(event: MouseEvent): boolean {
    if (!editingAllowed()) return false;
    const target = event.target as Element | null;
    if (!target || !(target.classList?.contains('wire-hit') || target.classList?.contains('wire')))
      return false;
    const point = C.clientToSvg(event.clientX, event.clientY);
    const hit = resolveWireSegmentAt(point);
    if (!hit) return false;
    return releaseWirePins(hit.conn);
  }

  return {
    wireSegmentDrag,
    resolveWireSegmentAt,
    armWireSegmentDrag,
    updateWireSegmentDrag,
    commitWireSegmentDrag,
    cancelWireSegmentDrag,
    releaseWirePins,
    handleWireDoubleClick,
    handleWireSegmentHover,
  };
}
//=============================================================================
