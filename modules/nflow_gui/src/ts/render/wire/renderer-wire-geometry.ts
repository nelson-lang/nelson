//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { buildFallbackPath } from './wire-paths.js';
import { segmentsIntersect } from '../geometry.js';
import { simplifyOrthogonalPath as sharedSimplify } from '../../router/router.js';
import type { Conn } from '../core/renderer-types.js';
import type { Segment } from './wire-paths.js';
//=============================================================================
export function makeWireGeometryHandlers(ctx: unknown) {
  const C = ctx as unknown as {
    getPortPosition?: (
      id: string,
      type: 'in' | 'out',
      index: number
    ) => { x: number; y: number } | null;
    buildOrthogonalHead?: (
      from: { x: number; y: number },
      to: { x: number; y: number }
    ) => { x: number; y: number }[];
    buildOrthogonalTail?: (
      from: { x: number; y: number },
      to: { x: number; y: number }
    ) => { x: number; y: number }[];
    dedupePoints: (p: { x: number; y: number }[]) => { x: number; y: number }[];
    removeColinearPoints?: (p: { x: number; y: number }[]) => { x: number; y: number }[];
    samePoint?: (a?: { x: number; y: number }, b?: { x: number; y: number }) => boolean;
    checkPortDirections?: (
      conn: Conn,
      points: { x: number; y: number }[],
      issues: string[]
    ) => boolean;
    segmentHitsAnyBlock?: (segments: Segment[], conn: Conn, issues: string[]) => boolean;
    segmentOverlapsOtherWire?: (segments: Segment[], conn: Conn, issues: string[]) => boolean;
  };
  function buildDragRenderPoints(conn: Conn, points?: { x: number; y: number }[]) {
    const fromPos = C.getPortPosition
      ? C.getPortPosition(conn.from, 'out', conn.fromIndex ?? 0)
      : null;
    const toPos = C.getPortPosition ? C.getPortPosition(conn.to, 'in', conn.toIndex ?? 0) : null;
    if (!fromPos || !toPos) return points || [];
    if (!points || points.length === 0) return buildFallbackPath(fromPos, toPos);
    const start = points[0];
    const end = points[points.length - 1];
    let result = points.slice();
    if (start.x !== fromPos.x || start.y !== fromPos.y)
      result = [...(C.buildOrthogonalHead ? C.buildOrthogonalHead(fromPos, start) : []), ...result];
    if (end.x !== toPos.x || end.y !== toPos.y)
      result = [...result, ...(C.buildOrthogonalTail ? C.buildOrthogonalTail(end, toPos) : [])];
    return simplifyOrthogonalPath(C.dedupePoints(result));
  }

  function simplifyOrthogonalPath(points?: { x: number; y: number }[]) {
    if (!points || points.length === 0) return [];
    // Delegate to the canonical implementation in router.ts (colinear removal,
    // diagonal corners, loop removal). The context hooks are only a pre-pass.
    const prepared = C.removeColinearPoints
      ? C.removeColinearPoints(C.dedupePoints(points as { x: number; y: number }[]))
      : C.dedupePoints(points as { x: number; y: number }[]);
    return sharedSimplify(prepared);
  }

  function tryShorterOrthogonalPath(conn: Conn, points?: { x: number; y: number }[]) {
    if (!points || points.length < 3) return points || [];
    // Never rewrite a wire with pinned (user-anchored) interior points.
    if (points.some((p) => (p as { pin?: boolean }).pin)) return points;
    const start = points[0];
    const end = points[points.length - 1];
    const currentTurns = countTurns(points);
    const candidates: Array<{ x: number; y: number }[]> = [];
    if (start.x !== end.x && start.y !== end.y) {
      candidates.push([start, { x: end.x, y: start.y }, end]);
      candidates.push([start, { x: start.x, y: end.y }, end]);
    } else {
      candidates.push([start, end]);
    }
    for (let i = 0; i < candidates.length; i += 1) {
      const candidate = C.removeColinearPoints
        ? C.removeColinearPoints(C.dedupePoints(candidates[i]))
        : C.dedupePoints(candidates[i]);
      if (candidate.length < 2) continue;
      if (countTurns(candidate) >= currentTurns) continue;
      const issues: string[] = [];
      if (
        typeof C.checkPortDirections === 'function' &&
        !C.checkPortDirections(conn, candidate, issues)
      )
        continue;
      const segments: Segment[] = [];
      for (let j = 0; j < candidate.length - 1; j += 1) {
        const a = candidate[j];
        const b = candidate[j + 1];
        if (a.x !== b.x && a.y !== b.y) {
          segments.length = 0;
          break;
        }
        segments.push({ a, b, orientation: a.x === b.x ? 'V' : 'H' });
      }
      if (!segments.length) continue;
      if (typeof C.segmentHitsAnyBlock === 'function' && C.segmentHitsAnyBlock(segments, conn, []))
        continue;
      if (
        typeof C.segmentOverlapsOtherWire === 'function' &&
        C.segmentOverlapsOtherWire(segments, conn, [])
      )
        continue;
      const selfCross = segments.some((seg: Segment, idx: number) =>
        segments.slice(idx + 2).some((other: Segment) => segmentsIntersect(seg, other))
      );
      if (selfCross) continue;
      return candidate;
    }
    return points;
  }

  function countTurns(points?: { x: number; y: number }[]) {
    if (!points || points.length < 3) return 0;
    let turns = 0;
    let lastDir: 'H' | 'V' | null = null;
    for (let i = 0; i < points.length - 1; i += 1) {
      const a = points[i];
      const b = points[i + 1];
      const dir = a.x === b.x ? 'V' : a.y === b.y ? 'H' : null;
      if (!dir) continue;
      if (lastDir && dir !== lastDir) turns += 1;
      lastDir = dir;
    }
    return turns;
  }

  function orthogonalIntersection(
    a: { x: number; y: number },
    b: { x: number; y: number },
    c: { x: number; y: number },
    d: { x: number; y: number }
  ) {
    const aH = a.y === b.y;
    const bH = c.y === d.y;
    if (aH === bH) return null;
    const h = aH ? { a, b } : { a: c, b: d };
    const v = aH ? { a: c, b: d } : { a, b };
    const hx1 = Math.min(h.a.x, h.b.x);
    const hx2 = Math.max(h.a.x, h.b.x);
    const vy1 = Math.min(v.a.y, v.b.y);
    const vy2 = Math.max(v.a.y, v.b.y);
    const ix = v.a.x;
    const iy = h.a.y;
    if (ix > hx1 && ix < hx2 && iy > vy1 && iy < vy2) return { x: ix, y: iy };
    return null;
  }

  return {
    buildDragRenderPoints,
    simplifyOrthogonalPath,
    tryShorterOrthogonalPath,
    countTurns,
    orthogonalIntersection,
  };
}
//=============================================================================
