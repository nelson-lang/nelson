//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Point } from '../geometry.js';
import {
  buildFallbackPath,
  buildFallbackPathAvoidingBlocks,
  pathHitsAnyRect,
} from './wire-paths.js';
import type { FallbackRect } from './wire-paths.js';
import { rotatePoint } from '../geometry.js';
import { KEEP_OUT } from '../geometry.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
export function samePoint(a?: Point | null, b?: Point | null) {
  if (!a || !b) return false;
  return a.x === b.x && a.y === b.y;
}
//=============================================================================
export function dedupePoints(points: Point[]) {
  if (!points || points.length === 0) return [];
  const out: Point[] = [points[0]];
  for (let i = 1; i < points.length; i += 1) {
    const p = points[i];
    const last = out[out.length - 1];
    if (p.x !== last.x || p.y !== last.y) out.push(p);
    // A dropped duplicate donates its pin flag to the kept point.
    else if ((p as { pin?: boolean }).pin) (last as { pin?: boolean }).pin = true;
  }
  return out;
}
//=============================================================================
export function removeColinearPoints(points: Point[]) {
  if (!points || points.length <= 2) return points.slice();
  const out: Point[] = [points[0]];
  for (let i = 1; i < points.length - 1; i += 1) {
    const prev = out[out.length - 1];
    const cur = points[i];
    const next = points[i + 1];
    const colinear =
      (prev.x === cur.x && cur.x === next.x) || (prev.y === cur.y && cur.y === next.y);
    // Pinned (user-anchored) points are never merged away.
    if (!colinear || (cur as { pin?: boolean }).pin) out.push(cur);
  }
  out.push(points[points.length - 1]);
  return out;
}
//=============================================================================
export function makeWireHelpers({ state }: { state?: unknown } = {}) {
  function segmentOrientation(a: Point, b: Point) {
    if (!a || !b) return null;
    return a.x === b.x ? 'V' : a.y === b.y ? 'H' : null;
  }

  function getTurnIndices(points: Point[]) {
    const out: number[] = [];
    if (!points || points.length < 3) return out;
    let lastDir: 'H' | 'V' | null = null;
    for (let i = 0; i < points.length - 1; i += 1) {
      const a = points[i];
      const b = points[i + 1];
      const dir = a.x === b.x ? 'V' : a.y === b.y ? 'H' : null;
      if (!dir) continue;
      if (lastDir && dir !== lastDir) out.push(i);
      lastDir = dir;
    }
    return out;
  }

  function buildOrthogonalHead(from: Point, to: Point) {
    if (!from || !to) return [from, to].filter(Boolean) as Point[];
    if (from.x === to.x || from.y === to.y) return [from, to];
    return [from, { x: from.x, y: to.y }, to];
  }

  function buildOrthogonalTail(from: Point, to: Point) {
    return buildOrthogonalHead(from, to).reverse();
  }

  function buildFallbackPathFromPorts(conn: unknown) {
    const from = (conn as { from?: unknown }).from;
    const to = (conn as { to?: unknown }).to;
    const fromPos = getPortPosition(
      from,
      'out',
      Number((conn as { fromIndex?: unknown }).fromIndex ?? 0)
    );
    const toPos = getPortPosition(to, 'in', Number((conn as { toIndex?: unknown }).toIndex ?? 0));
    return buildFallbackPath(fromPos, toPos);
  }

  function buildFastDragPathFromPorts(conn: unknown) {
    return buildFallbackPathFromPorts(conn);
  }

  /** Axis-aligned bounds of every block except the two the connection joins
   *  (a wire's own ports sit on its endpoint blocks' edges), grown by a grid
   *  cell of clearance so a fallback route keeps clear of neighbours. */
  function collectFallbackObstacleRects(conn: unknown): FallbackRect[] {
    const rects: FallbackRect[] = [];
    const blkMap = (state as { blocks?: Map<string, unknown> })?.blocks;
    if (!blkMap || typeof (blkMap as Map<string, unknown>).values !== 'function') return rects;
    const fromId = String((conn as { from?: unknown })?.from ?? '');
    const toId = String((conn as { to?: unknown })?.to ?? '');
    for (const b of (blkMap as Map<string, unknown>).values()) {
      if (!b || typeof b !== 'object') continue;
      const rec = b as {
        id?: unknown;
        x?: unknown;
        y?: unknown;
        width?: unknown;
        height?: unknown;
      };
      const id = String(rec.id ?? '');
      if (id === fromId || id === toId) continue;
      const x = Number(rec.x);
      const y = Number(rec.y);
      const w = Number(rec.width);
      const h = Number(rec.height);
      if (!Number.isFinite(x) || !Number.isFinite(y) || !Number.isFinite(w) || !Number.isFinite(h))
        continue;
      rects.push({
        left: x - KEEP_OUT,
        top: y - KEEP_OUT,
        right: x + w + KEEP_OUT,
        bottom: y + h + KEEP_OUT,
      });
    }
    return rects;
  }

  /** Block-aware fallback: like buildFallbackPathFromPorts but routes around
   *  intervening blocks. Used when the router fails a wire, so the last-resort
   *  path does not slice across a block. */
  function buildBlockAwareFallbackFromPorts(conn: unknown) {
    const from = (conn as { from?: unknown }).from;
    const to = (conn as { to?: unknown }).to;
    const fromPos = getPortPosition(
      from,
      'out',
      Number((conn as { fromIndex?: unknown }).fromIndex ?? 0)
    );
    const toPos = getPortPosition(to, 'in', Number((conn as { toIndex?: unknown }).toIndex ?? 0));
    return buildFallbackPathAvoidingBlocks(fromPos, toPos, collectFallbackObstacleRects(conn));
  }

  /** True when `points` (default: the connection's own route) runs through a
   *  block the connection does not join. Used as a quality gate: a routed wire
   *  that slices across a foreign block can be replaced by the block-aware
   *  fallback. */
  function wireCrossesForeignBlock(conn: unknown, points?: Point[]): boolean {
    const pts = (points ?? (conn as { points?: Point[] })?.points) as Point[] | undefined;
    if (!Array.isArray(pts) || pts.length < 2) return false;
    return pathHitsAnyRect(pts, collectFallbackObstacleRects(conn));
  }

  function getPortPositionRaw(id: unknown, type: string, index = 0) {
    try {
      const blkMap = (state as { blocks?: Map<string, unknown> }).blocks;
      if (blkMap && typeof (blkMap as Map<string, unknown>).get === 'function') {
        const block = (blkMap as Map<string, unknown>).get(String(id));
        if (
          block &&
          typeof block === 'object' &&
          'x' in (block as object) &&
          'y' in (block as object)
        ) {
          const b0 = block as unknown as { x?: unknown; y?: unknown };
          const bx = Number(b0.x);
          const by = Number(b0.y);
          const ports = (block as unknown as { ports?: unknown[] }).ports as
            | Array<{ x: number; y: number; type?: string; index?: number }>
            | undefined;
          const port = Array.isArray(ports)
            ? ports.find((p) => p && p.type === type && p.index === (index ?? 0))
            : undefined;
          if (port && typeof port.x === 'number' && typeof port.y === 'number')
            return { x: bx + port.x, y: by + port.y };
          return { x: bx, y: by };
        }
      }
    } catch (e) {
      // swallow
    }
    return null;
  }

  function getPortPosition(id: unknown, type: string, index = 0) {
    const raw = getPortPositionRaw(id, type, index);
    if (!raw) return null;
    try {
      const blkMap = (state as { blocks?: Map<string, unknown> }).blocks;
      const block = blkMap && (blkMap as Map<string, unknown>).get(String(id));
      if (block && typeof block === 'object' && FieldNames.kRotation in (block as object)) {
        const b = block as unknown as {
          x?: unknown;
          y?: unknown;
          width?: unknown;
          height?: unknown;
          rotation?: unknown;
        };
        return rotatePoint(raw, {
          x: Number(b.x),
          y: Number(b.y),
          width: Number(b.width),
          height: Number(b.height),
          rotation: Number(b.rotation || 0),
        });
      }
    } catch (e) {
      // ignore
    }
    return raw;
  }

  function getWirePosition(conn: unknown) {
    const pts = (conn as { points?: unknown })?.points;
    if (Array.isArray(pts) && pts.length) return pts[0] as Point;
    const from = (conn as { from?: unknown }).from;
    const to = (conn as { to?: unknown }).to;
    const fromPos = getPortPosition(
      from,
      'out',
      Number((conn as { fromIndex?: unknown }).fromIndex ?? 0)
    );
    const toPos = getPortPosition(to, 'in', Number((conn as { toIndex?: unknown }).toIndex ?? 0));
    if (fromPos && toPos) return fromPos;
    return null;
  }

  function getWirePositionRaw(conn: unknown) {
    return getWirePosition(conn);
  }

  function applyWireOffsets(conn: unknown, points: Point[] | null | undefined) {
    void conn;
    return points || [];
  }

  return {
    dedupePoints,
    removeColinearPoints,
    samePoint,
    segmentOrientation,
    getTurnIndices,
    buildOrthogonalHead,
    buildOrthogonalTail,
    buildFallbackPathFromPorts,
    buildFastDragPathFromPorts,
    buildBlockAwareFallbackFromPorts,
    wireCrossesForeignBlock,
    getPortPosition,
    getPortPositionRaw,
    getWirePosition,
    getWirePositionRaw,
    applyWireOffsets,
  };
}
//=============================================================================
