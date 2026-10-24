//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { GRID_SIZE } from '../index.js';
import { HOP_RADIUS } from '../index.js';
import type { Point } from '../geometry.js';
//=============================================================================
export type Segment = {
  owner?: unknown;
  /** Content-based key of the owning connection; decides hop ownership. */
  ownerKey?: string;
  orientation: 'H' | 'V';
  a: Point;
  b: Point;
  minX?: number;
  maxX?: number;
  minY?: number;
  maxY?: number;
  x?: number;
  y?: number;
  isStub?: boolean;
};
/**
 * Content-based ordering key for a connection. Used to decide which of two
 * crossing wires draws the hop arc: the one whose key sorts FIRST. Being a
 * pure function of the endpoints, hop ownership is identical across full and
 * fast render passes and independent of iteration order.
 */
export function connOrderKey(conn: unknown): string | undefined {
  const c = conn as { from?: unknown; to?: unknown; fromIndex?: unknown; toIndex?: unknown };
  if (!c || c.from === undefined || c.to === undefined) return undefined;
  return `${c.from}:${Number(c.fromIndex ?? 0)}->${c.to}:${Number(c.toIndex ?? 0)}`;
}
//=============================================================================
export function buildFallbackPath(fromPos?: Point | null, toPos?: Point | null): Point[] {
  if (!fromPos || !toPos) return [];
  if (fromPos.x === toPos.x || fromPos.y === toPos.y) return [fromPos, toPos];

  // S/Z route with a vertical spine kept a stub clearance away from both ports.
  const STUB = 10; // aligned with GRID_SIZE in other code
  const aX = Math.min(fromPos.x, toPos.x);
  const bX = Math.max(fromPos.x, toPos.x);
  const centre = (fromPos.x + toPos.x) / 2;
  const minAllowed = aX + STUB;
  const maxAllowed = bX - STUB;

  let spineX: number;
  if (minAllowed <= maxAllowed) {
    if (centre < minAllowed) spineX = Math.round(minAllowed);
    else if (centre > maxAllowed) spineX = Math.round(maxAllowed);
    else spineX = Math.round(centre);
  } else {
    // too tight to satisfy both sides: bias the spine away from the destination
    spineX = Math.round(fromPos.x < toPos.x ? fromPos.x + STUB : fromPos.x - STUB);
  }

  return [fromPos, { x: spineX, y: fromPos.y }, { x: spineX, y: toPos.y }, toPos];
}
//=============================================================================
export type FallbackRect = { left: number; top: number; right: number; bottom: number };
/** True when an axis-aligned segment a-b passes through the interior of `rect`.
 *  Touching an edge does not count, so a wire may run flush along a block. */
function segmentHitsRect(a: Point, b: Point, rect: FallbackRect): boolean {
  if (a.y === b.y) {
    const y = a.y;
    if (y <= rect.top || y >= rect.bottom) return false;
    const lo = Math.min(a.x, b.x);
    const hi = Math.max(a.x, b.x);
    return hi > rect.left && lo < rect.right;
  }
  if (a.x === b.x) {
    const x = a.x;
    if (x <= rect.left || x >= rect.right) return false;
    const lo = Math.min(a.y, b.y);
    const hi = Math.max(a.y, b.y);
    return hi > rect.top && lo < rect.bottom;
  }
  return false;
}
//=============================================================================
export function pathHitsAnyRect(path: Point[], rects: FallbackRect[]): boolean {
  for (let i = 1; i < path.length; i += 1) {
    for (let r = 0; r < rects.length; r += 1) {
      if (segmentHitsRect(path[i - 1], path[i], rects[r])) return true;
    }
  }
  return false;
}
/**
 * Fallback route for a wire the router failed, avoiding blocks. Tries a
 * single-bend vertical and horizontal spine at every grid track between the
 * ports plus a margin outside them, and keeps the block-free candidate closest
 * to the mid-line. Falls back to the naive S/Z route when none is clean.
 * `rects` must EXCLUDE the source and target blocks (the ports sit on their
 * edges). Pure and deterministic.
 */
export function buildFallbackPathAvoidingBlocks(
  fromPos: Point | null | undefined,
  toPos: Point | null | undefined,
  rects: FallbackRect[] = [],
  grid = GRID_SIZE
): Point[] {
  if (!fromPos || !toPos) return [];
  if (fromPos.x === toPos.x || fromPos.y === toPos.y) return [fromPos, toPos];
  if (!rects.length) return buildFallbackPath(fromPos, toPos);

  const g = grid > 0 ? grid : GRID_SIZE;
  const MARGIN = 24; // grid steps of clearance to search outside the port span
  const snap = (v: number) => Math.round(v / g) * g;

  let best: Point[] | null = null;
  let bestScore = Infinity;
  const consider = (path: Point[], score: number) => {
    if (score >= bestScore) return;
    if (pathHitsAnyRect(path, rects)) return;
    bestScore = score;
    best = path;
  };

  // Vertical-spine candidates: [from] -> (spineX, fromY) -> (spineX, toY) -> [to]
  const midX = (fromPos.x + toPos.x) / 2;
  const loX = Math.min(fromPos.x, toPos.x);
  const hiX = Math.max(fromPos.x, toPos.x);
  const xs = new Set<number>();
  for (let x = snap(loX); x <= hiX; x += g) xs.add(x);
  for (let k = 1; k <= MARGIN; k += 1) {
    xs.add(snap(loX) - k * g);
    xs.add(snap(hiX) + k * g);
  }
  for (const x of xs) {
    consider([fromPos, { x, y: fromPos.y }, { x, y: toPos.y }, toPos], Math.abs(x - midX));
  }

  // Horizontal-spine candidates (slightly deprioritised so a vertical spine of
  // equal displacement wins, matching the naive builder's shape).
  const midY = (fromPos.y + toPos.y) / 2;
  const loY = Math.min(fromPos.y, toPos.y);
  const hiY = Math.max(fromPos.y, toPos.y);
  const ys = new Set<number>();
  for (let y = snap(loY); y <= hiY; y += g) ys.add(y);
  for (let k = 1; k <= MARGIN; k += 1) {
    ys.add(snap(loY) - k * g);
    ys.add(snap(hiY) + k * g);
  }
  for (const y of ys) {
    consider([fromPos, { x: fromPos.x, y }, { x: toPos.x, y }, toPos], Math.abs(y - midY) + g / 2);
  }

  return best ?? buildFallbackPath(fromPos, toPos);
}
//=============================================================================
export function buildSegments(points: Point[] | undefined | null, owner?: unknown): Segment[] {
  const out: Segment[] = [];
  if (!points || points.length < 2) return out;
  for (let i = 0; i < points.length; i += 1) {
    const p = points[i];
    if (!Number.isFinite(p?.x) || !Number.isFinite(p?.y)) return out;
  }
  const pts = points as Point[];
  const ownerKey = connOrderKey(owner);

  let segStart = points[0];
  let previous = points[0];
  let orient: 'V' | 'H' = points[1].x === points[0].x ? 'V' : 'H';

  function emit(s: Point, e: Point, overrideStub: boolean | null = null) {
    if (s.x === e.x && s.y === e.y) return;
    if (orient === 'V') {
      const minY = Math.min(s.y, e.y);
      const maxY = Math.max(s.y, e.y);
      const len = maxY - minY;
      const isStub =
        overrideStub ??
        ((s === pts[0] && len <= GRID_SIZE) || (e === pts[pts.length - 1] && len <= GRID_SIZE));
      out.push({ owner, ownerKey, orientation: 'V', a: s, b: e, minY, maxY, x: s.x, isStub });
    } else {
      const minX = Math.min(s.x, e.x);
      const maxX = Math.max(s.x, e.x);
      const len = maxX - minX;
      const isStub =
        overrideStub ??
        ((s === pts[0] && len <= GRID_SIZE) || (e === pts[pts.length - 1] && len <= GRID_SIZE));
      out.push({ owner, ownerKey, orientation: 'H', a: s, b: e, minX, maxX, y: s.y, isStub });
    }
  }

  for (let i = 1; i < points.length; i += 1) {
    const curr = points[i];
    const stepOrient: 'V' | 'H' = curr.x === previous.x ? 'V' : 'H';
    if (stepOrient !== orient) {
      emit(segStart, previous);
      segStart = previous;
      orient = stepOrient;
    }
    previous = curr;
  }
  emit(segStart, previous);
  return out;
}
//=============================================================================
type Crossing = { coord: number; minOtherKey?: string };
function noteCrossing(map: Map<number, string | undefined>, coord: number, otherKey?: string) {
  if (!map.has(coord)) {
    map.set(coord, otherKey);
    return;
  }
  const existing = map.get(coord);
  if (otherKey !== undefined && (existing === undefined || otherKey < existing)) {
    map.set(coord, otherKey);
  }
}
//=============================================================================
function crossingsFromMap(map: Map<number, string | undefined>): Crossing[] {
  return Array.from(map.entries()).map(([coord, minOtherKey]) => ({ coord, minOtherKey }));
}
//=============================================================================
function getCrossingsOnHorizontal(seg: Segment, otherSegments: Segment[]): Crossing[] {
  const result = new Map<number, string | undefined>();
  const y = seg.y as number;
  for (let i = 0; i < otherSegments.length; i += 1) {
    const other = otherSegments[i];
    if (other.orientation !== 'V') continue;
    if (seg.ownerKey !== undefined && other.ownerKey === seg.ownerKey) continue;
    const ox = other.x as number;
    if (ox <= (seg.minX as number) || ox >= (seg.maxX as number)) continue;
    if (y <= (other.minY as number) || y >= (other.maxY as number)) continue;
    noteCrossing(result, ox, other.ownerKey);
  }
  return crossingsFromMap(result);
}
//=============================================================================
function getCrossingsOnVertical(seg: Segment, otherSegments: Segment[]): Crossing[] {
  const result = new Map<number, string | undefined>();
  const x = seg.x as number;
  for (let i = 0; i < otherSegments.length; i += 1) {
    const other = otherSegments[i];
    if (other.orientation !== 'H') continue;
    if (seg.ownerKey !== undefined && other.ownerKey === seg.ownerKey) continue;
    const oy = other.y as number;
    if (oy <= (seg.minY as number) || oy >= (seg.maxY as number)) continue;
    if (x <= (other.minX as number) || x >= (other.maxX as number)) continue;
    noteCrossing(result, oy, other.ownerKey);
  }
  return crossingsFromMap(result);
}
/**
 * At a crossing the wire whose key sorts FIRST draws the hop. With no key on
 * either side (legacy callers) the drawing wire hops.
 */
function ownsHop(segKey: string | undefined, minOtherKey: string | undefined): boolean {
  if (segKey === undefined || minOtherKey === undefined) return true;
  return segKey <= minOtherKey;
}
/** Corner rounding radius (px); kept below GRID_SIZE so port stubs always
 *  retain a straight run for the arrow marker. */
const CORNER_RADIUS = 4;
export function buildPathWithHops(
  segments: Segment[],
  otherSegments: Segment[],
  hopClaims: Map<string, Set<string>> | Set<string> | null = null,
  options: { roundedCorners?: boolean } = {}
): string {
  if (!segments.length) return '';
  const rounded = options.roundedCorners === true;
  const commands: string[] = [];
  let current = segments[0].a;
  commands.push(`M ${current.x} ${current.y}`);

  const segLen = (seg: Segment) =>
    seg.orientation === 'H'
      ? (seg.maxX as number) - (seg.minX as number)
      : (seg.maxY as number) - (seg.minY as number);
  const dirOf = (seg: Segment) =>
    seg.orientation === 'H'
      ? { x: Math.sign(seg.b.x - seg.a.x), y: 0 }
      : { x: 0, y: Math.sign(seg.b.y - seg.a.y) };
  const cornerAfter = (idx: number): { dIn: Point; dOut: Point } | null => {
    if (!rounded) return null;
    const seg = segments[idx];
    const next = segments[idx + 1];
    if (!next) return null;
    if (seg.orientation === next.orientation) return null;
    if (seg.b.x !== next.a.x || seg.b.y !== next.a.y) return null;
    if (segLen(seg) < CORNER_RADIUS * 2 || segLen(next) < CORNER_RADIUS * 2) return null;
    // Keep clear of hop arcs near the corner on either side.
    const guard = CORNER_RADIUS + HOP_RADIUS * 2;
    const nearCorner = (s: Segment) => {
      const crossings =
        s.orientation === 'H'
          ? getCrossingsOnHorizontal(s, otherSegments)
          : getCrossingsOnVertical(s, otherSegments);
      const corner = s.orientation === 'H' ? seg.b.x : seg.b.y;
      return crossings.some((c) => Math.abs(c.coord - corner) < guard);
    };
    if (nearCorner(seg) || nearCorner(next)) return null;
    return { dIn: dirOf(seg), dOut: dirOf(next) };
  };
  const hopKey = (x: number, y: number) => `${Math.round(x)},${Math.round(y)}`;
  const canDrawHop = (claims: typeof hopClaims, key: string, orientation: 'H' | 'V') => {
    if (!claims) return true;
    const opposite = orientation === 'H' ? 'V' : 'H';
    if (claims instanceof Map) {
      let set = claims.get(key);
      if (!set) {
        set = new Set([orientation]);
        claims.set(key, set);
        return true;
      }
      if (set.has(opposite)) return false;
      set.add(orientation);
      return true;
    }
    if (claims instanceof Set) {
      const sameKey = `${key}|${orientation}`;
      const oppositeKey = `${key}|${opposite}`;
      if (claims.has(oppositeKey) || claims.has(key)) return false;
      claims.add(sameKey);
      return true;
    }
    return true;
  };
  const hopSweepFlag = (dir: number) => (dir === 1 ? 1 : 0);

  const emitTail = (seg: Segment, idx: number) => {
    const corner = cornerAfter(idx);
    if (!corner) {
      commands.push(`L ${seg.b.x} ${seg.b.y}`);
      current = seg.b;
      return;
    }
    const { dIn, dOut } = corner;
    const sweep = dIn.x * dOut.y - dIn.y * dOut.x > 0 ? 1 : 0;
    commands.push(`L ${seg.b.x - CORNER_RADIUS * dIn.x} ${seg.b.y - CORNER_RADIUS * dIn.y}`);
    commands.push(
      `a ${CORNER_RADIUS} ${CORNER_RADIUS} 0 0 ${sweep} ${CORNER_RADIUS * (dIn.x + dOut.x)} ${
        CORNER_RADIUS * (dIn.y + dOut.y)
      }`
    );
    // The arc lands ON the next segment's line past its start point; mark the
    // corner as reached so the next iteration does not jump back to it.
    current = seg.b;
  };

  segments.forEach((seg, idx) => {
    if (current.x !== seg.a.x || current.y !== seg.a.y) {
      commands.push(`L ${seg.a.x} ${seg.a.y}`);
      current = seg.a;
    }
    if (seg.isStub) {
      emitTail(seg, idx);
      return;
    }
    if (seg.orientation === 'H') {
      const dir = seg.a.x <= seg.b.x ? 1 : -1;
      const crossings = getCrossingsOnHorizontal(seg, otherSegments)
        .filter(
          (c) =>
            c.coord > (seg.minX as number) + HOP_RADIUS &&
            c.coord < (seg.maxX as number) - HOP_RADIUS
        )
        .sort((a, b) => (dir === 1 ? a.coord - b.coord : b.coord - a.coord));
      crossings.forEach(({ coord: x, minOtherKey }) => {
        if (!ownsHop(seg.ownerKey, minOtherKey)) return;
        const key = hopKey(x, seg.a.y);
        if (!canDrawHop(hopClaims, key, 'H')) return;
        const sweep = hopSweepFlag(dir);
        commands.push(`L ${x - HOP_RADIUS * dir} ${seg.a.y}`);
        commands.push(`a ${HOP_RADIUS} ${HOP_RADIUS} 0 0 ${sweep} ${HOP_RADIUS * 2 * dir} 0`);
      });
      emitTail(seg, idx);
      return;
    } else {
      const dir = seg.a.y <= seg.b.y ? 1 : -1;
      const crossings = getCrossingsOnVertical(seg, otherSegments)
        .filter(
          (c) =>
            c.coord > (seg.minY as number) + HOP_RADIUS &&
            c.coord < (seg.maxY as number) - HOP_RADIUS
        )
        .sort((a, b) => (dir === 1 ? a.coord - b.coord : b.coord - a.coord));
      crossings.forEach(({ coord: y, minOtherKey }) => {
        if (!ownsHop(seg.ownerKey, minOtherKey)) return;
        const key = hopKey(seg.a.x, y);
        if (!canDrawHop(hopClaims, key, 'V')) return;
        const sweep = hopSweepFlag(dir);
        commands.push(`L ${seg.a.x} ${y - HOP_RADIUS * dir}`);
        commands.push(`a ${HOP_RADIUS} ${HOP_RADIUS} 0 0 ${sweep} 0 ${HOP_RADIUS * 2 * dir}`);
      });
      emitTail(seg, idx);
    }
  });

  return commands.join(' ');
}
//=============================================================================
export function sameWirePoint(a?: Point | null, b?: Point | null): boolean {
  return Boolean(a && b && a.x === b.x && a.y === b.y);
}
//=============================================================================
function pointKey(point: Point) {
  return `${point.x},${point.y}`;
}
//=============================================================================
export function normalizeWirePoints(points: unknown): Point[] {
  if (!Array.isArray(points)) return [];
  const out: Point[] = [];
  const arr = points as unknown[];
  for (let i = 0; i < arr.length; i += 1) {
    const pt = arr[i];
    let xRaw: unknown = undefined;
    let yRaw: unknown = undefined;
    if (pt && typeof pt === 'object') {
      if ('x' in pt) xRaw = (pt as { x?: unknown }).x;
      if ('y' in pt) yRaw = (pt as { y?: unknown }).y;
    }
    if (xRaw === undefined && Array.isArray(pt)) xRaw = (pt as unknown[])[0];
    if (yRaw === undefined && Array.isArray(pt)) yRaw = (pt as unknown[])[1];
    const x = Number(xRaw ?? NaN);
    const y = Number(yRaw ?? NaN);
    if (!Number.isFinite(x) || !Number.isFinite(y)) continue;
    const node = { x, y };
    if (!out.length || !sameWirePoint(out[out.length - 1], node)) out.push(node);
  }
  return out;
}
//=============================================================================
function segmentVector(from: Point | null, to: Point | null) {
  if (!from || !to) return null;
  const dxRaw = to.x - from.x;
  const dyRaw = to.y - from.y;
  if (dxRaw === 0 && dyRaw === 0) return null;
  if (dxRaw !== 0 && dyRaw !== 0) return null;
  if (dxRaw !== 0) return { dx: Math.sign(dxRaw), dy: 0, len: Math.abs(dxRaw) };
  return { dx: 0, dy: Math.sign(dyRaw), len: Math.abs(dyRaw) };
}
//=============================================================================
function firstSplitPointFromSource(pathA: unknown, pathB: unknown): Point | null {
  const a = normalizeWirePoints(pathA);
  const b = normalizeWirePoints(pathB);
  if (a.length < 2 || b.length < 2) return null;
  if (!sameWirePoint(a[0], b[0])) return null;

  let ia = 0;
  let ib = 0;
  let pos: Point = { ...a[0] };
  let progressed = 0;

  while (ia < a.length - 1 && ib < b.length - 1) {
    while (ia < a.length - 1 && sameWirePoint(pos, a[ia + 1])) ia += 1;
    while (ib < b.length - 1 && sameWirePoint(pos, b[ib + 1])) ib += 1;
    if (ia >= a.length - 1 || ib >= b.length - 1) break;

    const va = segmentVector(pos, a[ia + 1]);
    const vb = segmentVector(pos, b[ib + 1]);
    if (!va || !vb) break;

    if (va.dx !== vb.dx || va.dy !== vb.dy) {
      return progressed > 0 ? { ...pos } : null;
    }

    const step = Math.min(va.len, vb.len);
    if (!(step > 0)) break;
    pos = { x: pos.x + va.dx * step, y: pos.y + va.dy * step };
    progressed += step;

    const endA = sameWirePoint(pos, a[ia + 1]);
    const endB = sameWirePoint(pos, b[ib + 1]);
    if (endA) ia += 1;
    if (endB) ib += 1;

    const hasNextA = ia < a.length - 1;
    const hasNextB = ib < b.length - 1;
    if (!hasNextA && !hasNextB) return null;
    if (!hasNextA || !hasNextB) return progressed > 0 ? { ...pos } : null;

    const nextA = segmentVector(pos, a[ia + 1]);
    const nextB = segmentVector(pos, b[ib + 1]);
    if (!nextA || !nextB) break;

    if (endA && !endB) {
      if (nextA.dx !== va.dx || nextA.dy !== va.dy) return progressed > 0 ? { ...pos } : null;
      continue;
    }
    if (!endA && endB) {
      if (nextB.dx !== va.dx || nextB.dy !== va.dy) return progressed > 0 ? { ...pos } : null;
      continue;
    }
    if (nextA.dx !== nextB.dx || nextA.dy !== nextB.dy) return progressed > 0 ? { ...pos } : null;
  }
  return null;
}
//=============================================================================
export function collectSharedSourceSplitPoints(
  connections: unknown[] = [],
  getPoints: ((conn: unknown) => unknown) | null = null
): Point[] {
  const groups = new Map<string, Point[][]>();
  connections.forEach((conn) => {
    if (!conn) return;
    const raw =
      typeof getPoints === 'function' ? getPoints(conn) : (conn as { points?: unknown }).points;
    const pts = normalizeWirePoints(raw);
    if (pts.length < 2) return;
    const fromVal = (conn as { from?: unknown }).from;
    const fromIndexVal = (conn as { fromIndex?: unknown }).fromIndex;
    const sourceKey = `${String(fromVal)}:${Number(fromIndexVal ?? 0)}`;
    const list = groups.get(sourceKey) || [];
    list.push(pts);
    groups.set(sourceKey, list);
  });

  const splitSet = new Set<string>();
  groups.forEach((paths) => {
    if (paths.length < 2) return;
    for (let i = 0; i < paths.length; i += 1) {
      for (let j = i + 1; j < paths.length; j += 1) {
        const split = firstSplitPointFromSource(paths[i], paths[j]);
        if (split) splitSet.add(pointKey(split));
      }
    }
  });

  return Array.from(splitSet).map((key) => {
    const [x, y] = key.split(',').map(Number);
    return { x, y };
  });
}
//=============================================================================
