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
import { GRID_SIZE, alignToGrid } from '../utils/geometry.js';
import { rotatePoint, getPortSide, getRotatedBounds, sideToDir } from '../utils/port-utils.js';
import { nudgeConnections } from './nudge.js';
import { physicalNetKeys } from './physical-net.js';
import { orderFanOutTurns } from './fan-order.js';
//=============================================================================
type Point = { x: number; y: number };
type Segment = {
  a: Point;
  b: Point;
  orientation: 'H' | 'V' | 'N';
  minX: number;
  maxX: number;
  minY: number;
  maxY: number;
};
//=============================================================================
type ConnWithPoints = {
  points?: Point[];
  from?: string | number;
  to?: string | number;
  fromIndex?: number;
  toIndex?: number;
  key?: string;
  __ref?: any;
};
//=============================================================================
type BlockPort = { index: number; x: number; y: number; type?: string };
type Block = {
  id: string;
  x: number;
  y: number;
  width: number;
  height: number;
  rotation?: number;
  ports: BlockPort[];
};
//=============================================================================
type RouterState = { blocks: Map<string, Block>; connections: ConnWithPoints[] };
type RouterNode = { id: string; x: number; y: number; dir?: number | string };
type RouterResult = { wires: Map<string, { points?: Point[] }> };
type Entry = {
  conn: ConnWithPoints;
  idx: number;
  points: Point[];
  segments: Segment[];
};
//=============================================================================
type FirstTurnInfo = {
  initDir: 'H' | 'V';
  turnIndex: number;
  turn: Point;
  isSimpleL: boolean;
  forwardSign: number;
};
/**
 * Decompose a polyline into orthogonal segments (or mark as 'N' for diagonal).
 */
export function buildOrthoSegments(points: Point[]) {
  const out: Segment[] = [];
  if (!Array.isArray(points)) return out;
  for (let i = 0; i + 1 < points.length; i += 1) {
    const p = points[i];
    const q = points[i + 1];
    if (!p || !q) continue;
    if (p.x === q.x && p.y === q.y) continue;
    const orient = p.x === q.x ? 'V' : p.y === q.y ? 'H' : 'N';
    out.push({
      a: p,
      b: q,
      orientation: orient,
      minX: Math.min(p.x, q.x),
      maxX: Math.max(p.x, q.x),
      minY: Math.min(p.y, q.y),
      maxY: Math.max(p.y, q.y),
    });
  }
  return out;
}
/**
 * Report self-crossings, parallel overlaps and non-orthogonal segments of a set
 * of connections, with per-kind totals. Connections that share a source or
 * target port are not compared unless `ignoreSharedPorts` is false.
 */
export function analyzeConnectionGeometry(
  connections: ConnWithPoints[] = [],
  options: { ignoreSharedPorts?: boolean } = {}
) {
  const skipShared = options.ignoreSharedPorts !== false;

  const records: Entry[] = (connections || [])
    .map((c: ConnWithPoints, i: number) => ({
      conn: c,
      idx: i,
      points: Array.isArray(c?.points) ? (c.points as Point[]) : ([] as Point[]),
      segments: buildOrthoSegments(
        Array.isArray(c?.points) ? (c.points as Point[]) : ([] as Point[])
      ),
    }))
    .filter((r) => r.points.length >= 2 && r.segments.length > 0);

  const report = {
    selfCrosses: [] as Array<{ index: number; count: number }>,
    overlaps: [] as Array<{ a: number; b: number; count: number }>,
    nonOrthogonal: [] as number[],
  };

  for (const rec of records) {
    if (rec.segments.some((s) => s.orientation === 'N')) report.nonOrthogonal.push(rec.idx);
    const sc = countSelfCrossings(rec.segments);
    if (sc > 0) report.selfCrosses.push({ index: rec.idx, count: sc });
  }

  for (let i = 0; i < records.length; i += 1) {
    for (let j = i + 1; j < records.length; j += 1) {
      const A = records[i];
      const B = records[j];
      if (skipShared) {
        const aFrom = A.conn.fromIndex ?? 0;
        const aTo = A.conn.toIndex ?? 0;
        const bFrom = B.conn.fromIndex ?? 0;
        const bTo = B.conn.toIndex ?? 0;
        const sharedFrom = A.conn.from === B.conn.from && aFrom === bFrom;
        const sharedTo = A.conn.to === B.conn.to && aTo === bTo;
        if (sharedFrom || sharedTo) continue;
      }
      const ov = countParallelOverlaps(A.segments as Segment[], B.segments as Segment[]);
      if (ov > 0) report.overlaps.push({ a: A.idx, b: B.idx, count: ov });
    }
  }

  const totals = {
    selfCrosses: report.selfCrosses.reduce((s, it) => s + it.count, 0),
    overlaps: report.overlaps.reduce((s, it) => s + it.count, 0),
    nonOrthogonal: report.nonOrthogonal.length,
  };
  return { issues: report, totals };
}
/** Geometry totals plus turn and length statistics of the routed wires, keyed by connection key. */
export function analyzeResultWireQuality(
  connections: ConnWithPoints[] = [],
  wireMap: Map<string, { points?: Point[] }> | Record<string, { points?: Point[] }> | null = null
) {
  const connsWithPoints = (connections || []).map((conn: ConnWithPoints, idx: number) => {
    const wire =
      (wireMap as any)?.get?.(conn.key as string) ??
      (wireMap && (wireMap as Record<string, { points?: Point[] }>)[conn.key as string]);
    return {
      from: conn.from,
      to: conn.to,
      fromIndex: conn.fromIndex ?? 0,
      toIndex: conn.toIndex ?? 0,
      points: Array.isArray((wire as any)?.points)
        ? ((wire as any).points as Point[])
        : ([] as Point[]),
      __idx: idx,
    };
  });
  const analyzed = analyzeConnectionGeometry(connsWithPoints, { ignoreSharedPorts: true });
  const turns = connsWithPoints.map((conn: { points: Point[] }) => countTurns(conn.points));
  const length = connsWithPoints.map((conn: { points: Point[] }) => pathLength(conn.points));
  return {
    ...analyzed.totals,
    maxTurns: turns.length ? Math.max(...turns) : 0,
    avgTurns: turns.length ? turns.reduce((a: number, b: number) => a + b, 0) / turns.length : 0,
    avgLength: length.length
      ? length.reduce((a: number, b: number) => a + b, 0) / length.length
      : 0,
  };
}
//=============================================================================
export function countSelfCrossings(segments: Segment[]) {
  let tally = 0;
  const n = segments.length;
  for (let a = 0; a < n; a += 1) {
    const sA = segments[a];
    if (sA.orientation === 'N') continue;
    for (let b = a + 2; b < n; b += 1) {
      if (a === 0 && b === n - 1) continue;
      const sB = segments[b];
      if (sB.orientation === 'N' || sA.orientation === sB.orientation) continue;
      const horiz = sA.orientation === 'H' ? sA : sB;
      const vert = sA.orientation === 'V' ? sA : sB;
      const ix = vert.a.x;
      const iy = horiz.a.y;
      if (ix > horiz.minX && ix < horiz.maxX && iy > vert.minY && iy < vert.maxY) tally += 1;
    }
  }
  return tally;
}
//=============================================================================
export function countParallelOverlaps(segmentsA: Segment[], segmentsB: Segment[]) {
  let tally = 0;
  for (const sA of segmentsA) {
    if (sA.orientation === 'N') continue;
    for (const sB of segmentsB) {
      if (sB.orientation === 'N') continue;
      if (sA.orientation !== sB.orientation) continue;
      if (sA.orientation === 'H') {
        if (sA.a.y !== sB.a.y) continue;
        const overlap = Math.min(sA.maxX, sB.maxX) - Math.max(sA.minX, sB.minX);
        if (overlap > 0) tally += 1;
      } else {
        if (sA.a.x !== sB.a.x) continue;
        const overlap = Math.min(sA.maxY, sB.maxY) - Math.max(sA.minY, sB.minY);
        if (overlap > 0) tally += 1;
      }
    }
  }
  return tally;
}
//=============================================================================
export function countTurns(points: Point[]) {
  if (!Array.isArray(points) || points.length < 3) return 0;
  let prev: 'H' | 'V' | null = null;
  let cnt = 0;
  for (let i = 0; i + 1 < points.length; i += 1) {
    const a = points[i];
    const b = points[i + 1];
    const d = a.x === b.x ? 'V' : a.y === b.y ? 'H' : null;
    if (!d) continue;
    if (prev && prev !== d) cnt += 1;
    prev = d;
  }
  return cnt;
}
//=============================================================================
export function pathLength(points: Point[]) {
  if (!Array.isArray(points) || points.length < 2) return 0;
  let acc = 0;
  for (let i = 0; i + 1 < points.length; i += 1) {
    acc += Math.abs(points[i + 1].x - points[i].x) + Math.abs(points[i + 1].y - points[i].y);
  }
  return acc;
}
//=============================================================================
export function densifyGridPath(points: Point[]) {
  if (!Array.isArray(points) || points.length < 2) return points || [];
  const out: Point[] = [];
  const markPin = (src: Point) => {
    // Pin flags ride along on the densified copy of their original point.
    if ((src as any).pin && out.length) (out[out.length - 1] as any).pin = true;
  };
  for (let i = 0; i + 1 < points.length; i += 1) {
    const a = points[i];
    const b = points[i + 1];
    if (!a || !b) continue;
    if (out.length === 0) {
      out.push({ x: a.x, y: a.y });
      markPin(a);
    }
    const sx = Math.sign(b.x - a.x);
    const sy = Math.sign(b.y - a.y);
    const steps = Math.max(Math.abs(b.x - a.x), Math.abs(b.y - a.y));
    if (sx !== 0 && sy !== 0) {
      const last = out[out.length - 1];
      if (last.x !== b.x || last.y !== b.y) out.push({ x: b.x, y: b.y });
      markPin(b);
      continue;
    }
    let cx = a.x;
    let cy = a.y;
    for (let s = 0; s < steps; s += 1) {
      cx += sx;
      cy += sy;
      out.push({ x: cx, y: cy });
    }
    markPin(b);
  }
  return out;
}
//=============================================================================
export function segmentDir(a?: Point | null, b?: Point | null): 'H' | 'V' | null {
  if (!a || !b) return null;
  if (a.x === b.x && a.y !== b.y) return 'V';
  if (a.y === b.y && a.x !== b.x) return 'H';
  return null;
}
//=============================================================================
export function firstTurnInfo(points: Point[]): FirstTurnInfo | null {
  if (!Array.isArray(points) || points.length < 4) return null;
  const base = segmentDir(points[0], points[1]);
  if (!base) return null;
  let idx = -1;
  for (let i = 1; i + 1 < points.length; i += 1) {
    const s = segmentDir(points[i], points[i + 1]);
    if (!s) continue;
    if (s !== base) {
      idx = i;
      break;
    }
  }
  if (idx < 1 || idx + 1 >= points.length) return null;
  const initDir = base;
  const turn = points[idx];
  const post = segmentDir(points[idx], points[idx + 1]);
  const isSimpleL = Boolean(post && post !== base && idx === 1);
  const forwardSign =
    initDir === 'H'
      ? Math.sign(points[1].x - points[0].x) || 1
      : Math.sign(points[1].y - points[0].y) || 1;
  return { initDir, turnIndex: idx, turn, isSimpleL, forwardSign };
}
//=============================================================================
export function minTurnDistanceToEndpointCells(points: Point[], endpointIdx: number) {
  if (!Array.isArray(points) || points.length < 3) return Number.POSITIVE_INFINITY;
  const ep = points[endpointIdx];
  if (!ep) return Number.POSITIVE_INFINITY;
  let best = Number.POSITIVE_INFINITY;
  for (let i = 1; i + 1 < points.length; i += 1) {
    const a = segmentDir(points[i - 1], points[i]);
    const b = segmentDir(points[i], points[i + 1]);
    if (!a || !b || a === b) continue;
    const dpx = Math.abs(points[i].x - ep.x) + Math.abs(points[i].y - ep.y);
    const cells = dpx / GRID_SIZE;
    if (cells < best) best = cells;
  }
  return best;
}
//=============================================================================
export function canAdjustSharedTurn(
  points: Point[],
  info: FirstTurnInfo | null | undefined,
  axis: 'x' | 'y',
  targetValue: number,
  preferredStubCells = 2
) {
  if (!Array.isArray(points) || !info) return false;
  const a = info.turnIndex;
  const b = info.turnIndex + 1;
  if (a < 0 || b >= points.length) return false;
  const copy = points.map((pt) => ({ x: pt.x, y: pt.y }));
  copy[a][axis] = targetValue as number;
  copy[b][axis] = targetValue as number;
  const curS = minTurnDistanceToEndpointCells(points, 0);
  const curE = minTurnDistanceToEndpointCells(points, points.length - 1);
  const nxtS = minTurnDistanceToEndpointCells(copy, 0);
  const nxtE = minTurnDistanceToEndpointCells(copy, copy.length - 1);
  if (!Number.isFinite(nxtS) || !Number.isFinite(nxtE)) return false;
  if (nxtS < preferredStubCells && curS >= preferredStubCells) return false;
  if (nxtE < preferredStubCells && curE >= preferredStubCells) return false;
  return true;
}
//=============================================================================
export function wouldCreateNonSharedOverlap(
  conn: ConnWithPoints,
  proposedPoints: Point[],
  connections: ConnWithPoints[]
) {
  const proposedSegs = buildOrthoSegments(proposedPoints);
  if (!proposedSegs.length) return false;
  const fIdx = conn.fromIndex ?? 0;
  const tIdx = conn.toIndex ?? 0;
  for (const other of connections) {
    if (!other || other === conn) continue;
    const of = other.fromIndex ?? 0;
    const ot = other.toIndex ?? 0;
    const sharedFrom = other.from === conn.from && of === fIdx;
    const sharedTo = other.to === conn.to && ot === tIdx;
    if (sharedFrom || sharedTo) continue;
    const otherSegs = buildOrthoSegments(other.points || []);
    if (!otherSegs.length) continue;
    if (countParallelOverlaps(proposedSegs, otherSegs) > 0) return true;
  }
  return false;
}
//=============================================================================
export function buildStubPoint(portPos: Point, side: string): Point {
  if (side === 'left') return { x: portPos.x - GRID_SIZE, y: portPos.y };
  if (side === 'right') return { x: portPos.x + GRID_SIZE, y: portPos.y };
  if (side === 'top') return { x: portPos.x, y: portPos.y - GRID_SIZE };
  return { x: portPos.x, y: portPos.y + GRID_SIZE };
}
//=============================================================================
export function isValidStub(port: Point, other: Point, side: string): boolean {
  if (side === 'left') return other.y === port.y && other.x <= port.x - GRID_SIZE;
  if (side === 'right') return other.y === port.y && other.x >= port.x + GRID_SIZE;
  if (side === 'top') return other.x === port.x && other.y <= port.y - GRID_SIZE;
  return other.x === port.x && other.y >= port.y + GRID_SIZE;
}
//=============================================================================
export function dedupePoints(points: Point[]) {
  if (!Array.isArray(points) || points.length === 0) return points;
  const res: Point[] = [];
  for (const p of points) {
    const last = res[res.length - 1];
    if (!last || last.x !== p.x || last.y !== p.y) res.push(p);
    // A dropped duplicate donates its pin flag to the kept point.
    else if ((p as any).pin) (last as any).pin = true;
  }
  return res;
}
//=============================================================================
function orthIntersect(a: Point, b: Point, c: Point, d: Point): Point | null {
  const aH = a.y === b.y;
  const cH = c.y === d.y;
  if (aH === cH) return null;
  const horiz = aH ? { a, b } : { a: c, b: d };
  const vert = aH ? { a: c, b: d } : { a, b };
  const hx1 = Math.min(horiz.a.x, horiz.b.x);
  const hx2 = Math.max(horiz.a.x, horiz.b.x);
  const vy1 = Math.min(vert.a.y, vert.b.y);
  const vy2 = Math.max(vert.a.y, vert.b.y);
  const ix = vert.a.x;
  const iy = horiz.a.y;
  if (ix > hx1 && ix < hx2 && iy > vy1 && iy < vy2) return { x: ix, y: iy };
  return null;
}
//=============================================================================
export function removeColinearPoints(points: Point[]) {
  if (!Array.isArray(points) || points.length < 3) return points;
  const out: Point[] = [points[0]];
  for (let i = 1; i + 1 < points.length; i += 1) {
    const prev = out[out.length - 1];
    const curr = points[i];
    const next = points[i + 1];
    // Pinned (user-anchored) points are never merged away.
    if (!(curr as any).pin) {
      if ((prev.x === curr.x && curr.x === next.x) || (prev.y === curr.y && curr.y === next.y))
        continue;
    }
    out.push(curr);
  }
  out.push(points[points.length - 1]);
  return out;
}
//=============================================================================
export function simplifyOrthogonalPath(points: Point[]) {
  if (!Array.isArray(points) || points.length === 0) return points;
  if (points.length === 1) return points;
  // A pinned wire keeps its user-authored shape: only dedupe + colinear
  // merge (both pin-preserving), no L-fix, no loop removal.
  if (points.some((p) => (p as any).pin)) {
    return removeColinearPoints(dedupePoints(points));
  }
  let seq = removeColinearPoints(dedupePoints(points));

  if (seq.length === 2) {
    const [s, e] = seq;
    if (s.x !== e.x && s.y !== e.y) seq = [s, { x: e.x, y: s.y }, e];
  }

  let mutated = true;
  while (mutated && seq.length >= 4) {
    mutated = false;
    outer: for (let i = 1; i < seq.length - 1; i += 1) {
      for (let j = 0; j < i - 1; j += 1) {
        const hit = orthIntersect(seq[j], seq[j + 1], seq[i], seq[i + 1]);
        if (!hit) continue;
        const head = seq.slice(0, j + 1);
        const lastHead = head[head.length - 1];
        if (lastHead.x !== hit.x || lastHead.y !== hit.y) head.push(hit);
        let tail = seq.slice(i + 1);
        if (
          tail.length &&
          tail[0].x === head[head.length - 1].x &&
          tail[0].y === head[head.length - 1].y
        )
          tail = tail.slice(1);
        seq = removeColinearPoints(dedupePoints([...head, ...tail]));
        mutated = true;
        break outer;
      }
    }
  }
  return seq;
}
export function pointsToPath(points: Point[]) {
  if (!Array.isArray(points) || points.length === 0) return '';
  const head = `M ${points[0].x} ${points[0].y}`;
  const tail = points
    .slice(1)
    .map((p) => `L ${p.x} ${p.y}`)
    .join(' ');
  return tail.length ? `${head} ${tail}` : head;
}
//=============================================================================
export function enforcePortStubs(state: RouterState, conn: ConnWithPoints, points: Point[]) {
  if (!Array.isArray(points) || points.length < 2) return points;
  const fromBlk = state.blocks.get(String(conn.from));
  const toBlk = state.blocks.get(String(conn.to));
  if (!fromBlk || !toBlk) return points;
  const fi = conn.fromIndex ?? 0;
  const ti = conn.toIndex ?? 0;
  const fromP = fromBlk.ports.find((p) => p.type === 'out' && p.index === fi);
  const toP = toBlk.ports.find((p) => p.type === 'in' && p.index === ti);
  if (!fromP || !toP) return points;

  const fromAbs = rotatePoint({ x: fromBlk.x + fromP.x, y: fromBlk.y + fromP.y }, fromBlk as any);
  const toAbs = rotatePoint({ x: toBlk.x + toP.x, y: toBlk.y + toP.y }, toBlk as any);
  const fromG = alignToGrid(fromAbs);
  const toG = alignToGrid(toAbs);
  const fromSide = getPortSide(fromBlk as any, fromAbs);
  const toSide = getPortSide(toBlk as any, toAbs);

  const seq = points.slice();
  if (seq[0].x !== fromG.x || seq[0].y !== fromG.y) seq.unshift(fromG);
  const second = seq[1];
  const startStub = buildStubPoint(fromG, fromSide);
  if (!isValidStub(fromG, second, fromSide)) {
    seq.splice(1, 0, startStub);
    const after = seq[2];
    if (after && (after.x !== startStub.x || after.y !== startStub.y)) {
      if (fromSide === 'left' || fromSide === 'right')
        seq.splice(2, 0, { x: startStub.x, y: after.y });
      else seq.splice(2, 0, { x: after.x, y: startStub.y });
    }
  }

  if (seq[seq.length - 1].x !== toG.x || seq[seq.length - 1].y !== toG.y) seq.push(toG);
  const prev = seq[seq.length - 2];
  const endStub = buildStubPoint(toG, toSide);
  if (!isValidStub(toG, prev, toSide)) {
    if (prev && (prev.x !== endStub.x || prev.y !== endStub.y)) {
      if (toSide === 'left' || toSide === 'right')
        seq.splice(seq.length - 1, 0, { x: endStub.x, y: prev.y });
      else seq.splice(seq.length - 1, 0, { x: prev.x, y: endStub.y });
    }
    seq.splice(seq.length - 1, 0, endStub);
  }

  return simplifyOrthogonalPath(dedupePoints(seq));
}
/** Build the `computeWireRoutes` input (nodes, connections, obstacles, prevSolution) from `state`. */
export function buildRouter2Input(state: RouterState, dirtySet: Set<any> | null = null) {
  const nodes: RouterNode[] = [];
  const index = new Map<string, RouterNode>();
  const connsOut: { from: string; to: string; key: string; __ref?: any }[] = [];
  const prev: Record<string, Point[]> = {};
  const obs: any[] = [];

  for (const conn of state.connections) {
    const src = ensurePortNode(
      state,
      conn.from as string | number,
      'out',
      conn.fromIndex ?? 0,
      index,
      nodes
    );
    const dst = ensurePortNode(
      state,
      conn.to as string | number,
      'in',
      conn.toIndex ?? 0,
      index,
      nodes
    );
    if (!src || !dst) continue;
    const key = `${conn.from}:${conn.fromIndex ?? 0}->${conn.to}:${conn.toIndex ?? 0}`;
    connsOut.push({ from: src.id, to: dst.id, key, __ref: conn });
    // Previous routes are given for ALL connections: non-dirty wires become
    // static occupancy, dirty ones use theirs for route hysteresis. Dirtiness is
    // decided by computeWireRoutes, not by prev presence.
    void dirtySet;
    if (Array.isArray(conn.points) && conn.points.length > 1) {
      const gridPoints = conn.points.map((pt) => {
        const gp: Point & { pin?: boolean } = {
          x: Math.round(pt.x / GRID_SIZE),
          y: Math.round(pt.y / GRID_SIZE),
        };
        // Pin flags survive the px→grid round trip so pinned wires are
        // recognized as static by the router.
        if ((pt as any).pin) gp.pin = true;
        return gp;
      });
      prev[key] = densifyGridPath(gridPoints);
    }
  }

  // Sort blocks by id so obstacle order (and thus obstacle-owner overwrites)
  // is deterministic and identical to the native router (RouterEngine.cpp).
  const sortedBlocks = [...state.blocks.values()].sort((a, b) =>
    String(a.id) < String(b.id) ? -1 : String(a.id) > String(b.id) ? 1 : 0
  );
  for (const b of sortedBlocks) obs.push(...blockToObstacles(b));

  const prevSolution = Object.keys(prev).length ? { wires: prev } : null;
  return { nodes, connections: connsOut, obstacles: obs, prevSolution };
}
/**
 * Write the router result (grid points) back into `state` connections in
 * application coordinates. Returns a Map from connection to its SVG path.
 */
export function applyRouter2Result(
  state: RouterState,
  connections: { key: string; __ref?: any }[],
  result: RouterResult
) {
  const out = new Map<any, string>();
  const changed: any[] = [];
  for (const c of connections) {
    const wire = result.wires.get(c.key);
    if (!wire || !Array.isArray(wire.points) || wire.points.length === 0) {
      if (c.__ref && Array.isArray(c.__ref.points) && c.__ref.points.length) changed.push(c.__ref);
      else if (c.__ref) {
        c.__ref.points = [];
        changed.push(c.__ref);
      }
      continue;
    }
    let pts = wire.points.map((p) => {
      const px: Point & { pin?: boolean } = { x: p.x * GRID_SIZE, y: p.y * GRID_SIZE };
      if ((p as any).pin) px.pin = true;
      return px;
    });
    pts = enforcePortStubs(state, c.__ref, pts);
    if (c.__ref) c.__ref.points = pts;
    changed.push(c.__ref);
  }

  normalizeSharedSourceJunctions(changed as ConnWithPoints[]);
  // Spread exact channel overlaps one grid cell apart. Shared-source prefixes
  // and the branches of one physical net overlap by design (grouped by net key).
  const netKeys = physicalNetKeys((state.connections as any[]) || [], state.blocks as any);
  nudgeConnections(
    (state.connections as any[]) || [],
    (state.blocks && typeof state.blocks.values === 'function' ? state.blocks.values() : []) as any,
    { netKeys }
  );
  // Siblings leaving or entering one block turn in an order that avoids
  // crossings (columns are only permuted).
  orderFanOutTurns(
    (state.connections as any[]) || [],
    (state.blocks && typeof state.blocks.values === 'function' ? state.blocks.values() : []) as any
  );
  for (const conn of changed) {
    const pts = Array.isArray(conn.points) ? conn.points : [];
    out.set(conn, pts.length ? pointsToPath(pts) : '');
  }
  return out;
}
//=============================================================================
function ensurePortNode(
  state: RouterState,
  blockId: string | number,
  type: string,
  index: number,
  nodeMap: Map<string, RouterNode>,
  nodes: RouterNode[]
) {
  const block = state.blocks.get(String(blockId));
  if (!block) return null;
  const port = block.ports.find((p) => p.type === type && p.index === index);
  if (!port) return null;
  const key = `${blockId}:${type}:${index}`;
  if (nodeMap.has(key)) return nodeMap.get(key) ?? null;
  const raw = rotatePoint({ x: block.x + port.x, y: block.y + port.y }, block as any);
  const pos = alignToGrid(raw);
  const side = getPortSide(block as any, raw);
  const dir = sideToDir(side);
  const node: RouterNode = {
    id: key,
    x: Math.round(pos.x / GRID_SIZE),
    y: Math.round(pos.y / GRID_SIZE),
    dir,
  };
  nodeMap.set(key, node);
  nodes.push(node);
  return node;
}
/** Block id of a node id (legacy keys are `block:side:index`). */
export function blockIdFromNodeId(nodeId: string | number) {
  return typeof nodeId === 'string' ? nodeId.split(':')[0] : String(nodeId);
}
/** Obstacle rectangles (grid coordinates) covering the block body and its ports. */
export function blockToObstacles(block: Block) {
  const PORT_RADIUS = 6;
  const pad = 0;
  const b = getRotatedBounds(block);
  let l = b.left;
  let r = b.right;
  let t = b.top;
  let bt = b.bottom;
  for (const p of block.ports) {
    const pos = rotatePoint({ x: block.x + p.x, y: block.y + p.y }, block);
    l = Math.min(l, pos.x - PORT_RADIUS);
    r = Math.max(r, pos.x + PORT_RADIUS);
    t = Math.min(t, pos.y - PORT_RADIUS);
    bt = Math.max(bt, pos.y + PORT_RADIUS);
  }
  l -= pad;
  r += pad;
  t -= pad;
  bt += pad;
  return [
    {
      x0: Math.floor(l / GRID_SIZE),
      y0: Math.floor(t / GRID_SIZE),
      x1: Math.floor(r / GRID_SIZE),
      y1: Math.floor(bt / GRID_SIZE),
      owner: block.id,
    },
  ];
}
//=============================================================================
function normalizeSharedSourceJunctions(connections: any[]) {
  if (!Array.isArray(connections) || connections.length < 2) return;
  const preferredStubCells = 2;
  const groups = new Map<string, any[]>();
  for (const c of connections) {
    // Wires with pinned (user-anchored) points keep their shape verbatim.
    if (Array.isArray(c?.points) && c.points.some((p: any) => p?.pin)) continue;
    const k = `${c.from}:${c.fromIndex ?? 0}`;
    if (!groups.has(k)) groups.set(k, []);
    groups.get(k)!.push(c);
  }

  for (const group of groups.values()) {
    if (!group || group.length < 2) continue;
    const infos = group
      .map((g: any) => ({ conn: g, info: firstTurnInfo(g.points) }))
      .filter((x: any) => x.info && x.info.isSimpleL);
    if (infos.length < 2) continue;
    const base = infos[0];
    if (!base || !base.info) continue;
    const dir = base.info.initDir;
    if (!infos.every((it: any) => it.info && it.info.initDir === dir)) continue;

    const turns = infos.map((it: any) => it.info.turn);
    if (dir === 'H') {
      const yRef = turns[0].y;
      if (!turns.every((p: any) => p.y === yRef)) continue;
      const xs = turns.map((p: any) => p.x);
      const minX = Math.min(...xs);
      const maxX = Math.max(...xs);
      if (maxX - minX > GRID_SIZE * 4) continue;
      const targetX = base.info.forwardSign > 0 ? maxX : minX;
      for (const { conn, info } of infos) {
        if (!info) continue;
        if (info.turn.x === targetX) continue;
        if (!canAdjustSharedTurn(conn.points, info, 'x', targetX, preferredStubCells)) continue;
        const proposal = conn.points.map((p: any) => ({ x: p.x, y: p.y }));
        proposal[info.turnIndex].x = targetX;
        proposal[info.turnIndex + 1].x = targetX;
        if (wouldCreateNonSharedOverlap(conn, proposal, connections)) continue;
        conn.points[info.turnIndex].x = targetX;
        conn.points[info.turnIndex + 1].x = targetX;
      }
    } else if (dir === 'V') {
      const xRef = turns[0].x;
      if (!turns.every((p: any) => p.x === xRef)) continue;
      const ys = turns.map((p: any) => p.y);
      const minY = Math.min(...ys);
      const maxY = Math.max(...ys);
      if (maxY - minY > GRID_SIZE * 4) continue;
      const targetY = base.info.forwardSign > 0 ? maxY : minY;
      for (const { conn, info } of infos) {
        if (!info) continue;
        if (info.turn.y === targetY) continue;
        if (!canAdjustSharedTurn(conn.points, info, 'y', targetY, preferredStubCells)) continue;
        const proposal = conn.points.map((p: any) => ({ x: p.x, y: p.y }));
        proposal[info.turnIndex].y = targetY;
        proposal[info.turnIndex + 1].y = targetY;
        if (wouldCreateNonSharedOverlap(conn, proposal, connections)) continue;
        conn.points[info.turnIndex].y = targetY;
        conn.points[info.turnIndex + 1].y = targetY;
      }
    }
  }
}
//=============================================================================
// Compatibility alias for legacy consumers
export { normalizeSharedSourceJunctions as normalizeConnectionJunctions };
//=============================================================================
