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
// Fan ordering pass.
//
// Z-shaped wires leaving (or entering) one block each turn in their own column.
// The router and the nudge pass pick those columns by proximity, so a wire can
// turn in front of a sibling whose first run it then has to cross. This pass runs
// on final geometry and only permutes the turn columns within a fan, so that no
// member turns inside a span another member still has to cross. The set of
// columns is unchanged, so hysteresis and grid alignment are kept. A group move
// is validated (stubs, block crossings, overlap) and reverted as a whole if invalid.
//=============================================================================
import { GRID_SIZE } from '../utils/geometry.js';
import { blockRect, segmentCrossesRect } from './nudge.js';
import type { BlockLike, Rect } from './nudge.js';
//=============================================================================
type Point = { x: number; y: number };
type ConnLike = {
  from?: string | number;
  to?: string | number;
  fromIndex?: number;
  toIndex?: number;
  points?: Point[];
};
//=============================================================================
type Axis = 'x' | 'y';
/** One Z wire: p0 -(along a)- p1 -(along b)- p2 -(along a)- p3. */
type Fan = {
  conn: ConnLike;
  a: Axis; // travel axis of the first and last runs
  b: Axis; // axis of the turn (perpendicular run)
  dir: 1 | -1; // travel direction along `a`
  start: number; // p0[b]
  end: number; // p3[b]
  lo: number;
  hi: number;
  col: number; // p1[a] (the turn column)
  from: number; // p0[a]
  to: number; // p3[a]
};
//=============================================================================
const MAX_EXHAUSTIVE = 7;
/** Drop repeated and collinear interior points (routes may carry grid steps). */
function compact(points: Point[]): Point[] {
  const out: Point[] = [];
  for (const p of points) {
    const last = out[out.length - 1];
    if (last && last.x === p.x && last.y === p.y) continue;
    const prev = out[out.length - 2];
    if (
      last &&
      prev &&
      ((prev.x === last.x && last.x === p.x) || (prev.y === last.y && last.y === p.y))
    ) {
      out[out.length - 1] = p;
      continue;
    }
    out.push(p);
  }
  return out;
}
//=============================================================================
function zShape(conn: ConnLike): Fan | null {
  if (!Array.isArray(conn.points) || conn.points.length < 4) return null;
  if (conn.points.some((p) => (p as { pin?: boolean }).pin)) return null;
  const pts = compact(conn.points);
  if (pts.length !== 4) return null;
  const [p0, p1, p2, p3] = pts;
  let a: Axis;
  let b: Axis;
  if (p0.y === p1.y && p1.x === p2.x && p2.y === p3.y && p0.x !== p1.x) {
    a = 'x';
    b = 'y';
  } else if (p0.x === p1.x && p1.y === p2.y && p2.x === p3.x && p0.y !== p1.y) {
    a = 'y';
    b = 'x';
  } else return null;
  const d1 = Math.sign(p1[a] - p0[a]);
  const d2 = Math.sign(p3[a] - p2[a]);
  if (d1 === 0 || d1 !== d2) return null;
  if (p1[b] === p2[b]) return null; // straight line, nothing to order
  conn.points = pts;
  return {
    conn,
    a,
    b,
    dir: d1 as 1 | -1,
    start: p0[b],
    end: p3[b],
    lo: Math.min(p0[b], p3[b]),
    hi: Math.max(p0[b], p3[b]),
    col: p1[a],
    from: p0[a],
    to: p3[a],
  };
}
//=============================================================================
/** True when `first` (turning at the earlier column) crosses `second`. */
function crosses(first: Fan, second: Fan): boolean {
  const d = first.dir;
  // Sharing a column is only fine when the spans are disjoint.
  if (first.col === second.col)
    return Math.min(first.hi, second.hi) - Math.max(first.lo, second.lo) > 0;
  // `first` turns inside the first run of `second`.
  const firstTurnsInSecondRun =
    first.lo < second.start && second.start < first.hi && first.col * d > second.from * d;
  // `second` turns inside the last run of `first`.
  const secondTurnsInFirstRun =
    second.lo < first.end && first.end < second.hi && second.col * d < first.to * d;
  return firstTurnsInSecondRun || secondTurnsInFirstRun;
}
//=============================================================================
/** Crossings of a fan once its members turn at `cols` (in order). */
function costOf(order: Fan[], cols: number[]): number {
  const saved = order.map((f) => f.col);
  order.forEach((f, k) => {
    f.col = cols[k];
  });
  let cost = 0;
  for (let i = 0; i < order.length; i += 1)
    for (let j = i + 1; j < order.length; j += 1) if (crosses(order[i], order[j])) cost += 1;
  order.forEach((f, k) => {
    f.col = saved[k];
  });
  return cost;
}
//=============================================================================
/** Best assignment of `cols` to the fan: exhaustive for small fans. */
function bestOrder(fans: Fan[], cols: number[]): Fan[] {
  if (fans.length > MAX_EXHAUSTIVE) {
    return fans
      .slice()
      .sort((p, q) => (crosses(p, q) ? -1 : crosses(q, p) ? 1 : p.col * p.dir - q.col * q.dir));
  }
  let best = fans;
  let bestCost = costOf(fans, cols);
  const permute = (prefix: Fan[], rest: Fan[]) => {
    if (bestCost === 0) return;
    if (!rest.length) {
      const c = costOf(prefix, cols);
      if (c < bestCost) {
        bestCost = c;
        best = prefix;
      }
      return;
    }
    for (let i = 0; i < rest.length; i += 1) {
      permute(prefix.concat(rest[i]), rest.slice(0, i).concat(rest.slice(i + 1)));
    }
  };
  permute([], fans);
  return best;
}
//=============================================================================
function runsCrossBlocks(fan: Fan, rects: Array<{ id: string; rect: Rect }>): boolean {
  const pts = fan.conn.points as Point[];
  const own = new Set([String(fan.conn.from), String(fan.conn.to)]);
  for (let i = 0; i + 1 < pts.length; i += 1) {
    const p = pts[i];
    const q = pts[i + 1];
    const orient = p.y === q.y ? 'H' : 'V';
    const track = orient === 'H' ? p.y : p.x;
    const lo = orient === 'H' ? Math.min(p.x, q.x) : Math.min(p.y, q.y);
    const hi = orient === 'H' ? Math.max(p.x, q.x) : Math.max(p.y, q.y);
    if (rects.some((r) => !own.has(r.id) && segmentCrossesRect(orient, track, lo, hi, r.rect)))
      return true;
  }
  return false;
}
//=============================================================================
/** Another wire (sibling or foreign) already runs along this column over the same span. */
function overlapsOther(fan: Fan, connections: ConnLike[]): boolean {
  for (const other of connections) {
    if (other === fan.conn) continue;
    const pts = other.points;
    if (!Array.isArray(pts)) continue;
    for (let i = 0; i + 1 < pts.length; i += 1) {
      const p = pts[i];
      const q = pts[i + 1];
      if (p[fan.a] !== fan.col || q[fan.a] !== fan.col) continue;
      const lo = Math.min(p[fan.b], q[fan.b]);
      const hi = Math.max(p[fan.b], q[fan.b]);
      if (Math.min(hi, fan.hi) - Math.max(lo, fan.lo) > 0) return true;
    }
  }
  return false;
}
//=============================================================================
function applyColumns(group: Fan[], cols: number[]): () => void {
  const undo = group.map((fan) => {
    const pts = fan.conn.points as Point[];
    const old = fan.col;
    return () => {
      pts[1][fan.a] = old;
      pts[2][fan.a] = old;
      fan.col = old;
    };
  });
  group.forEach((fan, k) => {
    const pts = fan.conn.points as Point[];
    pts[1][fan.a] = cols[k];
    pts[2][fan.a] = cols[k];
    fan.col = cols[k];
  });
  return () => undo.forEach((u) => u());
}
//=============================================================================
function orderGroup(
  group: Fan[],
  rects: Array<{ id: string; rect: Rect }>,
  connections: ConnLike[]
): boolean {
  if (group.length < 2) return false;
  const dir = group[0].dir;
  // Columns in turn order; members already sorted the same way keep their cost.
  const current = group.slice().sort((p, q) => p.col * p.dir - q.col * q.dir);
  const cols = current.map((f) => f.col);
  if (costOf(current, cols) === 0) return false;
  const order = bestOrder(group, cols);
  if (order.every((fan, k) => fan.col === cols[k])) return false;
  const undo = applyColumns(order, cols);
  const ok = order.every(
    (fan) =>
      (fan.col - fan.from) * dir >= GRID_SIZE &&
      (fan.to - fan.col) * dir >= GRID_SIZE &&
      !runsCrossBlocks(fan, rects) &&
      !overlapsOther(fan, connections)
  );
  if (!ok) undo();
  return ok;
}
/**
 * Permute the turn columns of every fan of Z wires (same source block, then
 * same target block) so that siblings do not cross each other. Mutates
 * `conn.points` in place; returns the number of fans changed.
 */
export function orderFanOutTurns(
  connections: ConnLike[],
  blocks: Iterable<BlockLike> = []
): number {
  if (!Array.isArray(connections) || connections.length < 2) return 0;
  const rects: Array<{ id: string; rect: Rect }> = [];
  for (const b of blocks || []) {
    if (b && Number.isFinite(b.x) && Number.isFinite(b.y))
      rects.push({ id: String(b.id), rect: blockRect(b) });
  }
  let changed = 0;
  for (const side of ['from', 'to'] as const) {
    const groups = new Map<string, Fan[]>();
    for (const conn of connections) {
      const fan = zShape(conn);
      if (!fan) continue;
      const key = `${String(conn[side])}|${fan.a}|${fan.dir}`;
      const list = groups.get(key) || [];
      list.push(fan);
      groups.set(key, list);
    }
    for (const group of groups.values()) {
      // Shared-source siblings keep their common prefix: leave such fans alone.
      const ports = group.map((f) => `${f.conn.from}:${f.conn.fromIndex ?? 0}`);
      if (side === 'from' && new Set(ports).size !== ports.length) continue;
      if (orderGroup(group, rects, connections)) changed += 1;
    }
  }
  return changed;
}
//=============================================================================
