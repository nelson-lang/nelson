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
// Physical (acausal) net geometry.
//
// A physical wire joins two undirected 'phys' pins, and several wires can share
// a pin: together they form one NET (one electrical / mechanical / thermal node).
// The routers only resolve directed in/out ports, so physical wires reach the
// renderer on the generic fallback path, where the separation pass spreads the
// branches of one net apart like rival signals.
//
// This module draws a net like a hand-drawn schematic: ONE trunk (horizontal or
// vertical) with a perpendicular tap from every pin, so all branches share the
// trunk and meet at explicit junctions. Buses of different nets never share a
// track. The code is pure (no DOM). A net whose bus would cross a block, or that
// carries a hand-pinned wire, keeps its previous geometry.
//=============================================================================
import { GRID_SIZE, alignToGrid } from '../utils/geometry.js';
import { rotatePoint, getPortSide, getRotatedBounds } from '../utils/port-utils.js';
//=============================================================================
type Point = { x: number; y: number };
type PinnedPoint = Point & { pin?: boolean };
type Side = 'left' | 'right' | 'top' | 'bottom';
type Rect = { left: number; right: number; top: number; bottom: number };
type OwnedRect = { id: string; rect: Rect };
export type PhysicalPort = { type?: string; index?: number; x: number; y: number };
export type PhysicalBlock = {
  x: number;
  y: number;
  width: number;
  height: number;
  rotation?: number;
  ports?: PhysicalPort[];
};
//=============================================================================
export type PhysicalConn = {
  from?: string | number;
  to?: string | number;
  fromIndex?: number;
  toIndex?: number;
  points?: PinnedPoint[];
};
//=============================================================================
type Pin = { key: string; blockId: string; pos: Point; side: Side };
export type PhysicalNet = {
  key: string;
  conns: PhysicalConn[];
  pins: Pin[];
  pinsOf: Map<PhysicalConn, [Pin, Pin]>;
};
//=============================================================================
// Same port padding the separation pass uses for its block rectangles.
const PORT_RADIUS = 6;
// Two buses of different nets stay at least this far apart.
const BUS_SPACING = 2 * GRID_SIZE;
// Callers (apply choke points, renderer, headless specs) may hand over a state
// with no block map at all: that is simply a diagram with no physical net.
function mapLike(blocks: unknown): ReadonlyMap<unknown, unknown> | null {
  const m = blocks as ReadonlyMap<unknown, unknown> | null | undefined;
  return m && typeof m.get === 'function' && typeof m[Symbol.iterator] === 'function' ? m : null;
}
//=============================================================================
function blockOf(blocks: ReadonlyMap<unknown, unknown>, id: unknown): PhysicalBlock | null {
  if (id === undefined || id === null) return null;
  const b = blocks.get(id) ?? blocks.get(String(id)) ?? blocks.get(Number(id));
  return (b as PhysicalBlock) || null;
}
//=============================================================================
function portOf(block: PhysicalBlock, type: string, index: number): PhysicalPort | undefined {
  return (block.ports || []).find((p) => p.type === type && Number(p.index ?? -1) === index);
}
//=============================================================================
// Both endpoints are physical pins. A sensor carries a 'phys' pin AND an 'out'
// at the same index, so the index alone is ambiguous: the wire is a signal only
// when it runs from such an 'out' INTO an 'in' (sensor -> scope / workspace). A
// sensor's pin wired to another pin (HeatFlowSensor.a -> TemperatureSensor.port)
// is a physical branch like any other.
function purePhysicalPair(
  fromBlock: PhysicalBlock,
  toBlock: PhysicalBlock,
  fromIndex: number,
  toIndex: number
): boolean {
  if (!portOf(fromBlock, 'phys', fromIndex) || !portOf(toBlock, 'phys', toIndex)) return false;
  const looksLikeSignal = !!portOf(fromBlock, 'out', fromIndex) && !!portOf(toBlock, 'in', toIndex);
  return !looksLikeSignal;
}
//=============================================================================
function pinOf(block: PhysicalBlock, blockId: string, index: number): Pin {
  const port = portOf(block, 'phys', index) as PhysicalPort;
  const raw = rotatePoint({ x: block.x + port.x, y: block.y + port.y }, block);
  return {
    key: `${blockId}:${index}`,
    blockId,
    pos: alignToGrid(raw),
    side: getPortSide(block, raw),
  };
}
/**
 * Group the physical (pin-to-pin) wires of a diagram into nets: union-find over
 * the pins they share. Signal wires and anything not joining two genuine
 * physical pins are ignored. Deterministic order (by net key).
 */
export function collectPhysicalNets(
  connections: readonly unknown[],
  blocks: ReadonlyMap<unknown, unknown>
): PhysicalNet[] {
  const blockMap = mapLike(blocks);
  if (!blockMap || !Array.isArray(connections)) return [];
  blocks = blockMap;
  const parent = new Map<string, string>();
  const find = (k: string): string => {
    let r = k;
    while (parent.get(r) !== r) r = parent.get(r) as string;
    let c = k;
    while (parent.get(c) !== r) {
      const n = parent.get(c) as string;
      parent.set(c, r);
      c = n;
    }
    return r;
  };
  const ensure = (k: string) => {
    if (!parent.has(k)) parent.set(k, k);
  };
  const union = (a: string, b: string) => {
    const ra = find(a);
    const rb = find(b);
    if (ra !== rb) parent.set(rb, ra);
  };

  const pins = new Map<string, Pin>();
  const entries: Array<{ conn: PhysicalConn; a: Pin; b: Pin }> = [];
  for (const raw of connections) {
    const conn = raw as PhysicalConn;
    if (!conn) continue;
    const fromBlock = blockOf(blocks, conn.from);
    const toBlock = blockOf(blocks, conn.to);
    if (!fromBlock || !toBlock) continue;
    const fi = Number(conn.fromIndex ?? 0);
    const ti = Number(conn.toIndex ?? 0);
    if (!purePhysicalPair(fromBlock, toBlock, fi, ti)) continue;
    const a = pinOf(fromBlock, String(conn.from), fi);
    const b = pinOf(toBlock, String(conn.to), ti);
    if (a.key === b.key) continue;
    if (!pins.has(a.key)) pins.set(a.key, a);
    if (!pins.has(b.key)) pins.set(b.key, b);
    ensure(a.key);
    ensure(b.key);
    union(a.key, b.key);
    entries.push({ conn, a: pins.get(a.key) as Pin, b: pins.get(b.key) as Pin });
  }

  const byRoot = new Map<string, PhysicalNet>();
  for (const e of entries) {
    const root = find(e.a.key);
    let net = byRoot.get(root);
    if (!net) {
      net = { key: root, conns: [], pins: [], pinsOf: new Map() };
      byRoot.set(root, net);
    }
    net.conns.push(e.conn);
    net.pinsOf.set(e.conn, [e.a, e.b]);
  }
  for (const net of byRoot.values()) {
    const seen = new Set<string>();
    for (const [a, b] of net.pinsOf.values()) {
      for (const p of [a, b]) {
        if (!seen.has(p.key)) {
          seen.add(p.key);
          net.pins.push(p);
        }
      }
    }
    net.pins.sort((p, q) => (p.key < q.key ? -1 : p.key > q.key ? 1 : 0));
    net.key = `net:${net.pins[0].key}`;
  }
  return [...byRoot.values()].sort((a, b) => (a.key < b.key ? -1 : a.key > b.key ? 1 : 0));
}
//=============================================================================
/** conn -> net key, for every wire that belongs to a physical net. */
export function physicalNetKeys(
  connections: readonly unknown[],
  blocks: ReadonlyMap<unknown, unknown>
): Map<unknown, string> {
  const out = new Map<unknown, string>();
  for (const net of collectPhysicalNets(connections, blocks)) {
    for (const c of net.conns) out.set(c, net.key);
  }
  return out;
}
//=============================================================================
// Trees are built for a HORIZONTAL trunk; a vertical trunk is the same problem
// in the transposed plane (x <-> y, sides swapped).
type Tap = { path: Point[]; tx: number }; // pin -> its trunk point (inclusive); tx = along-trunk coordinate
type Trunk = { level: number; lo: number; hi: number };
// Whether a trunk at `level` spanning [lo, hi] stays clear of other nets' buses.
type FreeFn = (level: number, lo: number, hi: number) => boolean;
type Tree = { paths: Map<PhysicalConn, Point[]>; trunk: Trunk };
/** Along-trunk coordinate of a pin's tap (independent of the trunk level). */
function tapX(pin: Pin): number {
  if (pin.side === 'top' || pin.side === 'bottom') return pin.pos.x;
  return pin.side === 'left' ? pin.pos.x - GRID_SIZE : pin.pos.x + GRID_SIZE;
}
/**
 * Candidate levels for a horizontal trunk, best first. A 'top' pin taps UP, a
 * 'bottom' pin taps DOWN, and a 'left'/'right' pin can lie ON the trunk (zero
 * tap) when every other pin sits on its facing side. Order: a side pin's own
 * line, the midpoint between the up- and down-facing pins, those bounds, then
 * the clear track just above / below the blocks the trunk spans, stepped
 * outward while another net's bus holds it. Empty when the side constraints
 * contradict each other.
 */
function trunkCandidates(pins: Pin[], rects: OwnedRect[], free: FreeFn): number[] {
  let lower = -Infinity;
  let upper = Infinity;
  const onLine: number[] = [];
  const ys: number[] = [];
  for (const p of pins) {
    ys.push(p.pos.y);
    if (p.side === 'top') upper = Math.min(upper, p.pos.y - GRID_SIZE);
    else if (p.side === 'bottom') lower = Math.max(lower, p.pos.y + GRID_SIZE);
    else {
      const facing = pins.every(
        (q) => q === p || (p.side === 'right' ? q.pos.x > p.pos.x : q.pos.x < p.pos.x)
      );
      if (facing) onLine.push(p.pos.y);
    }
  }
  if (lower > upper) return [];
  const txs = pins.map(tapX);
  const lo = Math.min(...txs);
  const hi = Math.max(...txs);
  const out: number[] = [];
  const push = (v: number) => {
    if (!Number.isFinite(v) || v < lower || v > upper || out.includes(v)) return;
    if (free(v, lo, hi)) out.push(v);
  };
  const sorted = ys.slice().sort((a, b) => a - b);
  const median = sorted[Math.floor(sorted.length / 2)];
  for (const y of onLine
    .slice()
    .sort((a, b) => Math.abs(a - median) - Math.abs(b - median) || a - b))
    push(y);
  if (Number.isFinite(lower) && Number.isFinite(upper)) {
    let t = Math.round((lower + upper) / 2 / GRID_SIZE) * GRID_SIZE;
    if (t < lower) t = lower;
    if (t > upper) t = upper;
    push(t);
  }
  if (Number.isFinite(lower)) push(lower);
  if (Number.isFinite(upper)) push(upper);
  // Clearance levels: just outside every block the trunk would run across,
  // stepped outward past any bus another net already laid on that track.
  let top = Infinity;
  let bottom = -Infinity;
  for (const { rect } of rects) {
    if (rect.right <= lo || rect.left >= hi) continue;
    top = Math.min(top, rect.top);
    bottom = Math.max(bottom, rect.bottom);
  }
  const MAX_STEPS = 50;
  if (Number.isFinite(top) && !Number.isFinite(lower)) {
    // no down-facing pin
    let v = Math.floor((top - GRID_SIZE) / GRID_SIZE) * GRID_SIZE;
    for (let k = 0; k < MAX_STEPS && !free(v, lo, hi); k += 1) v -= BUS_SPACING;
    push(v);
  }
  if (Number.isFinite(bottom) && !Number.isFinite(upper)) {
    // no up-facing pin
    let v = Math.ceil((bottom + GRID_SIZE) / GRID_SIZE) * GRID_SIZE;
    for (let k = 0; k < MAX_STEPS && !free(v, lo, hi); k += 1) v += BUS_SPACING;
    push(v);
  }
  return out;
}
//=============================================================================
function tapOf(pin: Pin, ty: number): Tap | null {
  const { pos, side } = pin;
  if (side === 'top') return ty < pos.y ? { path: [pos, { x: pos.x, y: ty }], tx: pos.x } : null;
  if (side === 'bottom') return ty > pos.y ? { path: [pos, { x: pos.x, y: ty }], tx: pos.x } : null;
  const sx = side === 'left' ? pos.x - GRID_SIZE : pos.x + GRID_SIZE;
  if (ty === pos.y) return { path: [pos, { x: sx, y: ty }], tx: sx };
  return { path: [pos, { x: sx, y: pos.y }, { x: sx, y: ty }], tx: sx };
}
//=============================================================================
function segmentCrossesRect(
  orient: 'H' | 'V',
  track: number,
  lo: number,
  hi: number,
  r: Rect
): boolean {
  if (orient === 'H') return track > r.top && track < r.bottom && hi > r.left && lo < r.right;
  return track > r.left && track < r.right && hi > r.top && lo < r.bottom;
}
//=============================================================================
function pathCrosses(path: Point[], rects: OwnedRect[], ownId: string | null): boolean {
  for (let i = 0; i + 1 < path.length; i += 1) {
    const a = path[i];
    const b = path[i + 1];
    if (a.x === b.x && a.y === b.y) continue;
    const orient: 'H' | 'V' = a.y === b.y ? 'H' : 'V';
    const track = orient === 'H' ? a.y : a.x;
    const lo = orient === 'H' ? Math.min(a.x, b.x) : Math.min(a.y, b.y);
    const hi = orient === 'H' ? Math.max(a.x, b.x) : Math.max(a.y, b.y);
    for (const { id, rect } of rects) {
      if (ownId !== null && id === ownId) continue;
      if (segmentCrossesRect(orient, track, lo, hi, rect)) return true;
    }
  }
  return false;
}
//=============================================================================
/** Drop repeated points, then interior points that lie on a straight run. */
function tidy(pts: Point[]): Point[] {
  const d: Point[] = [];
  for (const p of pts) {
    const l = d[d.length - 1];
    if (!l || l.x !== p.x || l.y !== p.y) d.push({ x: p.x, y: p.y });
  }
  const o: Point[] = [];
  for (let i = 0; i < d.length; i += 1) {
    if (i > 0 && i < d.length - 1) {
      const a = o[o.length - 1];
      const b = d[i];
      const c = d[i + 1];
      if ((a.x === b.x && b.x === c.x) || (a.y === b.y && b.y === c.y)) continue;
    }
    o.push(d[i]);
  }
  return o;
}
//=============================================================================
/** A path folding back on itself along one line (a stub retraced into a block). */
function backtracks(pts: Point[]): boolean {
  for (let i = 0; i + 2 < pts.length; i += 1) {
    const a = pts[i];
    const b = pts[i + 1];
    const c = pts[i + 2];
    if (a.x === b.x && b.x === c.x && Math.sign(b.y - a.y) * Math.sign(c.y - b.y) < 0) return true;
    if (a.y === b.y && b.y === c.y && Math.sign(b.x - a.x) * Math.sign(c.x - b.x) < 0) return true;
  }
  return false;
}
//=============================================================================
function treeAtLevel(net: PhysicalNet, rects: OwnedRect[], ty: number): Tree | null {
  const taps = new Map<string, Tap>();
  for (const pin of net.pins) {
    const tap = tapOf(pin, ty);
    if (!tap) return null;
    if (pathCrosses(tap.path, rects, pin.blockId)) return null;
    taps.set(pin.key, tap);
  }
  const txs = [...taps.values()].map((t) => t.tx);
  const lo = Math.min(...txs);
  const hi = Math.max(...txs);
  if (
    hi > lo &&
    pathCrosses(
      [
        { x: lo, y: ty },
        { x: hi, y: ty },
      ],
      rects,
      null
    )
  )
    return null;

  const paths = new Map<PhysicalConn, Point[]>();
  for (const [conn, [a, b]] of net.pinsOf) {
    const ta = taps.get(a.key) as Tap;
    const tb = taps.get(b.key) as Tap;
    const raw: Point[] = [...ta.path];
    if (ta.tx !== tb.tx) raw.push({ x: tb.tx, y: ty });
    raw.push(...tb.path.slice().reverse());
    if (backtracks(raw)) return null;
    paths.set(conn, tidy(raw));
  }
  return { paths, trunk: { level: ty, lo, hi } };
}
//=============================================================================
function buildTreeH(net: PhysicalNet, rects: OwnedRect[], free: FreeFn): Tree | null {
  for (const ty of trunkCandidates(net.pins, rects, free)) {
    const tree = treeAtLevel(net, rects, ty);
    if (tree) return tree;
  }
  return null;
}
//=============================================================================
const SWAP_SIDE: Record<Side, Side> = {
  left: 'top',
  right: 'bottom',
  top: 'left',
  bottom: 'right',
};
//=============================================================================
const swapPoint = (p: Point): Point => ({ x: p.y, y: p.x });
function transposeNet(net: PhysicalNet): PhysicalNet {
  const byKey = new Map<string, Pin>();
  const pins = net.pins.map((p) => {
    const t: Pin = { ...p, pos: swapPoint(p.pos), side: SWAP_SIDE[p.side] };
    byKey.set(p.key, t);
    return t;
  });
  const pinsOf = new Map<PhysicalConn, [Pin, Pin]>();
  for (const [conn, [a, b]] of net.pinsOf) {
    pinsOf.set(conn, [byKey.get(a.key) as Pin, byKey.get(b.key) as Pin]);
  }
  return { key: net.key, conns: net.conns, pins, pinsOf };
}
//=============================================================================
function transposeRects(rects: OwnedRect[]): OwnedRect[] {
  return rects.map(({ id, rect }) => ({
    id,
    rect: { left: rect.top, right: rect.bottom, top: rect.left, bottom: rect.right },
  }));
}
//=============================================================================
function blockRects(blocks: ReadonlyMap<unknown, unknown>): OwnedRect[] {
  const out: OwnedRect[] = [];
  const blockMap = mapLike(blocks);
  if (!blockMap) return out;
  for (const [id, raw] of blockMap) {
    const b = raw as PhysicalBlock;
    if (!b || !Number.isFinite(b.x) || !Number.isFinite(b.y)) continue;
    let { left, right, top, bottom } = getRotatedBounds(b);
    for (const p of b.ports || []) {
      const pos = rotatePoint({ x: b.x + p.x, y: b.y + p.y }, b);
      left = Math.min(left, pos.x - PORT_RADIUS);
      right = Math.max(right, pos.x + PORT_RADIUS);
      top = Math.min(top, pos.y - PORT_RADIUS);
      bottom = Math.max(bottom, pos.y + PORT_RADIUS);
    }
    out.push({ id: String(id), rect: { left, right, top, bottom } });
  }
  return out;
}
/**
 * Bus geometry for every physical net: conn -> orthogonal path. The trunk is
 * horizontal when the pins spread more in x than in y, else vertical, with the
 * other orientation as fallback. Nets are laid narrowest first and a bus is
 * stacked further out when another net's bus already uses the track over the
 * same span. A net that cannot be drawn cleanly (contradictory pin sides, a
 * block in the way, a hand-pinned wire) is absent from the result and keeps its
 * geometry.
 */
export function buildPhysicalNetTreePaths(
  connections: readonly unknown[],
  blocks: ReadonlyMap<unknown, unknown>
): Map<unknown, Point[]> {
  const out = new Map<unknown, Point[]>();
  const rects = blockRects(blocks);
  const usedH: Trunk[] = [];
  const usedV: Trunk[] = [];
  const clear = (used: Trunk[], level: number, lo: number, hi: number) =>
    !used.some(
      (u) => Math.abs(u.level - level) < BUS_SPACING && Math.min(u.hi, hi) - Math.max(u.lo, lo) > 0
    );
  const freeH: FreeFn = (level, lo, hi) => clear(usedH, level, lo, hi);
  const freeV: FreeFn = (level, lo, hi) => clear(usedV, level, lo, hi);

  const nets = collectPhysicalNets(connections, blocks)
    .filter((net) => net.pins.length >= 2)
    .map((net) => {
      const xs = net.pins.map((p) => p.pos.x);
      const ys = net.pins.map((p) => p.pos.y);
      const spanX = Math.max(...xs) - Math.min(...xs);
      const spanY = Math.max(...ys) - Math.min(...ys);
      return { net, spanX, spanY, span: Math.max(spanX, spanY) };
    })
    .sort((a, b) => a.span - b.span || (a.net.key < b.net.key ? -1 : 1));

  for (const { net, spanX, spanY } of nets) {
    const pinned = net.conns.some(
      (c) => Array.isArray(c.points) && c.points.some((p) => Boolean(p && p.pin))
    );
    if (pinned) continue;
    const horizontal = (): (Tree & { vertical: boolean }) | null => {
      const t = buildTreeH(net, rects, freeH);
      return t ? { ...t, vertical: false } : null;
    };
    const vertical = (): (Tree & { vertical: boolean }) | null => {
      const t = buildTreeH(transposeNet(net), transposeRects(rects), freeV);
      if (!t) return null;
      const paths = new Map<PhysicalConn, Point[]>();
      for (const [conn, pts] of t.paths) paths.set(conn, pts.map(swapPoint));
      return { paths, trunk: t.trunk, vertical: true };
    };
    const tree = spanX >= spanY ? horizontal() ?? vertical() : vertical() ?? horizontal();
    if (!tree) continue;
    (tree.vertical ? usedV : usedH).push(tree.trunk);
    for (const [conn, pts] of tree.paths) out.set(conn, pts);
  }
  return out;
}
/**
 * Junction dots for physical nets: every point where three or more branches of
 * one net meet (a tap joining the shared trunk, a multi-way node). A plain
 * corner (two directions) or a pin end (one) is not a junction.
 */
export function collectPhysicalNetJunctionPoints(
  connections: readonly unknown[],
  blocks: ReadonlyMap<unknown, unknown>
): Point[] {
  const out: Point[] = [];
  for (const net of collectPhysicalNets(connections, blocks)) {
    if (net.conns.length < 2) continue;
    const dirs = new Map<string, Set<string>>();
    const add = (p: Point, d: string) => {
      const k = `${p.x},${p.y}`;
      let s = dirs.get(k);
      if (!s) {
        s = new Set();
        dirs.set(k, s);
      }
      s.add(d);
    };
    for (const c of net.conns) {
      const pts = (c.points || []).filter((p) => p && Number.isFinite(p.x) && Number.isFinite(p.y));
      for (let i = 0; i + 1 < pts.length; i += 1) {
        const a = pts[i];
        const b = pts[i + 1];
        if (a.x === b.x && a.y === b.y) continue;
        if (a.x !== b.x && a.y !== b.y) continue;
        const d = a.x === b.x ? (b.y > a.y ? 'D' : 'U') : b.x > a.x ? 'R' : 'L';
        const back = d === 'D' ? 'U' : d === 'U' ? 'D' : d === 'R' ? 'L' : 'R';
        add(a, d);
        add(b, back);
      }
    }
    for (const [k, s] of dirs) {
      if (s.size >= 3) {
        const [x, y] = k.split(',').map(Number);
        out.push({ x, y });
      }
    }
  }
  return out;
}
//=============================================================================
