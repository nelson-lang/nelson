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
// Post-route nudging / separation pass.
//
// The router's soft proximity penalties discourage (but cannot forbid)
// two different nets sharing the exact same channel (collinear overlapping
// segments). This pass runs on FINAL routed geometry (pixel space) at the
// two apply choke points, groups overlapping collinear segments into
// channels, orders members by where they come from / go to (crossing-
// minimizing heuristic), and spreads them one grid cell apart.
//
// Design constraints:
// - offsets are whole grid cells so nudged routes round-trip losslessly
//   into prevSolution (route hysteresis keeps them stable afterwards)
// - shared-source siblings (same from:fromIndex) move as ONE member; their
//   common prefix overlaps by design. The branches of one PHYSICAL net (passed
//   in as `netKeys`) are grouped the same way: they share a trunk on purpose
// - every move is validated (stub preservation, no block crossing, no new
//   exact overlap) and reverted individually when invalid, so quality can
//   only improve
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { GRID_SIZE } from '../utils/geometry.js';
import { getRotatedBounds, rotatePoint } from '../utils/port-utils.js';
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
export type BlockLike = {
  id: string;
  x: number;
  y: number;
  width: number;
  height: number;
  rotation?: number;
  ports?: { x: number; y: number }[];
};
//=============================================================================
export type Rect = { left: number; right: number; top: number; bottom: number };
type ChannelSegment = {
  conn: ConnLike;
  connIdx: number;
  segIndex: number; // points[segIndex] -> points[segIndex + 1]
  orient: 'H' | 'V';
  track: number; // y for H, x for V
  lo: number;
  hi: number;
  entryCoord: number; // perpendicular coord of the preceding bend's far end
  exitCoord: number; // perpendicular coord of the following bend's far end
  sourceKey: string; // from:fromIndex, shared-source grouping
  connKey: string;
};
//=============================================================================
type Member = {
  segments: ChannelSegment[];
  entryCoord: number;
  exitCoord: number;
  connKey: string;
};
//=============================================================================
const PORT_RADIUS = 6;
export function blockRect(block: BlockLike): Rect {
  const b = getRotatedBounds(block as any);
  let { left, right, top, bottom } = b;
  for (const p of block.ports || []) {
    const pos = rotatePoint({ x: block.x + p.x, y: block.y + p.y }, block as any);
    left = Math.min(left, pos.x - PORT_RADIUS);
    right = Math.max(right, pos.x + PORT_RADIUS);
    top = Math.min(top, pos.y - PORT_RADIUS);
    bottom = Math.max(bottom, pos.y + PORT_RADIUS);
  }
  return { left, right, top, bottom };
}
//=============================================================================
function connKeyOf(conn: ConnLike, idx: number): string {
  return `${conn.from}:${conn.fromIndex ?? 0}->${conn.to}:${conn.toIndex ?? 0}:${idx}`;
}
//=============================================================================
/** Grouping key: the physical net a wire belongs to, else its source port. */
function sourceKeyOf(conn: ConnLike, netKeys?: Map<unknown, string>): string {
  return netKeys?.get(conn) ?? `${conn.from}:${conn.fromIndex ?? 0}`;
}
//=============================================================================
function collectSegments(
  connections: ConnLike[],
  netKeys?: Map<unknown, string>
): ChannelSegment[] {
  const out: ChannelSegment[] = [];
  connections.forEach((conn, connIdx) => {
    const pts = conn.points;
    if (!Array.isArray(pts) || pts.length < 4) return; // no interior segment
    // Pinned (user-anchored) wires are immovable for the separation pass.
    if (pts.some((p) => (p as { pin?: boolean }).pin)) return;
    const segCount = pts.length - 1;
    for (let i = 1; i < segCount - 1; i += 1) {
      const a = pts[i];
      const b = pts[i + 1];
      if (!a || !b || (a.x === b.x && a.y === b.y)) continue;
      const orient = a.x === b.x ? 'V' : a.y === b.y ? 'H' : null;
      if (!orient) continue;
      const prev = pts[i - 1];
      const next = pts[i + 2];
      if (!prev || !next) continue;
      if (orient === 'H') {
        out.push({
          conn,
          connIdx,
          segIndex: i,
          orient,
          track: a.y,
          lo: Math.min(a.x, b.x),
          hi: Math.max(a.x, b.x),
          entryCoord: prev.y,
          exitCoord: next.y,
          sourceKey: sourceKeyOf(conn, netKeys),
          connKey: connKeyOf(conn, connIdx),
        });
      } else {
        out.push({
          conn,
          connIdx,
          segIndex: i,
          orient,
          track: a.x,
          lo: Math.min(a.y, b.y),
          hi: Math.max(a.y, b.y),
          entryCoord: prev.x,
          exitCoord: next.x,
          sourceKey: sourceKeyOf(conn, netKeys),
          connKey: connKeyOf(conn, connIdx),
        });
      }
    }
  });
  return out;
}
//=============================================================================
/** Union-find channel clustering: same orient+track, overlapping intervals. */
function buildChannels(segments: ChannelSegment[]): ChannelSegment[][] {
  const byTrack = new Map<string, ChannelSegment[]>();
  for (const seg of segments) {
    const key = `${seg.orient}:${seg.track}`;
    const list = byTrack.get(key) || [];
    list.push(seg);
    byTrack.set(key, list);
  }

  const channels: ChannelSegment[][] = [];
  for (const list of byTrack.values()) {
    if (list.length < 2) continue;
    list.sort((a, b) => a.lo - b.lo || a.hi - b.hi || (a.connKey < b.connKey ? -1 : 1));
    let cluster: ChannelSegment[] = [list[0]];
    let clusterHi = list[0].hi;
    for (let i = 1; i < list.length; i += 1) {
      const seg = list[i];
      if (seg.lo < clusterHi) {
        cluster.push(seg);
        clusterHi = Math.max(clusterHi, seg.hi);
      } else {
        if (cluster.length > 1) channels.push(cluster);
        cluster = [seg];
        clusterHi = seg.hi;
      }
    }
    if (cluster.length > 1) channels.push(cluster);
  }
  return channels;
}
//=============================================================================
function buildMembers(cluster: ChannelSegment[]): Member[] {
  const bySource = new Map<string, ChannelSegment[]>();
  for (const seg of cluster) {
    // Segments of the same net always move together; shared-source siblings
    // merge too (their common prefix overlaps by design).
    const key = seg.sourceKey;
    const list = bySource.get(key) || [];
    list.push(seg);
    bySource.set(key, list);
  }
  const members: Member[] = [];
  for (const segs of bySource.values()) {
    segs.sort((a, b) => (a.connKey < b.connKey ? -1 : a.connKey > b.connKey ? 1 : 0));
    members.push({
      segments: segs,
      entryCoord: Math.min(...segs.map((s) => s.entryCoord)),
      exitCoord: Math.min(...segs.map((s) => s.exitCoord)),
      connKey: segs[0].connKey,
    });
  }
  // Crossing-minimizing order: members whose neighbors come from smaller
  // perpendicular coordinates take the smaller offsets.
  members.sort(
    (a, b) =>
      a.entryCoord - b.entryCoord ||
      a.exitCoord - b.exitCoord ||
      (a.connKey < b.connKey ? -1 : a.connKey > b.connKey ? 1 : 0)
  );
  return members;
}
//=============================================================================
export function segmentCrossesRect(
  orient: 'H' | 'V',
  track: number,
  lo: number,
  hi: number,
  rect: Rect
): boolean {
  if (orient === 'H') {
    return track > rect.top && track < rect.bottom && hi > rect.left && lo < rect.right;
  }
  return track > rect.left && track < rect.right && hi > rect.top && lo < rect.bottom;
}
//=============================================================================
/** Apply a perpendicular offset to one member; returns an undo closure. */
function applyMemberOffset(member: Member, offset: number): () => void {
  const touched: Array<{ pt: Point; axis: 'x' | 'y'; old: number }> = [];
  const seen = new Set<Point>();
  for (const seg of member.segments) {
    const pts = seg.conn.points as Point[];
    const axis: 'x' | 'y' = seg.orient === 'H' ? 'y' : 'x';
    for (const pt of [pts[seg.segIndex], pts[seg.segIndex + 1]]) {
      if (seen.has(pt)) continue;
      seen.add(pt);
      touched.push({ pt, axis, old: pt[axis] });
      pt[axis] = pt[axis] + offset;
    }
  }
  return () => {
    for (const t of touched) t.pt[t.axis] = t.old;
  };
}
/** First/last segments must keep direction and a minimal stub length.
 *  Lengths may differ (guard bends can be spliced in): the two paths are
 *  compared end-for-end, not index-for-index. */
function stubsIntact(conn: ConnLike, original: Point[]): boolean {
  const pts = conn.points as Point[];
  if (!Array.isArray(pts) || pts.length < 2 || original.length < 2) return false;
  const check = (oa: Point, ob: Point, na: Point, nb: Point) => {
    const odx = Math.sign(ob.x - oa.x);
    const ody = Math.sign(ob.y - oa.y);
    const ndx = Math.sign(nb.x - na.x);
    const ndy = Math.sign(nb.y - na.y);
    if (odx !== ndx || ody !== ndy) return false;
    const len = Math.abs(nb.x - na.x) + Math.abs(nb.y - na.y);
    return len >= GRID_SIZE;
  };
  return (
    check(original[0], original[1], pts[0], pts[1]) &&
    check(
      original[original.length - 2],
      original[original.length - 1],
      pts[pts.length - 2],
      pts[pts.length - 1]
    )
  );
}
/**
 * Spread overlapping collinear segments of different nets one grid cell
 * apart. Mutates `conn.points` in place; every member move is validated and
 * reverted when it would cross a block, break a stub, or create a new exact
 * overlap. Deterministic and idempotent (a separated channel produces
 * singleton clusters on the next run and is left untouched).
 */
export function nudgeConnections(
  connections: ConnLike[],
  blocks: Iterable<BlockLike> = [],
  options: { spacing?: number; netKeys?: Map<unknown, string> } = {}
): number {
  if (!Array.isArray(connections) || connections.length < 2) return 0;
  const spacing = Number(options.spacing) || GRID_SIZE;

  const rects: Rect[] = [];
  for (const b of blocks || []) {
    if (b && Number.isFinite(b.x) && Number.isFinite(b.y)) rects.push(blockRect(b));
  }

  const segments = collectSegments(connections, options.netKeys);
  const channels = buildChannels(segments);
  channels.sort((a, b) => {
    const ka = `${a[0].orient}:${a[0].track}:${a[0].lo}`;
    const kb = `${b[0].orient}:${b[0].track}:${b[0].lo}`;
    return ka < kb ? -1 : ka > kb ? 1 : 0;
  });

  let moved = 0;
  const validateMember = (member: Member, originals: Map<ConnLike, Point[]>): boolean => {
    for (const [conn, original] of originals) {
      if (!stubsIntact(conn, original)) return false;
    }
    for (const seg of member.segments) {
      const pts = seg.conn.points as Point[];
      const a = pts[seg.segIndex];
      const b = pts[seg.segIndex + 1];
      const track = seg.orient === 'H' ? a.y : a.x;
      const lo = seg.orient === 'H' ? Math.min(a.x, b.x) : Math.min(a.y, b.y);
      const hi = seg.orient === 'H' ? Math.max(a.x, b.x) : Math.max(a.y, b.y);
      if (rects.some((r) => segmentCrossesRect(seg.orient, track, lo, hi, r))) return false;
      // A new exact overlap with a foreign wire would just trade one for another.
      for (const other of segments) {
        if (other.conn === seg.conn) continue;
        if (other.sourceKey === seg.sourceKey) continue;
        if (member.segments.includes(other)) continue;
        if (other.orient !== seg.orient) continue;
        // Compare against the other's CURRENT track (it may have moved).
        const opts2 = other.conn.points as Point[];
        const oa = opts2[other.segIndex];
        const obp = opts2[other.segIndex + 1];
        if (!oa || !obp) continue;
        const otherTrack = other.orient === 'H' ? oa.y : oa.x;
        if (otherTrack !== track) continue;
        const olo = other.orient === 'H' ? Math.min(oa.x, obp.x) : Math.min(oa.y, obp.y);
        const ohi = other.orient === 'H' ? Math.max(oa.x, obp.x) : Math.max(oa.y, obp.y);
        if (Math.min(hi, ohi) - Math.max(lo, olo) > 0) return false;
      }
    }
    return true;
  };

  for (const cluster of channels) {
    const members = buildMembers(cluster);
    if (members.length < 2) continue; // single net (or shared source), by design

    const mid = Math.floor((members.length - 1) / 2);
    members.forEach((member, k) => {
      const offset = (k - mid) * spacing;
      if (offset === 0) return;

      const originals = new Map<ConnLike, Point[]>();
      for (const seg of member.segments) {
        if (!originals.has(seg.conn)) {
          originals.set(
            seg.conn,
            (seg.conn.points as Point[]).map((p) => ({ ...p }))
          );
        }
      }

      // Preferred direction first, mirrored direction as fallback (the
      // preferred side may be blocked by an obstacle or a short stub).
      for (const candidate of [offset, -offset]) {
        const undo = applyMemberOffset(member, candidate);
        if (validateMember(member, originals)) {
          moved += 1;
          return;
        }
        undo();
      }
    });
  }
  // Centering pass: a lone interior segment within one grid cell of a block is
  // re-centered in its free channel, only when that increases its clearance.
  const clustered = new Set<ChannelSegment>();
  for (const cluster of channels) for (const seg of cluster) clustered.add(seg);

  const sortedSingles = segments
    .filter((seg) => !clustered.has(seg))
    .sort((a, b) => {
      const ka = `${a.orient}:${a.track}:${a.lo}:${a.connKey}`;
      const kb = `${b.orient}:${b.track}:${b.lo}:${b.connKey}`;
      return ka < kb ? -1 : ka > kb ? 1 : 0;
    });

  for (const seg of sortedSingles) {
    let below = Infinity; // free space toward +axis
    let above = Infinity; // free space toward -axis
    for (const r of rects) {
      if (seg.orient === 'H') {
        if (seg.hi <= r.left || seg.lo >= r.right) continue;
        if (r.bottom <= seg.track) above = Math.min(above, seg.track - r.bottom);
        if (r.top >= seg.track) below = Math.min(below, r.top - seg.track);
      } else {
        if (seg.hi <= r.top || seg.lo >= r.bottom) continue;
        if (r.right <= seg.track) above = Math.min(above, seg.track - r.right);
        if (r.left >= seg.track) below = Math.min(below, r.left - seg.track);
      }
    }
    const clearance = Math.min(above, below);
    // One full cell of clearance is the router's normal distance.
    if (!(clearance < spacing)) continue;
    if (!Number.isFinite(above) && !Number.isFinite(below)) continue;

    // Midline of the free channel; unbounded sides are capped at 3 cells so
    // wires do not wander off.
    const upEdge = Number.isFinite(above) ? seg.track - above : seg.track - 3 * spacing;
    const downEdge = Number.isFinite(below) ? seg.track + below : seg.track + 3 * spacing;
    const target = Math.round((upEdge + downEdge) / 2 / spacing) * spacing;
    const offset = target - seg.track;
    if (offset === 0) continue;
    const newClearance = Math.min(
      Number.isFinite(above) ? above + offset : Infinity,
      Number.isFinite(below) ? below - offset : Infinity
    );
    if (!(newClearance > clearance)) continue;

    // Guard bends: next to a port stub, duplicate the shared bend so the stub
    // keeps its anchor. The splice shifts this conn's segment indices, so only
    // ONE centering correction is applied per pass; later passes continue.
    const original = (seg.conn.points as Point[]).map((p) => ({ ...p }));
    const pts = seg.conn.points as Point[];
    const axis: 'x' | 'y' = seg.orient === 'H' ? 'y' : 'x';
    // A guard bend only works when the motion is PERPENDICULAR to the stub;
    // moving a stub-adjacent segment parallel to its stub would fold the
    // path back on itself.
    const stubAxisOf = (a: Point, b: Point): 'x' | 'y' => (a.y === b.y ? 'x' : 'y');
    if (seg.segIndex === 1 && stubAxisOf(pts[0], pts[1]) === axis) continue;
    if (
      seg.segIndex === pts.length - 3 &&
      stubAxisOf(pts[pts.length - 2], pts[pts.length - 1]) === axis
    )
      continue;
    let i = seg.segIndex;
    if (i === 1) {
      pts.splice(1, 0, { ...pts[1] });
      i += 1;
    }
    if (i === pts.length - 3) {
      pts.splice(pts.length - 2, 0, { ...pts[pts.length - 2] });
    }
    pts[i][axis] += offset;
    pts[i + 1][axis] += offset;

    const track = seg.orient === 'H' ? pts[i].y : pts[i].x;
    const lo =
      seg.orient === 'H' ? Math.min(pts[i].x, pts[i + 1].x) : Math.min(pts[i].y, pts[i + 1].y);
    const hi =
      seg.orient === 'H' ? Math.max(pts[i].x, pts[i + 1].x) : Math.max(pts[i].y, pts[i + 1].y);
    let ok = stubsIntact(seg.conn, original);
    if (ok && rects.some((r) => segmentCrossesRect(seg.orient, track, lo, hi, r))) ok = false;
    if (ok) {
      for (const other of segments) {
        if (other.conn === seg.conn) continue;
        if (other.sourceKey === seg.sourceKey) continue;
        if (other.orient !== seg.orient) continue;
        const opts2 = other.conn.points as Point[];
        const oa = opts2[other.segIndex];
        const obp = opts2[other.segIndex + 1];
        if (!oa || !obp) continue;
        const otherTrack = other.orient === 'H' ? oa.y : oa.x;
        if (otherTrack !== track) continue;
        const olo = other.orient === 'H' ? Math.min(oa.x, obp.x) : Math.min(oa.y, obp.y);
        const ohi = other.orient === 'H' ? Math.max(oa.x, obp.x) : Math.max(oa.y, obp.y);
        if (Math.min(hi, ohi) - Math.max(lo, olo) > 0) {
          ok = false;
          break;
        }
      }
    }
    if (ok) {
      moved += 1;
      break; // one centering correction per pass (indices shifted)
    }
    seg.conn.points = original;
  }

  return moved;
}
//=============================================================================
