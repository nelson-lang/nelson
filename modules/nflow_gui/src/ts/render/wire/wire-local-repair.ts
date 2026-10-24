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
// Minimum-disturbance local repair for wire routes during block drags: only the
// terminal segment attached to a moved port (plus its adjacent perpendicular
// segment) deforms and everything else stays frozen. The repair is always computed
// from a baseline captured at drag start (never from the previous frame), so it is
// idempotent and cannot drift.
//=============================================================================
import { GRID_SIZE } from '../geometry.js';
import { rotatePoint, getPortSide, alignToGrid } from '../geometry.js';
import type { Point, Block as GeoBlock } from '../geometry.js';
import { buildFallbackPath } from './wire-paths.js';
//=============================================================================
/** Wire point; `pin` marks user-anchored waypoints (preserved verbatim). */
export type RepairPoint = Point & { pin?: boolean };
export type PortInfo = { pos: Point; side: string };
export type RepairBaseline = {
  points: RepairPoint[];
  fromPos: Point;
  toPos: Point;
};
//=============================================================================
type PortDesc = { type: string; index: number; x: number; y: number };
type BlockDesc = Omit<GeoBlock, 'ports'> & { ports?: PortDesc[] };
type StateLike = { blocks: Map<string, BlockDesc> };
type ConnLike = {
  from: string;
  to: string;
  fromIndex?: number;
  toIndex?: number;
  points?: RepairPoint[];
};
//=============================================================================
function clonePoint(p: RepairPoint): RepairPoint {
  return p.pin ? { x: p.x, y: p.y, pin: true } : { x: p.x, y: p.y };
}
//=============================================================================
function clonePoints(points: RepairPoint[]): RepairPoint[] {
  return points.map(clonePoint);
}
//=============================================================================
/** Drop consecutive duplicate points, merging pin flags of merged duplicates. */
export function dedupeRepairPoints(points: RepairPoint[]): RepairPoint[] {
  if (!points.length) return [];
  const out: RepairPoint[] = [clonePoint(points[0])];
  for (let i = 1; i < points.length; i += 1) {
    const p = points[i];
    const last = out[out.length - 1];
    if (p.x === last.x && p.y === last.y) {
      if (p.pin) last.pin = true;
      continue;
    }
    out.push(clonePoint(p));
  }
  return out;
}
//=============================================================================
function orientationOf(a: Point, b: Point): 'H' | 'V' | null {
  if (a.y === b.y && a.x !== b.x) return 'H';
  if (a.x === b.x && a.y !== b.y) return 'V';
  return null;
}
//=============================================================================
function stubDirection(side: string): Point {
  if (side === 'left') return { x: -1, y: 0 };
  if (side === 'right') return { x: 1, y: 0 };
  if (side === 'top') return { x: 0, y: -1 };
  return { x: 0, y: 1 };
}
//=============================================================================
function axisOfSide(side: string): 'H' | 'V' {
  return side === 'left' || side === 'right' ? 'H' : 'V';
}
//=============================================================================
/** Rigidly translate a whole path (multi-select drag: both endpoints moved). */
export function translatePath(points: RepairPoint[], dx: number, dy: number): RepairPoint[] {
  return points.map((p) => {
    const q = clonePoint(p);
    q.x += dx;
    q.y += dy;
    return q;
  });
}
/**
 * Deform a path whose FROM endpoint moved to `newPos`: only P[0] and P[1] move
 * (the terminal segment slides with the port, the adjacent perpendicular segment
 * stretches), P[2..] stay frozen. A fresh stub + orthogonal jog is inserted when
 * the terminal segment collapses below `minStub` (or reverses behind the port).
 */
function repairFromEnd(
  points: RepairPoint[],
  newPos: Point,
  side: string,
  minStub: number
): RepairPoint[] {
  const n = points.length;
  const Q = clonePoints(points);
  const old0 = points[0];
  if (newPos.x === old0.x && newPos.y === old0.y) return Q;
  Q[0] = { ...Q[0], x: newPos.x, y: newPos.y };

  const orient = orientationOf(old0, points[1]) ?? axisOfSide(side);

  if (n === 2) {
    // Single segment: keep it straight when still colinear, else rebuild a
    // compact S/Z (there is no routed shape to preserve on a 2-point path).
    if (orient === 'H' && newPos.y === Q[1].y) return dedupeRepairPoints(Q);
    if (orient === 'V' && newPos.x === Q[1].x) return dedupeRepairPoints(Q);
    return buildFallbackPath(newPos, Q[1]) as RepairPoint[];
  }

  if (orient === 'H') {
    Q[1] = { ...Q[1], y: newPos.y };
  } else {
    Q[1] = { ...Q[1], x: newPos.x };
  }

  // Terminal segment must remain a valid stub leaving the port on its side.
  const dir = stubDirection(side);
  const proj = (Q[1].x - Q[0].x) * dir.x + (Q[1].y - Q[0].y) * dir.y;
  if (proj < minStub) {
    const stubPt: RepairPoint = { x: Q[0].x + dir.x * minStub, y: Q[0].y + dir.y * minStub };
    const jog: RepairPoint = dir.x !== 0 ? { x: stubPt.x, y: Q[1].y } : { x: Q[1].x, y: stubPt.y };
    Q.splice(1, 0, stubPt, jog);
  }

  return dedupeRepairPoints(Q);
}
//=============================================================================
function reversePoints(points: RepairPoint[]): RepairPoint[] {
  return points.slice().reverse();
}
//=============================================================================
export function repairEndpointMove(
  points: RepairPoint[],
  end: 'from' | 'to',
  newPos: Point,
  side: string,
  minStub: number = GRID_SIZE
): RepairPoint[] {
  if (!points || points.length < 2) return points ? clonePoints(points) : [];
  if (end === 'to') {
    return reversePoints(repairFromEnd(reversePoints(points), newPos, side, minStub));
  }
  return repairFromEnd(points, newPos, side, minStub);
}
/**
 * Repair a baseline route for the current port positions.
 *
 * - both endpoints moved by the same delta: rigid translation
 * - otherwise each moved endpoint is repaired independently, middle frozen
 */
export function repairPath(
  baseline: RepairBaseline,
  fromNow: PortInfo,
  toNow: PortInfo,
  minStub: number = GRID_SIZE
): RepairPoint[] {
  const dFx = fromNow.pos.x - baseline.fromPos.x;
  const dFy = fromNow.pos.y - baseline.fromPos.y;
  const dTx = toNow.pos.x - baseline.toPos.x;
  const dTy = toNow.pos.y - baseline.toPos.y;

  if (dFx === 0 && dFy === 0 && dTx === 0 && dTy === 0) {
    return clonePoints(baseline.points);
  }
  if (dFx === dTx && dFy === dTy) {
    return translatePath(baseline.points, dFx, dFy);
  }
  let Q = clonePoints(baseline.points);
  if (dFx !== 0 || dFy !== 0) {
    Q = repairEndpointMove(Q, 'from', fromNow.pos, fromNow.side, minStub);
  }
  if (dTx !== 0 || dTy !== 0) {
    Q = repairEndpointMove(Q, 'to', toNow.pos, toNow.side, minStub);
  }
  return Q;
}
/**
 * Resolve the grid-aligned port anchors and sides for a connection.
 * Ports are matched by (type, index); mirrors enforcePortStubs.
 */
export function resolveConnPorts(
  state: StateLike,
  conn: ConnLike
): { from: PortInfo; to: PortInfo } | null {
  const blocks = state?.blocks;
  if (!blocks || typeof blocks.get !== 'function') return null;
  const fromBlock = blocks.get(conn.from);
  const toBlock = blocks.get(conn.to);
  if (!fromBlock || !toBlock) return null;
  const fromPort = (fromBlock.ports || []).find(
    (p) => p.type === 'out' && p.index === (conn.fromIndex ?? 0)
  );
  const toPort = (toBlock.ports || []).find(
    (p) => p.type === 'in' && p.index === (conn.toIndex ?? 0)
  );
  if (!fromPort || !toPort) return null;
  const fromRot = rotatePoint(
    { x: fromBlock.x + fromPort.x, y: fromBlock.y + fromPort.y },
    fromBlock
  );
  const toRot = rotatePoint({ x: toBlock.x + toPort.x, y: toBlock.y + toPort.y }, toBlock);
  return {
    from: { pos: alignToGrid(fromRot), side: getPortSide(fromBlock, fromRot) },
    to: { pos: alignToGrid(toRot), side: getPortSide(toBlock, toRot) },
  };
}
/**
 * Repair a connection's route during a drag session.
 *
 * On the first call of a session, the current route and port anchors are
 * captured into `baselines`; subsequent calls always deform that baseline.
 * Returns null when there is no valid baseline (freshly created wire);
 * caller falls back to the S/Z preview.
 */
export function repairConnForDrag(
  state: StateLike,
  conn: ConnLike,
  baselines: WeakMap<object, RepairBaseline>
): RepairPoint[] | null {
  const ports = resolveConnPorts(state, conn);
  if (!ports) return null;

  let base = baselines.get(conn as object);
  if (!base) {
    const pts = conn.points;
    if (!Array.isArray(pts) || pts.length < 2) return null;
    base = {
      points: clonePoints(pts),
      fromPos: { x: pts[0].x, y: pts[0].y },
      toPos: { x: pts[pts.length - 1].x, y: pts[pts.length - 1].y },
    };
    baselines.set(conn as object, base);
  }

  return repairPath(base, ports.from, ports.to);
}
//=============================================================================
