//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Pure helpers extracted for incremental porting of create-renderer
export type Point = { x: number; y: number };
export type Connection = { points: Point[] };
export function distancePointToSegment(a: Point, b: Point, p: Point): number {
  const vx = b.x - a.x;
  const vy = b.y - a.y;
  const wx = p.x - a.x;
  const wy = p.y - a.y;
  const c1 = vx * wx + vy * wy;
  if (c1 <= 0) return Math.hypot(p.x - a.x, p.y - a.y);
  const c2 = vx * vx + vy * vy;
  if (c2 <= c1) return Math.hypot(p.x - b.x, p.y - b.y);
  const t = c1 / c2;
  const projX = a.x + t * vx;
  const projY = a.y + t * vy;
  return Math.hypot(p.x - projX, p.y - projY);
}
//=============================================================================
export function findNearestConnection(
  connections: Connection[],
  point: Point,
  threshold = 10
): Connection | null {
  let best: Connection | null = null;
  let bestDist = Infinity;
  for (const conn of connections) {
    const pts = conn.points || [];
    for (let i = 0; i + 1 < pts.length; i++) {
      const d = distancePointToSegment(pts[i], pts[i + 1], point);
      if (d < bestDist) {
        bestDist = d;
        best = conn;
      }
    }
  }
  return bestDist <= threshold ? best : null;
}
//=============================================================================
export function dedupePoints(points: Point[]): Point[] {
  if (!points || points.length < 2) return points || [];
  const deduped: Point[] = [points[0]];
  for (let i = 1; i < points.length; i += 1) {
    const prev = deduped[deduped.length - 1];
    const next = points[i];
    if (prev.x === next.x && prev.y === next.y) continue;
    deduped.push(next);
  }
  return deduped;
}
//=============================================================================
export function buildStubPoint(portPos: Point, side: string): Point {
  if (side === 'left') return { x: portPos.x - 8, y: portPos.y };
  if (side === 'right') return { x: portPos.x + 8, y: portPos.y };
  if (side === 'top') return { x: portPos.x, y: portPos.y - 8 };
  return { x: portPos.x, y: portPos.y + 8 };
}
//=============================================================================
export function isValidStub(port: Point, other: Point | undefined, side: string): boolean {
  if (!other) return false;
  if (side === 'left') return other.y === port.y && other.x <= port.x - 8;
  if (side === 'right') return other.y === port.y && other.x >= port.x + 8;
  if (side === 'top') return other.x === port.x && other.y <= port.y - 8;
  return other.x === port.x && other.y >= port.y + 8;
}
//=============================================================================
export function buildOrthogonalHead(fromPos: Point, toPos: Point): Point[] {
  if (fromPos.x === toPos.x || fromPos.y === toPos.y) return [fromPos];
  return [fromPos, { x: fromPos.x, y: toPos.y }];
}
//=============================================================================
export function buildOrthogonalTail(fromPos: Point, toPos: Point): Point[] {
  if (fromPos.x === toPos.x || fromPos.y === toPos.y) return [fromPos, toPos];
  return [fromPos, { x: fromPos.x, y: toPos.y }, toPos];
}
//=============================================================================
export function hasInvalidPoint(points: Point[] | undefined): boolean {
  if (!points || points.length === 0) return false;
  return points.some((pt) => !Number.isFinite(pt.x) || !Number.isFinite(pt.y));
}
//=============================================================================
export function simplifyOrthogonalPath(points: Point[]): Point[] {
  if (!points || points.length < 3) return points.slice();
  const out: Point[] = [points[0]];
  for (let i = 1; i < points.length - 1; i += 1) {
    const a = out[out.length - 1];
    const b = points[i];
    const c = points[i + 1];
    if ((a.x === b.x && b.x === c.x) || (a.y === b.y && b.y === c.y)) {
      continue;
    }
    out.push(b);
  }
  out.push(points[points.length - 1]);
  return out;
}
//=============================================================================
export function tryShorterOrthogonalPath(conn: Connection | undefined, points: Point[]): Point[] {
  void conn;
  if (!points || points.length < 3) return points.slice();
  const simplified = simplifyOrthogonalPath(points);
  if (simplified.length <= points.length) return simplified;
  return points.slice();
}
//=============================================================================
