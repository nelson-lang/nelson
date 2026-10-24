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
// Wire endpoints on the exact port. The router works on a 10 px grid and starts
// paths on the grid-snapped port positions, but a port often sits between two
// grid lines (the inputs of a two-input block are 12 px above and below its
// centre). This pure pass moves the first and last runs of a routed path onto
// the exact port coordinates. Aligned ports give one straight line, a slightly
// offset pair gets one mid-span jog, any other path shifts its end runs by a few
// pixels and stays orthogonal.
//=============================================================================
import { GRID_SIZE } from '../geometry.js';
//=============================================================================
export type Point = { x: number; y: number };
export type PortSide = 'left' | 'right' | 'top' | 'bottom';
const horizontal = (side: PortSide) => side === 'left' || side === 'right';
const same = (a: Point, b: Point) => a.x === b.x && a.y === b.y;
/**
 * @param points routed path, first/last points on the grid-snapped ports
 * @param fromPos exact position of the source port
 * @param fromSide side of the block the source port leaves from
 * @param toPos exact position of the target port
 * @param toSide side of the block the target port enters
 */
export function desnapEndpoints(
  points: Point[],
  fromPos: Point,
  fromSide: PortSide,
  toPos: Point,
  toSide: PortSide
): Point[] {
  if (!Array.isArray(points) || points.length < 2) return points;
  const first = points[0];
  const last = points[points.length - 1];
  // Only close small snapping gaps; anything larger is not a grid artefact.
  const near = (a: Point, b: Point) =>
    Math.abs(a.x - b.x) < GRID_SIZE && Math.abs(a.y - b.y) < GRID_SIZE;
  if (!near(first, fromPos) || !near(last, toPos)) return points;
  const from = { x: fromPos.x, y: fromPos.y };
  const to = { x: toPos.x, y: toPos.y };

  if (points.length === 2) {
    if (horizontal(fromSide) && horizontal(toSide)) {
      if (from.y === to.y) return [from, to];
      const mx = Math.round((from.x + to.x) / 2 / GRID_SIZE) * GRID_SIZE;
      return [from, { x: mx, y: from.y }, { x: mx, y: to.y }, to];
    }
    if (!horizontal(fromSide) && !horizontal(toSide)) {
      if (from.x === to.x) return [from, to];
      const my = Math.round((from.y + to.y) / 2 / GRID_SIZE) * GRID_SIZE;
      return [from, { x: from.x, y: my }, { x: to.x, y: my }, to];
    }
    return points;
  }

  const pts = points.map((p) => ({ x: p.x, y: p.y }));
  const n = pts.length;
  if (horizontal(fromSide)) {
    if (pts[1].y === first.y) pts[1].y = from.y;
  } else if (pts[1].x === first.x) pts[1].x = from.x;
  pts[0] = from;
  if (horizontal(toSide)) {
    if (pts[n - 2].y === last.y) pts[n - 2].y = to.y;
  } else if (pts[n - 2].x === last.x) pts[n - 2].x = to.x;
  pts[n - 1] = to;
  // A three-point L whose corner serves both runs must stay orthogonal.
  for (let i = 0; i + 1 < pts.length; i += 1) {
    if (pts[i].x !== pts[i + 1].x && pts[i].y !== pts[i + 1].y) return points;
  }
  return pts.filter((p, i) => i === 0 || !same(p, pts[i - 1]));
}
//=============================================================================
