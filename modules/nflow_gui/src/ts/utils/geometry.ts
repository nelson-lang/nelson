//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/**
 * Grid cell size used throughout the router and renderer.
 * Coordinates are snapped to multiples of this value.
 */
export const GRID_SIZE = 10;
/**
 * Recommended minimum keep-out distance (alias of `GRID_SIZE`).
 */
export const KEEP_OUT = GRID_SIZE;
/**
 * Snap a numeric coordinate to the router grid using a stable rounding rule.
 * @param value - coordinate in pixels
 * @returns snapped coordinate aligned to `GRID_SIZE`
 */
export function snapToGrid(value: number): number {
  // Use an explicit rounding strategy (preserves behavior for negative numbers)
  const ratio = value / GRID_SIZE;
  const rounded = ratio >= 0 ? Math.floor(ratio + 0.5) : Math.ceil(ratio - 0.5);
  return rounded * GRID_SIZE;
}
//=============================================================================
export function segmentHitsRect(
  a: { x: number; y: number },
  b: { x: number; y: number },
  rect: { left: number; right: number; top: number; bottom: number }
): boolean {
  // Only axis-aligned segments are considered.
  const vx = b.x - a.x;
  const vy = b.y - a.y;
  const segMinX = Math.min(a.x, b.x);
  const segMaxX = Math.max(a.x, b.x);
  const segMinY = Math.min(a.y, b.y);
  const segMaxY = Math.max(a.y, b.y);

  if (vx === 0) {
    if (!(a.x > rect.left && a.x < rect.right)) return false;
    return intervalOverlapStrict(segMinY, segMaxY, rect.top, rect.bottom);
  }
  if (vy === 0) {
    if (!(a.y > rect.top && a.y < rect.bottom)) return false;
    return intervalOverlapStrict(segMinX, segMaxX, rect.left, rect.right);
  }
  return false;
}
//=============================================================================
function intervalOverlapStrict(a1: number, a2: number, b1: number, b2: number): boolean {
  return Math.max(a1, b1) < Math.min(a2, b2);
}
/**
 * Compute basic length statistics for an orthogonal polyline.
 * Returns total length plus counts of short segments useful for penalties.
 * @param points - ordered list of grid points
 * @returns object with `length`, `seg1`, `seg2`, and `shortPenalty`
 */
export function segmentLengthStats(points?: Array<{ x: number; y: number }>) {
  const stats = { length: 0, seg1: 0, seg2: 0, shortPenalty: 0 };
  if (!points || points.length < 2) return stats;

  const runs: number[] = [];
  let currentDir: null | 'H' | 'V' = null;
  let currentLen = 0;

  for (let i = 0; i < points.length - 1; i += 1) {
    const p0 = points[i];
    const p1 = points[i + 1];
    const dx = p1.x - p0.x;
    const dy = p1.y - p0.y;
    if (dx === 0 && dy === 0) continue;
    const dir = dx === 0 ? 'V' : dy === 0 ? 'H' : null;
    if (!dir) continue;
    const segLen = Math.abs(dx) + Math.abs(dy);
    if (currentDir === dir) {
      currentLen += segLen;
    } else {
      if (currentDir !== null) runs.push(currentLen);
      currentDir = dir;
      currentLen = segLen;
    }
  }
  if (currentDir !== null) runs.push(currentLen);

  let total = 0;
  for (let j = 0; j < runs.length; j += 1) {
    const r = runs[j];
    total += r;
    const gridLen = Math.round(r / GRID_SIZE);
    if (gridLen === 1) stats.seg1++;
    if (gridLen === 2) stats.seg2++;
  }
  stats.length = total;
  stats.shortPenalty = stats.seg1 * 60 + stats.seg2 * 50;
  return stats;
}
/**
 * Compute the shortest Euclidean distance from a point to a segment AB.
 * Supports non-axis aligned segments and clamps projection to segment.
 * @param point - point to measure
 * @param a - segment start
 * @param b - segment end
 * @returns distance in same units as inputs
 */
export function distancePointToSegment(
  point: { x: number; y: number },
  a: { x: number; y: number },
  b: { x: number; y: number }
): number {
  const vx = b.x - a.x;
  const vy = b.y - a.y;
  const wx = point.x - a.x;
  const wy = point.y - a.y;
  const len2 = vx * vx + vy * vy;
  if (len2 === 0) return Math.hypot(wx, wy);
  const t = (wx * vx + wy * vy) / len2;
  if (t <= 0) return Math.hypot(point.x - a.x, point.y - a.y);
  if (t >= 1) return Math.hypot(point.x - b.x, point.y - b.y);
  const px = a.x + t * vx;
  const py = a.y + t * vy;
  return Math.hypot(point.x - px, point.y - py);
}
//=============================================================================
export type Point = { x: number; y: number };
export type Rect = { left: number; right: number; top: number; bottom: number };
export type Segment = { a: Point; b: Point };
/**
 * Normalize a world-space coordinate without constraining it to the positive quadrant.
 * Invalid coordinates fall back independently so existing valid axes are preserved.
 */
export function normalizeWorldPosition(
  p: { x: unknown; y: unknown },
  fallback: Point = { x: 0, y: 0 }
): Point {
  const x = Number(p.x);
  const y = Number(p.y);
  const fx = Number(fallback.x);
  const fy = Number(fallback.y);
  return {
    x: Number.isFinite(x) ? x : Number.isFinite(fx) ? fx : 0,
    y: Number.isFinite(y) ? y : Number.isFinite(fy) ? fy : 0,
  };
}
export function alignToGrid(p: Point): Point {
  return { x: snapToGrid(p.x), y: snapToGrid(p.y) };
}
export function intersectsAxisAlignedRect(seg: Segment, rect: Rect): boolean {
  return segmentHitsRect(seg.a, seg.b, rect);
}
//=============================================================================
