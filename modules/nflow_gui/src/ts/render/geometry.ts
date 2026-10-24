//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { GRID_SIZE } from '../utils/geometry.js';
//=============================================================================
export {
  GRID_SIZE,
  KEEP_OUT,
  snapToGrid,
  distancePointToSegment,
  normalizeWorldPosition,
  segmentLengthStats,
} from '../utils/geometry.js';
//=============================================================================
export type Point = { x: number; y: number };
export type Rect = { left: number; right: number; top: number; bottom: number };
export type Segment = { a: Point; b: Point };
export type Block = {
  x: number;
  y: number;
  width: number;
  height: number;
  rotation?: number;
  ports?: Array<{ x: number; y: number }>;
};
/**
 * Rotate a point around the center of a block by block.rotation (degrees).
 * Multiples of 90 are handled explicitly for pixel-aligned results.
 */
export const rotatePoint = (point: Point, block: Block): Point => {
  const raw = Number(block.rotation ?? 0);
  const norm = ((raw % 360) + 360) % 360;
  const w = Number(block.width ?? 0);
  const h = Number(block.height ?? 0);
  const centerX = Number(block.x) + w / 2;
  const centerY = Number(block.y) + h / 2;
  const relX = point.x - centerX;
  const relY = point.y - centerY;

  const snapQuarter = Math.round(norm / 90) % 4;
  // Handle exact 90-degree increments with explicit transforms for pixel stability.
  switch (snapQuarter) {
    case 0:
      if (norm % 90 === 0) return { x: centerX + relX, y: centerY + relY };
      break;
    case 1:
      if (norm % 90 === 0) return { x: centerX - relY, y: centerY + relX };
      break;
    case 2:
      if (norm % 90 === 0) return { x: centerX - relX, y: centerY - relY };
      break;
    case 3:
      if (norm % 90 === 0) return { x: centerX + relY, y: centerY - relX };
      break;
  }

  const theta = (norm * Math.PI) / 180;
  const cosT = Math.cos(theta);
  const sinT = Math.sin(theta);
  const rx = cosT * relX - sinT * relY;
  const ry = sinT * relX + cosT * relY;
  return { x: Math.round(centerX + rx), y: Math.round(centerY + ry) };
};
/** Axis-aligned bounding box of a block after its rotation is applied. */
export const getRotatedBounds = (block: Block): Rect => {
  const rawCorners = [
    { x: block.x, y: block.y },
    { x: block.x + block.width, y: block.y },
    { x: block.x, y: block.y + block.height },
    { x: block.x + block.width, y: block.y + block.height },
  ];
  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;
  for (const c of rawCorners) {
    const p = rotatePoint(c, block);
    if (p.x < minX) minX = p.x;
    if (p.x > maxX) maxX = p.x;
    if (p.y < minY) minY = p.y;
    if (p.y > maxY) maxY = p.y;
  }
  return { left: minX, right: maxX, top: minY, bottom: maxY };
};
/** Side of a block a rotated port lies on: 'left', 'right', 'top' or 'bottom'. */
export const getPortSide = (
  block: { x: number; y: number; width: number; height: number },
  rotatedPoint: Point
): 'left' | 'right' | 'top' | 'bottom' => {
  const bx = Number(block.x);
  const by = Number(block.y);
  const bw = Number(block.width ?? 0);
  const bh = Number(block.height ?? 0);
  const centerX = bx + bw / 2;
  const centerY = by + bh / 2;
  const relX = rotatedPoint.x - centerX;
  const relY = rotatedPoint.y - centerY;
  // Compare normalized coordinates so larger blocks bias appropriately.
  const nx = Math.abs(relX) / (bw / 2 || 1);
  const ny = Math.abs(relY) / (bh / 2 || 1);
  if (nx >= ny) return relX < 0 ? 'left' : 'right';
  return relY < 0 ? 'top' : 'bottom';
};
/** Visual bounding rectangle of a block, including port radii (hit testing, layout). */
export const blockBounds = (block: Block): Rect => {
  const PORT_RADIUS = 6;
  const base = getRotatedBounds(block);
  let left = base.left;
  let right = base.right;
  let top = base.top;
  let bottom = base.bottom;
  const ports = block.ports ?? [];
  for (let i = 0; i < ports.length; i += 1) {
    const p = ports[i];
    const pos = rotatePoint({ x: block.x + p.x, y: block.y + p.y }, block);
    const l = pos.x - PORT_RADIUS;
    const r = pos.x + PORT_RADIUS;
    const t = pos.y - PORT_RADIUS;
    const b = pos.y + PORT_RADIUS;
    if (l < left) left = l;
    if (r > right) right = r;
    if (t < top) top = t;
    if (b > bottom) bottom = b;
  }
  return { left, right, top, bottom };
};
/**
 * Test whether an axis-aligned segment AB overlaps an axis-aligned rect.
 * Only horizontal or vertical segments are supported.
 */
export const segmentHitsRect = (a: Point, b: Point, rect: Rect): boolean => {
  const isHoriz = a.y === b.y;
  const isVert = a.x === b.x;
  if (!isHoriz && !isVert) return false;
  const sLeft = Math.min(a.x, b.x);
  const sRight = Math.max(a.x, b.x);
  const sTop = Math.min(a.y, b.y);
  const sBottom = Math.max(a.y, b.y);

  const intersectsRange = (p1: number, p2: number, q1: number, q2: number) => !(p2 < q1 || p1 > q2);

  return (
    intersectsRange(sLeft, sRight, rect.left, rect.right) &&
    intersectsRange(sTop, sBottom, rect.top, rect.bottom)
  );
};
export const intersectsAxisAlignedRect = (seg: Segment, rect: Rect): boolean =>
  segmentHitsRect(seg.a, seg.b, rect);
export const alignToGrid = (p: Point): Point => {
  const g = Number(GRID_SIZE || 10);
  return { x: Math.round(p.x / g) * g, y: Math.round(p.y / g) * g };
};
/**
 * Offset, from the block origin, of the port that block placement snaps on:
 * a port on the block's centre line when there is one, else the first input,
 * else the first output. Rotation is applied, so the offset is in world axes.
 * Null for a block without ports.
 */
export const blockPortAnchor = (block: {
  x: number;
  y: number;
  width: number;
  height: number;
  rotation?: number;
  ports?: Array<{ x: number; y: number; type?: string; index?: number }>;
}): Point | null => {
  const ports = (block.ports || []).filter((p) => p.type === 'in' || p.type === 'out');
  if (!ports.length) return null;
  const centred = ports.find((p) => p.y === block.height / 2 || p.x === block.width / 2);
  const port = centred ?? ports.find((p) => p.type === 'in') ?? ports[0];
  const world = rotatePoint({ x: block.x + port.x, y: block.y + port.y }, block);
  return { x: world.x - block.x, y: world.y - block.y };
};
/**
 * Snap a block origin so that its anchor port lands on the grid. Blocks come
 * in heights such as 70 whose centred port sits between two grid lines, so
 * snapping the corner would leave that port 5 px off the line every
 * neighbour's port sits on; anchoring the snap on the port keeps facing ports
 * exactly in line and their wire straight.
 */
export const alignBlockToGrid = (origin: Point, anchor: Point | null): Point => {
  if (!anchor) return alignToGrid(origin);
  const g = alignToGrid({ x: origin.x + anchor.x, y: origin.y + anchor.y });
  return { x: g.x - anchor.x, y: g.y - anchor.y };
};
//=============================================================================
export type WireSeg = { a: Point; b: Point; orientation: 'H' | 'V' };
/**
 * Return true when a horizontal and a vertical segment intersect.
 * Both segments must be strictly axis-aligned; parallel segments return false.
 */
export const segmentsIntersect = (
  segA: WireSeg & { a: Point; b: Point },
  segB: WireSeg & { a: Point; b: Point }
): boolean => {
  const orient = (s: WireSeg) => (s.a.y === s.b.y ? 'H' : 'V');
  const oA = orient(segA);
  const oB = orient(segB);
  if (oA === oB) return false;
  const horiz = oA === 'H' ? segA : segB;
  const vert = oA === 'H' ? segB : segA;
  const hx1 = Math.min(horiz.a.x, horiz.b.x);
  const hx2 = Math.max(horiz.a.x, horiz.b.x);
  const vy1 = Math.min(vert.a.y, vert.b.y);
  const vy2 = Math.max(vert.a.y, vert.b.y);
  const crossX = vert.a.x;
  const crossY = horiz.a.y;
  return crossX > hx1 && crossX < hx2 && crossY > vy1 && crossY < vy2;
};
/**
 * Determine whether two colinear axis-aligned segments overlap.
 * Returns true if they share at least one point.
 */
export const segmentsOverlap = (
  a: WireSeg & { orientation?: 'H' | 'V' },
  b: WireSeg & { orientation?: 'H' | 'V' }
): boolean => {
  const getOrient = (s: WireSeg) => s.orientation ?? (s.a.y === s.b.y ? 'H' : 'V');
  const oA = getOrient(a);
  const oB = getOrient(b);
  if (oA !== oB) return false;
  const intersects1d = (u1: number, u2: number, v1: number, v2: number) =>
    Math.max(Math.min(u1, u2), Math.min(v1, v2)) <= Math.min(Math.max(u1, u2), Math.max(v1, v2));
  if (oA === 'H') {
    if (a.a.y !== b.a.y) return false;
    return intersects1d(a.a.x, a.b.x, b.a.x, b.b.x);
  }
  if (a.a.x !== b.a.x) return false;
  return intersects1d(a.a.y, a.b.y, b.a.y, b.b.y);
};
//=============================================================================
