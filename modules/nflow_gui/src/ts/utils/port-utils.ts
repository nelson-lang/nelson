//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type Point = { x: number; y: number };
export type Block = {
  x: number;
  y: number;
  width: number;
  height: number;
  rotation?: number;
  ports?: Array<{ x: number; y: number }>;
};
//=============================================================================
function normalizeAngle(rotation?: number) {
  return (((rotation || 0) % 360) + 360) % 360;
}
//=============================================================================
export function rotatePoint(point: Point, block: Block): Point {
  const angle = normalizeAngle(block.rotation);
  const cx = block.x + block.width / 2;
  const cy = block.y + block.height / 2;
  const dx = point.x - cx;
  const dy = point.y - cy;

  // Only 0/90/180/270 are transformed; any other angle returns the point as is.
  const matrices: Record<number, [number, number, number, number]> = {
    0: [1, 0, 0, 1],
    90: [0, -1, 1, 0],
    180: [-1, 0, 0, -1],
    270: [0, 1, -1, 0],
  };

  const mat = matrices[angle as number];
  if (!mat) return point;
  const [m00, m01, m10, m11] = mat;
  return {
    x: cx + dx * m00 + dy * m01,
    y: cy + dx * m10 + dy * m11,
  };
}
export function getRotatedBounds(block: Block) {
  const corners = [
    { x: block.x, y: block.y },
    { x: block.x + block.width, y: block.y },
    { x: block.x + block.width, y: block.y + block.height },
    { x: block.x, y: block.y + block.height },
  ].map((c) => rotatePoint(c, block));

  let left = Infinity;
  let right = -Infinity;
  let top = Infinity;
  let bottom = -Infinity;

  for (const c of corners) {
    if (c.x < left) left = c.x;
    if (c.x > right) right = c.x;
    if (c.y < top) top = c.y;
    if (c.y > bottom) bottom = c.y;
  }

  return { left, right, top, bottom };
}
export function getPortSide(
  block: Block,
  rotatedPoint: Point
): 'left' | 'right' | 'top' | 'bottom' {
  const center = { x: block.x + block.width / 2, y: block.y + block.height / 2 };
  const dx = rotatedPoint.x - center.x;
  const dy = rotatedPoint.y - center.y;

  const absDx = Math.abs(dx);
  const absDy = Math.abs(dy);
  if (absDx >= absDy) return dx < 0 ? 'left' : 'right';
  return dy < 0 ? 'top' : 'bottom';
}
export function sideToDir(side: string) {
  const mapping: Record<string, 'left' | 'right' | 'up' | 'down'> = {
    left: 'left',
    right: 'right',
    top: 'up',
  };
  return mapping[side] ?? 'down';
}
export default { rotatePoint, getPortSide, getRotatedBounds, sideToDir };
//=============================================================================
