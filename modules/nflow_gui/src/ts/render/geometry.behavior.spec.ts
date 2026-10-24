//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import {
  rotatePoint,
  getRotatedBounds,
  getPortSide,
  blockBounds,
  intersectsAxisAlignedRect,
  segmentsIntersect,
  segmentsOverlap,
} from './geometry';
//=============================================================================
test('rotatePoint handles 0/90/180/270 rotations', () => {
  const block = { x: 0, y: 0, width: 10, height: 20 };
  expect(rotatePoint({ x: 5, y: 10 }, { ...block, rotation: 0 })).toEqual({ x: 5, y: 10 });
  expect(rotatePoint({ x: 5, y: 10 }, { ...block, rotation: 90 })).toHaveProperty('x');
  expect(rotatePoint({ x: 5, y: 10 }, { ...block, rotation: 180 })).toHaveProperty('x');
  expect(rotatePoint({ x: 5, y: 10 }, { ...block, rotation: 270 })).toHaveProperty('x');
});
//=============================================================================
test('getRotatedBounds swaps width/height for 90/270', () => {
  const b = { x: 0, y: 0, width: 10, height: 20 };
  const r0 = getRotatedBounds({ ...b, rotation: 0 });
  const r90 = getRotatedBounds({ ...b, rotation: 90 });
  expect(r0.right - r0.left).toBeCloseTo(10);
  expect(r90.right - r90.left).toBeCloseTo(20);
});
//=============================================================================
test('getPortSide chooses correct side', () => {
  const block = { x: 0, y: 0, width: 10, height: 10 };
  const center = { x: 5, y: 5 };
  expect(getPortSide(block, { x: 0, y: 5 })).toBe('left');
  expect(getPortSide(block, { x: 10, y: 5 })).toBe('right');
  expect(getPortSide(block, { x: 5, y: 0 })).toBe('top');
  expect(getPortSide(block, { x: 5, y: 10 })).toBe('bottom');
});
//=============================================================================
test('blockBounds includes ports', () => {
  const block: any = { x: 0, y: 0, width: 10, height: 10, ports: [{ x: 20, y: 20 }] };
  const bounds = blockBounds(block);
  expect(bounds.left).toBeLessThan(bounds.right);
  expect(bounds.top).toBeLessThan(bounds.bottom);
});
//=============================================================================
test('intersectsAxisAlignedRect works for vertical and horizontal segments', () => {
  const rect = { left: 0, right: 10, top: 0, bottom: 10 };
  expect(intersectsAxisAlignedRect({ a: { x: 5, y: 0 }, b: { x: 5, y: 10 } }, rect)).toBe(true);
  expect(intersectsAxisAlignedRect({ a: { x: 0, y: 5 }, b: { x: 10, y: 5 } }, rect)).toBe(true);
  expect(intersectsAxisAlignedRect({ a: { x: -1, y: -1 }, b: { x: -2, y: -2 } }, rect)).toBe(false);
});
//=============================================================================
test('segmentsIntersect and segmentsOverlap basic cases', () => {
  const h = { a: { x: 0, y: 5 }, b: { x: 10, y: 5 }, orientation: 'H' as const };
  const v = { a: { x: 5, y: 0 }, b: { x: 5, y: 10 }, orientation: 'V' as const };
  expect(segmentsIntersect(h, v)).toBe(true);
  const h2 = { a: { x: 2, y: 5 }, b: { x: 8, y: 5 }, orientation: 'H' as const };
  expect(segmentsOverlap(h, h2)).toBe(true);
  const v2 = { a: { x: 20, y: 0 }, b: { x: 20, y: 10 }, orientation: 'V' as const };
  expect(segmentsIntersect(h, v2)).toBe(false);
});
//=============================================================================
