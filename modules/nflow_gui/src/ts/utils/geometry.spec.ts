//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment node */
let geom: any;
beforeAll(async () => {
  geom = await import('./geometry.ts');
});
//=============================================================================
test('constants and snap behavior', () => {
  expect(geom.GRID_SIZE).toBe(10);
  expect(geom.KEEP_OUT).toBe(geom.GRID_SIZE);
  expect(geom.snapToGrid(7)).toBe(10);
  expect(geom.snapToGrid(4)).toBe(0);
  expect(geom.snapToGrid(15)).toBe(20);
});
//=============================================================================
test('normalizeWorldPosition preserves signed coordinates', () => {
  expect(geom.normalizeWorldPosition({ x: -40, y: -20 })).toEqual({ x: -40, y: -20 });
  expect(geom.normalizeWorldPosition({ x: '15', y: '-25' })).toEqual({ x: 15, y: -25 });
  expect(geom.normalizeWorldPosition({ x: Infinity, y: NaN }, { x: -5, y: 7 })).toEqual({
    x: -5,
    y: 7,
  });
});
//=============================================================================
test('segmentHitsRect vertical/horizontal/diagonal and boundaries', () => {
  const rect = { left: 10, right: 30, top: 10, bottom: 30 };
  expect(geom.segmentHitsRect({ x: 20, y: 0 }, { x: 20, y: 40 }, rect)).toBe(true);
  expect(geom.segmentHitsRect({ x: 5, y: 0 }, { x: 5, y: 40 }, rect)).toBe(false);
  expect(geom.segmentHitsRect({ x: 0, y: 0 }, { x: 10, y: 10 }, rect)).toBe(false);
  expect(geom.segmentHitsRect({ x: 10, y: 0 }, { x: 10, y: 40 }, rect)).toBe(false);
  expect(geom.segmentHitsRect({ x: 30, y: 0 }, { x: 30, y: 40 }, rect)).toBe(false);
  expect(geom.segmentHitsRect({ x: 0, y: 10 }, { x: 40, y: 10 }, rect)).toBe(false);
  expect(geom.segmentHitsRect({ x: 0, y: 30 }, { x: 40, y: 30 }, rect)).toBe(false);
});
//=============================================================================
test('distancePointToSegment projection, endpoints and zero-length', () => {
  const a = { x: 0, y: 0 };
  const b = { x: 10, y: 0 };
  const d1 = geom.distancePointToSegment({ x: 5, y: 5 }, a, b);
  expect(d1).toBeCloseTo(5);
  const d2 = geom.distancePointToSegment({ x: -5, y: 0 }, a, b);
  expect(d2).toBeCloseTo(5);
  const d3 = geom.distancePointToSegment({ x: 20, y: 0 }, a, b);
  expect(d3).toBeCloseTo(10);
  const a2 = { x: 5, y: 5 };
  const b2 = { x: 5, y: 5 };
  const d4 = geom.distancePointToSegment({ x: 10, y: 5 }, a2, b2);
  expect(d4).toBeCloseTo(5);
});
//=============================================================================
test('segmentLengthStats handles short/invalid and counts seg1/seg2', () => {
  expect(geom.segmentLengthStats([])).toEqual({ length: 0, seg1: 0, seg2: 0, shortPenalty: 0 });
  expect(geom.segmentLengthStats([{ x: 0, y: 0 }])).toEqual({
    length: 0,
    seg1: 0,
    seg2: 0,
    shortPenalty: 0,
  });
  const pts = [
    { x: 0, y: 0 },
    { x: 0, y: 10 },
    { x: 20, y: 10 },
  ];
  const stats = geom.segmentLengthStats(pts);
  expect(stats.length).toBeGreaterThan(0);
  expect(stats.seg1 + stats.seg2).toBeGreaterThanOrEqual(1);
  expect(typeof stats.shortPenalty).toBe('number');
});
//=============================================================================
test('segmentLengthStats handles diagonal segments (skips non-orthogonal)', () => {
  const pts = [
    { x: 0, y: 0 },
    { x: 5, y: 5 },
    { x: 10, y: 10 },
  ];
  const stats = geom.segmentLengthStats(pts);
  expect(stats).toBeDefined();
  expect(typeof stats.length).toBe('number');
  expect(typeof stats.seg1).toBe('number');
  expect(typeof stats.seg2).toBe('number');
});
//=============================================================================
describe('type contracts', () => {
  test('snap returns number and GRID_SIZE is numeric', () => {
    expect(typeof geom.snapToGrid(5)).toBe('number');
    expect(typeof geom.GRID_SIZE).toBe('number');
  });

  test('segmentLengthStats returns numeric fields', () => {
    const s = geom.segmentLengthStats([]);
    expect(typeof s.length).toBe('number');
    expect(typeof s.seg1).toBe('number');
    expect(typeof s.seg2).toBe('number');
  });
});
//=============================================================================
