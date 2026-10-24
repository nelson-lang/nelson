//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: typeof import('./geometry');
beforeAll(async () => {
  mod = (await import('./geometry.ts')) as any;
});
//=============================================================================
describe('snap', () => {
  test('rounds values to nearest GRID_SIZE', () => {
    expect(mod.snapToGrid(0)).toBe(0);
    expect(mod.snapToGrid(14)).toBe(10);
    expect(mod.snapToGrid(15)).toBe(20);
    expect(mod.snapToGrid(-6)).toBe(-10);
  });
});
//=============================================================================
describe('segmentHitsRect', () => {
  test('detects vertical segment crossing rect', () => {
    const a = { x: 5, y: 2 };
    const b = { x: 5, y: 8 };
    const rect = { left: 0, right: 10, top: 0, bottom: 10 };
    expect(mod.segmentHitsRect(a, b, rect)).toBe(true);
  });

  test('returns false for vertical segment outside horizontal bounds', () => {
    const a = { x: 0, y: 2 };
    const b = { x: 0, y: 8 };
    const rect = { left: 0, right: 10, top: 0, bottom: 10 };
    // uses strict >/< so x === left is false
    expect(mod.segmentHitsRect(a, b, rect)).toBe(false);
  });

  test('detects horizontal segment crossing rect', () => {
    const a = { x: 2, y: 5 };
    const b = { x: 8, y: 5 };
    const rect = { left: 0, right: 10, top: 0, bottom: 10 };
    expect(mod.segmentHitsRect(a, b, rect)).toBe(true);
  });

  test('returns false for diagonal segments', () => {
    const a = { x: 0, y: 0 };
    const b = { x: 5, y: 5 };
    const rect = { left: -1, right: 10, top: -1, bottom: 10 };
    expect(mod.segmentHitsRect(a, b, rect)).toBe(false);
  });
});
//=============================================================================
describe('segmentLengthStats', () => {
  test('returns zeros for empty or single-point inputs', () => {
    expect(mod.segmentLengthStats()).toEqual({ length: 0, seg1: 0, seg2: 0, shortPenalty: 0 });
    expect(mod.segmentLengthStats([{ x: 0, y: 0 }])).toEqual({
      length: 0,
      seg1: 0,
      seg2: 0,
      shortPenalty: 0,
    });
  });

  test('computes segment stats across runs', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 20, y: 0 },
      { x: 20, y: 10 },
      { x: 20, y: 30 },
    ];
    const stats = mod.segmentLengthStats(pts);
    expect(stats.length).toBe(50);
    expect(stats.seg1).toBe(0);
    expect(stats.seg2).toBe(1);
    expect(stats.shortPenalty).toBe(50);
  });
});
//=============================================================================
describe('distancePointToSegment', () => {
  test('distance to endpoint when projection before segment', () => {
    const a = { x: 0, y: 0 };
    const b = { x: 10, y: 0 };
    const p = { x: -5, y: 0 };
    expect(mod.distancePointToSegment(p, a, b)).toBeCloseTo(5);
  });

  test('distance to endpoint when projection after segment', () => {
    const a = { x: 0, y: 0 };
    const b = { x: 10, y: 0 };
    const p = { x: 15, y: 0 };
    expect(mod.distancePointToSegment(p, a, b)).toBeCloseTo(5);
  });

  test('perpendicular distance to segment', () => {
    const a = { x: 0, y: 0 };
    const b = { x: 10, y: 0 };
    const p = { x: 5, y: 5 };
    expect(mod.distancePointToSegment(p, a, b)).toBeCloseTo(5);
  });

  test('degenerate zero-length segment returns distance to point a', () => {
    const a = { x: 2, y: 3 };
    const b = { x: 2, y: 3 };
    const p = { x: 5, y: 7 };
    const expected = Math.hypot(p.x - a.x, p.y - a.y);
    expect(mod.distancePointToSegment(p, a, b)).toBeCloseTo(expected);
  });
});
//=============================================================================
