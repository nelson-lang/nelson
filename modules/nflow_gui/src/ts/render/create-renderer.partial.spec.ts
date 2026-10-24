//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import {
  distancePointToSegment,
  findNearestConnection,
  dedupePoints,
} from './create-renderer.partial';
//=============================================================================
describe('distancePointToSegment', () => {
  test('returns distance from point to start when projection is before segment', () => {
    const a = { x: 5, y: 0 };
    const b = { x: 10, y: 0 };
    const p = { x: 0, y: 0 };
    const d = distancePointToSegment(a, b, p);
    expect(d).toBeCloseTo(5);
  });

  test('returns distance from point to end when projection is past segment', () => {
    const a = { x: 0, y: 0 };
    const b = { x: 5, y: 0 };
    const p = { x: 10, y: 0 };
    const d = distancePointToSegment(a, b, p);
    expect(d).toBeCloseTo(5);
  });

  test('returns perpendicular distance for point near midpoint', () => {
    const a = { x: 0, y: 0 };
    const b = { x: 10, y: 0 };
    const p = { x: 5, y: 3 };
    const d = distancePointToSegment(a, b, p);
    expect(d).toBeCloseTo(3);
  });

  test('returns 0 for point on the segment', () => {
    const a = { x: 0, y: 0 };
    const b = { x: 10, y: 0 };
    const p = { x: 4, y: 0 };
    const d = distancePointToSegment(a, b, p);
    expect(d).toBeCloseTo(0);
  });

  test('handles zero-length segment (a === b)', () => {
    const a = { x: 3, y: 4 };
    const b = { x: 3, y: 4 };
    const p = { x: 0, y: 0 };
    const d = distancePointToSegment(a, b, p);
    expect(d).toBeCloseTo(5);
  });
});
//=============================================================================
describe('findNearestConnection', () => {
  test('returns null when connections list is empty', () => {
    const result = findNearestConnection([], { x: 5, y: 5 }, 10);
    expect(result).toBeNull();
  });

  test('returns connection when point is within threshold', () => {
    const conn = {
      points: [
        { x: 0, y: 0 },
        { x: 100, y: 0 },
      ],
    };
    const result = findNearestConnection([conn], { x: 50, y: 3 }, 10);
    expect(result).toBe(conn);
  });

  test('returns null when point is outside threshold', () => {
    const conn = {
      points: [
        { x: 0, y: 0 },
        { x: 100, y: 0 },
      ],
    };
    const result = findNearestConnection([conn], { x: 50, y: 50 }, 10);
    expect(result).toBeNull();
  });

  test('returns nearest connection when multiple exist', () => {
    const near = {
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
      ],
    };
    const far = {
      points: [
        { x: 0, y: 100 },
        { x: 10, y: 100 },
      ],
    };
    const result = findNearestConnection([near, far], { x: 5, y: 2 }, 10);
    expect(result).toBe(near);
  });

  test('uses default threshold of 10', () => {
    const conn = {
      points: [
        { x: 0, y: 0 },
        { x: 100, y: 0 },
      ],
    };
    const inside = findNearestConnection([conn], { x: 50, y: 9 });
    const outside = findNearestConnection([conn], { x: 50, y: 11 });
    expect(inside).toBe(conn);
    expect(outside).toBeNull();
  });
});
//=============================================================================
describe('dedupePoints', () => {
  test('returns empty array for empty input', () => {
    const result = dedupePoints([]);
    expect(result).toEqual([]);
  });

  test('returns single point unchanged', () => {
    const pts = [{ x: 1, y: 2 }];
    const result = dedupePoints(pts);
    expect(result).toEqual([{ x: 1, y: 2 }]);
  });

  test('removes consecutive duplicate points', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 0, y: 0 },
      { x: 5, y: 0 },
      { x: 5, y: 0 },
    ];
    const result = dedupePoints(pts);
    expect(result).toEqual([
      { x: 0, y: 0 },
      { x: 5, y: 0 },
    ]);
  });

  test('keeps non-consecutive duplicates', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 5, y: 0 },
      { x: 0, y: 0 },
    ];
    const result = dedupePoints(pts);
    expect(result).toHaveLength(3);
  });

  test('returns same points array reference if no duplicates', () => {
    const pts = [
      { x: 1, y: 2 },
      { x: 3, y: 4 },
      { x: 5, y: 6 },
    ];
    const result = dedupePoints(pts);
    expect(result).toHaveLength(3);
  });
});
//=============================================================================
