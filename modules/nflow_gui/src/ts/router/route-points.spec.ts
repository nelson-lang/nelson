//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, test } from 'vitest';
import {
  cloneRoutePoints,
  captureRoutePointsSnapshot,
  applyRoutePointsSnapshot,
} from './route-points';
//=============================================================================
describe('cloneRoutePoints', () => {
  test('returns null for null or empty input', () => {
    expect(cloneRoutePoints(null as any)).toBeNull();
    expect(cloneRoutePoints([])).toBeNull();
  });

  test('clones array-style points [x, y] to {x, y} objects', () => {
    const result = cloneRoutePoints([
      [0, 0],
      [10, 10],
    ] as any);
    expect(result).toEqual([
      { x: 0, y: 0 },
      { x: 10, y: 10 },
    ]);
  });

  test('clones object-style points {x, y}', () => {
    const result = cloneRoutePoints([
      { x: 0, y: 0 },
      { x: 5, y: 5 },
    ]);
    expect(result).toEqual([
      { x: 0, y: 0 },
      { x: 5, y: 5 },
    ]);
  });

  test('filters out points with non-numeric coordinates', () => {
    const result = cloneRoutePoints([{ x: 'a', y: 1 }] as any);
    expect(result).toBeNull();
  });

  test('handles mixed valid entries and keeps valid ones', () => {
    const arr = [[1, 2], { x: 3, y: 4 }] as any;
    const result = cloneRoutePoints(arr);
    expect(Array.isArray(result)).toBe(true);
    expect(result!.length).toBe(2);
    expect(result![0]).toEqual({ x: 1, y: 2 });
    expect(result![1]).toEqual({ x: 3, y: 4 });
  });
});
//=============================================================================
describe('captureRoutePointsSnapshot and applyRoutePointsSnapshot', () => {
  test('captures byIndex snapshot with correct length', () => {
    const conns = [
      {
        points: [
          [1, 1],
          [2, 2],
        ],
        id: 'a',
      },
      {
        points: [
          { x: 3, y: 3 },
          { x: 4, y: 4 },
        ],
        id: 'b',
      },
    ] as any;
    const snap = captureRoutePointsSnapshot(conns, (c: any) => String(c.id));
    expect(Array.isArray(snap.byIndex)).toBe(true);
    expect(snap.byIndex.length).toBe(2);
  });

  test('applyRoutePointsSnapshot byIndex restores points', () => {
    const conns = [
      {
        points: [
          [1, 1],
          [2, 2],
        ],
      },
      {
        points: [
          { x: 3, y: 3 },
          { x: 4, y: 4 },
        ],
      },
    ] as any;
    const keyFn = (c: any) => `${c.points?.length}`;
    const snap = captureRoutePointsSnapshot(conns, keyFn);
    const applied = applyRoutePointsSnapshot(conns, snap, keyFn);
    expect(snap.byIndex.length).toBe(2);
    expect(applied).toBe(2);
    expect(Array.isArray(conns[0].points)).toBe(true);
  });

  test('applyRoutePointsSnapshot byKey applies queued snapshots FIFO', () => {
    const conns = [
      { id: 1, from: 1, to: 2, points: [] },
      { id: 2, from: 1, to: 2, points: [] },
    ] as any;
    const keyFn = (c: any) => `${c.from}:${c.to}`;
    const snap = {
      byKey: {
        '1:2': [
          [
            [1, 1],
            [2, 2],
          ],
          [
            [3, 3],
            [4, 4],
          ],
        ],
      },
    } as any;
    const applied = applyRoutePointsSnapshot(conns, snap, keyFn);
    expect(applied).toBe(2);
    expect(conns[0].points).toEqual([
      { x: 1, y: 1 },
      { x: 2, y: 2 },
    ]);
    expect(conns[1].points).toEqual([
      { x: 3, y: 3 },
      { x: 4, y: 4 },
    ]);
  });
});
//=============================================================================
