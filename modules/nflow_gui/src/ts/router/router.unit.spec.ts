//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  computeWireRoutes,
  buildBlockOccupancyMap,
  buildObstacleNearMap,
  buildWireNearMap,
  routeAllConnections,
  routeDirtyConnections,
  normalizeConnectionJunctions,
  analyzeConnectionGeometry,
} from './router.js';
//=============================================================================
function cellIndex(x: number, y: number, minX: number, minY: number, gridW: number) {
  return (y - minY) * gridW + (x - minX);
}
//=============================================================================
describe('router.ts - unit tests', () => {
  describe('buildBlockOccupancyMap', () => {
    test('returns null for empty obstacles', () => {
      expect(buildBlockOccupancyMap([])).toBeNull();
    });

    test('creates grid set for single-cell obstacle', () => {
      const grid = buildBlockOccupancyMap([{ x0: 1, y0: 1, x1: 1, y1: 1 }]);
      expect(grid).toBeInstanceOf(Set);
      const expectedKey = ((1 & 0xffff) << 16) | (1 & 0xffff);
      expect(grid.has(expectedKey)).toBe(true);
    });
  });

  describe('buildObstacleNearMap', () => {
    test('marks direct and distance-2 neighbors correctly', () => {
      const grid = buildBlockOccupancyMap([{ x0: 2, y0: 2, x1: 2, y1: 2 }]);
      const bounds = { minX: 0, minY: 0, maxX: 4, maxY: 4 };
      const map = buildObstacleNearMap(grid, bounds);
      expect(map).toBeInstanceOf(Uint8Array);
      const gridW = bounds.maxX - bounds.minX + 1;
      const center = cellIndex(2, 2, bounds.minX, bounds.minY, gridW);
      expect(map[center]).toBe(1); // obstacle or immediate neighbor -> obs1
      const distant = cellIndex(4, 2, bounds.minX, bounds.minY, gridW);
      expect(map[distant]).toBe(2); // distance-2 neighbor -> obs2
    });
  });

  describe('buildWireNearMap', () => {
    test('marks nearby cells with 1 and farther cells with 2', () => {
      const occupancy = { points: new Map([['2,3', {}]]), edges: new Map() } as any;
      const bounds = { minX: 0, minY: 0, maxX: 5, maxY: 5 } as any;
      const map = buildWireNearMap(occupancy, bounds) as Uint8Array;
      expect(map).toBeInstanceOf(Uint8Array);
      const gridW = bounds.maxX - bounds.minX + 1;
      expect(map[cellIndex(3, 3, bounds.minX, bounds.minY, gridW)]).toBe(1);
      expect(map[cellIndex(4, 3, bounds.minX, bounds.minY, gridW)]).toBe(2);
    });
  });

  describe('computeWireRoutes', () => {
    test('reports failure for connections when nodes are missing', () => {
      const res = computeWireRoutes({
        nodes: [],
        connections: [{ from: 'a', to: 'b' }],
        obstacles: [],
      });
      // When nodes are missing the implementation increments the failed count
      // but does not push into the failures array for this branch.
      expect(res.cost.failed).toBeGreaterThan(0);
      expect(res.failures && res.failures.length).toBe(0);
    });

    test('re-uses prevSolution wires when incremental and not fullOptimize', () => {
      const prevSolution = { wires: { k1: { points: [{ x: 1, y: 1 }] } } } as any;
      const res = computeWireRoutes({
        nodes: [],
        connections: [{ key: 'k1', from: 'A', to: 'B' }],
        obstacles: [],
        prevSolution,
        settings: { incremental: true, fullOptimize: false },
      });
      expect(res.wires instanceof Map).toBe(true);
      expect(res.wires.get('k1')).toBe(prevSolution.wires.k1);
    });
  });

  describe('analyzeConnectionGeometry', () => {
    test('detects non-orthogonal segments', () => {
      const input = [
        {
          from: 'a',
          to: 'b',
          points: [
            { x: 0, y: 0 },
            { x: 1, y: 1 },
          ],
        },
      ];
      const out = analyzeConnectionGeometry(input as any);
      expect(out.totals.nonOrthogonal).toBeGreaterThan(0);
      expect(out.issues.nonOrthogonal.length).toBeGreaterThan(0);
    });

    test('detects parallel overlaps between connections', () => {
      const a = {
        from: 'A',
        to: 'X',
        points: [
          { x: 0, y: 0 },
          { x: 4, y: 0 },
        ],
      };
      const b = {
        from: 'B',
        to: 'Y',
        points: [
          { x: 2, y: 0 },
          { x: 6, y: 0 },
        ],
      };
      const out = analyzeConnectionGeometry([a as any, b as any]);
      expect(out.totals.overlaps).toBeGreaterThan(0);
      expect(out.issues.overlaps.length).toBeGreaterThan(0);
    });
  });

  describe('higher-level helpers', () => {
    test('routeAllConnections returns an empty Map for minimal state', () => {
      const state: any = { blocks: new Map(), connections: [] };
      const paths = routeAllConnections(state, 20, 20);
      expect(paths instanceof Map).toBe(true);
      expect((paths as Map<any, any>).size).toBe(0);
      expect((globalThis as any).__routerLast).toBeTruthy();
    });

    test('routeDirtyConnections returns an empty Map for minimal state', () => {
      const state: any = { blocks: new Map(), connections: [] };
      const paths = routeDirtyConnections(state, 20, 20, { x: 0, y: 0 }, new Set(), 50);
      expect(paths instanceof Map).toBe(true);
      expect((paths as Map<any, any>).size).toBe(0);
    });

    test('normalizeConnectionJunctions is a no-op on empty input', () => {
      const arr: any[] = [];
      expect(() => normalizeConnectionJunctions(arr)).not.toThrow();
      expect(arr.length).toBe(0);
    });
  });
});
//=============================================================================
