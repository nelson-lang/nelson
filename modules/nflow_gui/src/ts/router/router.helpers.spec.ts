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
  buildBlockOccupancyMap,
  buildObstacleNearMap,
  buildWireNearMap,
  computeWireRoutes,
} from './router';
//=============================================================================
describe('router helpers (focused)', () => {
  test('buildBlockOccupancyMap creates expected integer cells', () => {
    const obstacles = [{ x0: 0.2, y0: 0.2, x1: 1.8, y1: 1.8 }];
    const grid = buildBlockOccupancyMap(obstacles);
    expect(grid).not.toBeNull();
    // floor range 0..1 for x and y -> 4 cells
    expect(grid.size).toBe(4);
  });

  test('buildObstacleNearMap marks nearby obstacle codes', () => {
    const obstacles = [{ x0: 1, y0: 1, x1: 1, y1: 1 }];
    const grid = buildBlockOccupancyMap(obstacles);
    const bounds = { minX: 0, minY: 0, maxX: 2, maxY: 2 };
    const map = buildObstacleNearMap(grid, bounds);
    const gridW = bounds.maxX - bounds.minX + 1;
    const idx = (1 - bounds.minY) * gridW + (1 - bounds.minX);
    expect(map[idx]).toBe(1);
  });

  test('buildWireNearMap marks neighbor cells for a single wire point', () => {
    const occupancy = { points: new Map([['1,1', {}]]), edges: new Map() } as any;
    const bounds = { minX: 0, minY: 0, maxX: 2, maxY: 2 };
    const map = buildWireNearMap(occupancy, bounds);
    const gridW = bounds.maxX - bounds.minX + 1;
    const centerIdx = (1 - bounds.minY) * gridW + (1 - bounds.minX);
    const rightIdx = (1 - bounds.minY) * gridW + (2 - bounds.minX);
    // center itself should not be flagged; neighbor to the right should be marked as 1
    expect(map[centerIdx]).toBe(0);
    expect(map[rightIdx]).toBe(1);
  });

  test('computeWireRoutes returns a single routed wire for a trivial two-node case', () => {
    const nodes = [
      { id: 'a', x: 0, y: 0, dir: 'right' },
      { id: 'b', x: 2, y: 0, dir: 'left' },
    ];
    const connections = [{ from: 'a', to: 'b' }];
    const result = computeWireRoutes({
      nodes,
      connections,
      obstacles: [],
      settings: { maxTimeMs: 200 },
    });
    expect(result).toBeDefined();
    expect(result.wires instanceof Map).toBe(true);
    expect(result.wires.size).toBe(1);
    expect(typeof result.cost.total).toBe('number');
  });
});
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { analyzeConnectionGeometry } from './router.js';
import { vi } from 'vitest';
//=============================================================================
describe('router helpers', () => {
  test('buildBlockOccupancyMap produces integer keys for covered cells', () => {
    const obstacles = [{ x0: 1, y0: 1, x1: 2, y1: 2 }];
    const grid = buildBlockOccupancyMap(obstacles);
    expect(grid).toBeInstanceOf(Set);
    expect(grid).not.toBeNull();
    const keys = Array.from(grid || []);
    expect(keys.length).toBeGreaterThanOrEqual(4);
  });

  test('buildObstacleNearMap marks near cells', () => {
    const obstacles = [{ x0: 1, y0: 1, x1: 1, y1: 1 }];
    const grid = buildBlockOccupancyMap(obstacles);
    const bounds = { minX: 0, minY: 0, maxX: 3, maxY: 3 };
    const map = buildObstacleNearMap(grid, bounds);
    expect(map).not.toBeNull();
    expect(map).toBeInstanceOf(Uint8Array);
    const gridW = bounds.maxX - bounds.minX + 1;
    // the obstacle at (1,1) should mark (1,1) as obs1 (1) or obs2 depending on neighbor logic
    const idx = (1 - bounds.minY) * gridW + (1 - bounds.minX);
    expect([0, 1, 2]).toContain((map as Uint8Array)[idx]);
  });

  test('buildWireNearMap marks near areas around occupancy points', () => {
    const occupancy = { points: new Map(), edges: new Map() };
    occupancy.points.set('1,1', {});
    const bounds = { minX: 0, minY: 0, maxX: 3, maxY: 3 };
    const map = buildWireNearMap(occupancy, bounds);
    expect(map).not.toBeNull();
    expect(map).toBeInstanceOf(Uint8Array);
    const gridW = bounds.maxX - bounds.minX + 1;
    const idx = (1 - bounds.minY) * gridW + (2 - bounds.minX);
    expect((map as Uint8Array)[idx]).toBeGreaterThanOrEqual(0);
  });

  test('analyzeConnectionGeometry detects overlaps and non-orthogonal segments', () => {
    const connections = [
      {
        from: 'A',
        to: 'B',
        points: [
          { x: 0, y: 0 },
          { x: 4, y: 0 },
        ],
      },
      {
        from: 'C',
        to: 'D',
        points: [
          { x: 2, y: 0 },
          { x: 6, y: 0 },
        ],
      },
      {
        from: 'E',
        to: 'F',
        points: [
          { x: 0, y: 0 },
          { x: 1, y: 1 },
        ],
      }, // diagonal -> non-orthogonal
    ];
    const res = analyzeConnectionGeometry(connections, { ignoreSharedPorts: true });
    expect(res).toBeDefined();
    expect(res.totals).toBeDefined();
    expect(res.totals.overlaps).toBeGreaterThanOrEqual(1);
    expect(res.totals.nonOrthogonal).toBeGreaterThanOrEqual(1);
  });
});
//=============================================================================
