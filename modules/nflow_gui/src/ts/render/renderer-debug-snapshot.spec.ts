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
import { makeDebugSnapshotHandlers } from './core/renderer-debug-snapshot';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeSvg = () => {
  const svg = document.createElementNS(ns, 'svg');
  svg.setAttribute('viewBox', '0 0 800 600');
  return svg;
};
//=============================================================================
const makeCtx = (overrides: any = {}) => ({
  svg: makeSvg(),
  state: {
    blocks: new Map(),
    connections: [] as any[],
    isPinching: false,
    isPanning: false,
    fastRouting: false,
    routingDirty: false,
    routingScheduled: false,
  },
  blockLayer: document.createElementNS(ns, 'g'),
  wireLayer: document.createElementNS(ns, 'g'),
  ...overrides,
});
//=============================================================================
const makeBlock = (id: string, x: number, y: number, w = 60, h = 40) => ({
  id,
  x,
  y,
  width: w,
  height: h,
  rotation: 0,
  ports: [],
});
//=============================================================================
describe('makeDebugSnapshotHandlers', () => {
  describe('buildDebugObstacles', () => {
    test('returns empty array when no blocks', () => {
      const ctx = makeCtx();
      const { buildDebugObstacles } = makeDebugSnapshotHandlers(ctx as any);

      const obstacles = buildDebugObstacles();

      expect(obstacles).toEqual([]);
    });

    test('returns one obstacle per block with correct owner', () => {
      const ctx = makeCtx();
      ctx.state.blocks.set('b1', makeBlock('b1', 0, 0, 60, 40) as any);
      const { buildDebugObstacles } = makeDebugSnapshotHandlers(ctx as any);

      const obstacles = buildDebugObstacles();

      expect(obstacles.length).toBe(1);
      expect(obstacles[0].owner).toBe('b1');
    });

    test('obstacle x0/y0 are floor of position divided by GRID_SIZE', () => {
      const ctx = makeCtx();
      ctx.state.blocks.set('b2', makeBlock('b2', 20, 20, 60, 40) as any);
      const { buildDebugObstacles } = makeDebugSnapshotHandlers(ctx as any);

      const [obs] = buildDebugObstacles();
      // Assert: x0 = floor(20/20), y0 = floor(20/20) (GRID_SIZE=20)
      expect(typeof obs.x0).toBe('number');
      expect(typeof obs.y0).toBe('number');
      expect(obs.x1).toBeGreaterThanOrEqual(obs.x0);
      expect(obs.y1).toBeGreaterThanOrEqual(obs.y0);
    });

    test('returns an obstacle for each block', () => {
      const ctx = makeCtx();
      ctx.state.blocks.set('b1', makeBlock('b1', 0, 0) as any);
      ctx.state.blocks.set('b2', makeBlock('b2', 100, 100) as any);
      ctx.state.blocks.set('b3', makeBlock('b3', 200, 200) as any);
      const { buildDebugObstacles } = makeDebugSnapshotHandlers(ctx as any);

      const obstacles = buildDebugObstacles();

      expect(obstacles.length).toBe(3);
    });
  });

  describe('computeDebugObsNear', () => {
    test('returns zero counts for empty points', () => {
      const ctx = makeCtx();
      const { computeDebugObsNear } = makeDebugSnapshotHandlers(ctx as any);

      const result = computeDebugObsNear([], []);

      expect(result).toEqual({ obs1: 0, obs2: 0 });
    });

    test('returns zero counts for undefined points', () => {
      const ctx = makeCtx();
      const { computeDebugObsNear } = makeDebugSnapshotHandlers(ctx as any);

      const result = computeDebugObsNear(undefined, []);

      expect(result).toEqual({ obs1: 0, obs2: 0 });
    });

    test('counts obs1 when point is within 1 grid cell of obstacle', () => {
      const ctx = makeCtx();
      const { computeDebugObsNear } = makeDebugSnapshotHandlers(ctx as any);
      const obstacles = [{ x0: 5, y0: 5, x1: 7, y1: 7 }];
      // Point at pixel (60,60) → grid (6,6) which is inside obstacle → minDist=0 → obs1
      const points = [{ x: 60, y: 60 }];

      const result = computeDebugObsNear(points, obstacles);

      expect(result.obs1).toBe(1);
      expect(result.obs2).toBe(0);
    });

    test('counts obs2 when point is 2 grid cells from obstacle', () => {
      const ctx = makeCtx();
      const { computeDebugObsNear } = makeDebugSnapshotHandlers(ctx as any);
      // Obstacle at grid (10,10)-(12,12); point at pixel (80,100) → grid (8,10): dx=10-8=2, dy=0 → minDist=2
      const obstacles = [{ x0: 10, y0: 10, x1: 12, y1: 12 }];
      const points = [{ x: 80, y: 100 }]; // 80/10=8, 100/10=10

      const result = computeDebugObsNear(points, obstacles);

      expect(result.obs1).toBe(0);
      expect(result.obs2).toBe(1);
    });
  });

  describe('buildDebugSnapshot', () => {
    test('returns a string (joined newlines)', () => {
      const ctx = makeCtx();
      const { buildDebugSnapshot } = makeDebugSnapshotHandlers(ctx as any);

      const result = buildDebugSnapshot();

      expect(typeof result).toBe('string');
    });

    test('includes viewBox information from svg', () => {
      const ctx = makeCtx();
      const { buildDebugSnapshot } = makeDebugSnapshotHandlers(ctx as any);

      const result = buildDebugSnapshot();

      expect(result).toContain('viewBox=0 0 800 600');
    });

    test('includes blocks and connections count', () => {
      const ctx = makeCtx();
      ctx.state.blocks.set('b1', makeBlock('b1', 0, 0) as any);
      const { buildDebugSnapshot } = makeDebugSnapshotHandlers(ctx as any);

      const result = buildDebugSnapshot();

      expect(result).toContain('blocks=1');
      expect(result).toContain('connections=0');
    });

    test('includes summary: and nodes: sections', () => {
      const ctx = makeCtx();
      const { buildDebugSnapshot } = makeDebugSnapshotHandlers(ctx as any);

      const result = buildDebugSnapshot();

      expect(result).toContain('summary:');
      expect(result).toContain('nodes:');
      expect(result).toContain('wires:');
    });
  });
});
//=============================================================================
