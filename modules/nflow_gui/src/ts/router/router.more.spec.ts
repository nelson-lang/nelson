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
import {
  pointKey,
  pointKeyInt,
  edgeKeyFor,
  buildBlockOccupancyMap,
  obstacleNear,
  buildObstacleNearMap,
  buildWireNearMap,
  isBlocked,
  buildOccupancy,
  occupancyEdgeBlocked,
  occupancyPointInfo,
  MinHeap,
  cellIndex,
  stateIndex,
  decodeState,
  buildResultFromArrays,
  buildCost,
  proximityPenalty,
  densifyGridPath,
  buildOrthoSegments,
  countParallelOverlaps,
  countTurns,
  pathLength,
  applyRouter2Result,
  buildRouter2Input,
} from './router.js';
//=============================================================================
describe('router.more helpers', () => {
  test('pointKey and pointKeyInt produce distinct keys', () => {
    expect(pointKey({ x: 1, y: 2 })).toBe('1,2');
    const a = pointKeyInt(1, 2);
    const b = pointKeyInt(2, 1);
    expect(typeof a).toBe('number');
    expect(a).not.toBe(b);
  });

  test('edgeKeyFor vertical and horizontal edges', () => {
    const v = edgeKeyFor({ x: 5, y: 2 }, { x: 5, y: 8 });
    expect(v.startsWith('v:5:')).toBe(true);
    const h = edgeKeyFor({ x: 1, y: 3 }, { x: 4, y: 3 });
    expect(h.startsWith('h:1:4:3')).toBe(true);
  });

  test('buildBlockOccupancyMap and obstacleNear and buildObstacleNearMap', () => {
    const obs = [{ x0: 0.2, y0: 0.2, x1: 1.8, y1: 1.8 }];
    const grid = buildBlockOccupancyMap(obs);
    expect(grid.has(pointKeyInt(0, 0))).toBe(true);
    expect(grid.has(pointKeyInt(1, 1))).toBe(true);

    const near0 = obstacleNear({ x: 0, y: 0 }, grid);
    expect(near0.obs1 === 1 || near0.obs2 === 0).toBe(true);

    const bounds = { minX: 0, minY: 0, maxX: 1, maxY: 1 };
    const map = buildObstacleNearMap(grid, bounds);
    const idx00 = cellIndex(0, 0, bounds.minX, bounds.minY, bounds.maxX - bounds.minX + 1);
    expect(map[idx00]).toBeGreaterThanOrEqual(0);
  });

  test('buildWireNearMap marks neighboring cells', () => {
    const occ: any = { points: new Map([['0,0', {}]]), edges: new Map() };
    const bounds = { minX: -1, minY: -1, maxX: 1, maxY: 1 };
    const map = buildWireNearMap(occ, bounds);
    const gridW = bounds.maxX - bounds.minX + 1;
    const idxRight = cellIndex(1, 0, bounds.minX, bounds.minY, gridW);
    // a neighbor of 0,0 at (1,0) should be marked as near-wire (1 or 2)
    expect(map[idxRight] === 1 || map[idxRight] === 2).toBe(true);
  });

  test('isBlocked respects allowedSet and obstacleGrid', () => {
    const allowed = new Set([pointKeyInt(2, 2)]);
    expect(isBlocked({ x: 2, y: 2 }, [], allowed, null)).toBe(false);
    const grid = new Set([pointKeyInt(3, 3)]);
    expect(isBlocked({ x: 3, y: 3 }, [], new Set(), grid)).toBe(true);
  });

  test('buildOccupancy and occupancyEdgeBlocked and occupancyPointInfo', () => {
    const wires = [
      {
        points: [
          { x: 0, y: 0 },
          { x: 1, y: 0 },
        ],
        meta: { from: 'A', to: 'B', key: 'k1' },
      },
    ];
    const occ = buildOccupancy(wires as any);
    expect(occ.points.has('0,0')).toBe(true);
    const blocked = occupancyEdgeBlocked({ x: 0, y: 0 }, { x: 1, y: 0 }, occ as any, new Set(), {
      from: 'X',
    });
    expect(blocked).toBe(true);
    const info = occupancyPointInfo({ x: 0, y: 0 }, occ as any, new Set(), null as any);
    expect(info.occupied).toBe(true);
  });

  test('MinHeap pushes and pops in order', () => {
    const h = new MinHeap();
    h.push({ idx: 1, f: 10 });
    h.push({ idx: 2, f: 5 });
    h.push({ idx: 3, f: 7 });
    const first = h.pop();
    expect(first.idx).toBe(2);
    const second = h.pop();
    expect(second.idx).toBe(3);
  });

  test('cellIndex/stateIndex/decodeState roundtrip and buildResultFromArrays', () => {
    const minX = 0;
    const minY = 0;
    const gridW = 5;
    const x = 2;
    const y = 3;
    const cell = cellIndex(x, y, minX, minY, gridW);
    const idx = stateIndex(cell, 1, 0);
    const state = decodeState(idx, minX, minY, gridW);
    expect(state.x).toBe(x);
    expect(state.y).toBe(y);
    expect(state.dir).toBe(1);

    const totalArr = new Float64Array(idx + 1);
    totalArr[idx] = 42;
    const lenArr = new Int32Array(idx + 1);
    lenArr[idx] = 2;
    const turnsArr = new Int16Array(idx + 1);
    const hopsArr = new Int16Array(idx + 1);
    const nearArr = new Int32Array(idx + 1);
    const wire1Arr = new Int32Array(idx + 1);
    const wire2Arr = new Int32Array(idx + 1);
    const obs1Arr = new Int32Array(idx + 1);
    const obs2Arr = new Int32Array(idx + 1);
    const cameFrom = new Int32Array(idx + 1);
    cameFrom[idx] = -1;
    const res = buildResultFromArrays(
      idx,
      cameFrom,
      { totalArr, lenArr, turnsArr, hopsArr, nearArr, wire1Arr, wire2Arr, obs1Arr, obs2Arr },
      minX,
      minY,
      gridW
    );
    expect(res.points.length).toBe(1);
    expect(res.cost.total).toBe(42);
  });

  test('buildCost and proximityPenalty basic case', () => {
    const current = {
      length: 2,
      turns: 1,
      hops: 0,
      near: 0,
      nearBreakdown: { wire1: 0, wire2: 0, obs1: 0, obs2: 0 },
    };
    const opts: any = {
      lengthCost: 1,
      turnCost: 6,
      hopCost: 20,
      nearWirePenalty1: 0,
      nearWirePenalty2: 0,
      nearObstaclePenalty1: 0,
      nearObstaclePenalty2: 0,
    };
    const cost = buildCost(current as any, opts, {
      turnCost: 0,
      hopCost: 0,
      nearCost: 0,
      nearBreakdown: {},
      lengthIncrement: 1,
    });
    expect(cost.total).toBe(
      (current.length + 1) * opts.lengthCost +
        current.turns * opts.turnCost +
        current.hops * opts.hopCost
    );
    const prox = proximityPenalty(
      { x: 0, y: 0 },
      null,
      null,
      null,
      null,
      null,
      null,
      opts,
      null,
      current as any
    );
    expect(prox.cost).toBe(0);
  });

  test('densifyGridPath, buildOrthoSegments, overlaps, turns and pathLength', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 3, y: 0 },
    ];
    const dense = densifyGridPath(pts);
    expect(dense.length).toBe(4);
    const segs = buildOrthoSegments(dense);
    expect(segs.length).toBeGreaterThan(0);
    const a = [
      {
        a: { x: 0, y: 0 },
        b: { x: 10, y: 0 },
        orientation: 'H',
        minX: 0,
        maxX: 10,
        minY: 0,
        maxY: 0,
      },
    ];
    const b = [
      {
        a: { x: 5, y: 0 },
        b: { x: 15, y: 0 },
        orientation: 'H',
        minX: 5,
        maxX: 15,
        minY: 0,
        maxY: 0,
      },
    ];
    expect(countParallelOverlaps(a as any, b as any)).toBeGreaterThan(0);
    const turns = countTurns([
      { x: 0, y: 0 },
      { x: 1, y: 0 },
      { x: 1, y: 1 },
      { x: 2, y: 1 },
    ]);
    expect(turns).toBe(2);
    expect(
      pathLength([
        { x: 0, y: 0 },
        { x: 2, y: 0 },
        { x: 2, y: 3 },
      ])
    ).toBe(5);
  });

  test('applyRouter2Result updates connections and returns paths', () => {
    const state: any = { blocks: new Map() };
    const connRef: any = { points: [] };
    const connections: any = [{ key: 'k1', __ref: connRef }];
    const wires = new Map();
    wires.set('k1', { points: [{ x: 1, y: 1 }] });
    const res = applyRouter2Result(state, connections, { wires });
    const path = res.get(connRef);
    // points are multiplied by GRID_SIZE inside applyRouter2Result
    expect(typeof path).toBe('string');
    expect(path.startsWith('M ')).toBe(true);
  });

  test('buildRouter2Input produces nodes and connections', () => {
    const block = {
      id: 'B',
      x: 0,
      y: 0,
      width: 10,
      height: 10,
      rotation: 0,
      ports: [{ type: 'out', index: 0, x: 5, y: 5 }],
    };
    const state: any = {
      blocks: new Map([['B', block]]),
      connections: [{ from: 'B', to: 'B', fromIndex: 0, toIndex: 0 }],
    };
    const out = buildRouter2Input(state as any);
    expect(Array.isArray(out.nodes)).toBe(true);
    expect(Array.isArray(out.connections)).toBe(true);
  });
});
//=============================================================================
