//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('./router.ts');
});
//=============================================================================
test('router helpers: obstacle grid, near maps, basic routing and analysis', async () => {
  const {
    buildBlockOccupancyMap,
    buildObstacleNearMap,
    buildWireNearMap,
    computeWireRoutes,
    routeAllConnections,
    routeDirtyConnections,
    normalizeConnectionJunctions,
    analyzeConnectionGeometry,
  } = mod;

  const obstacles = [{ x0: 2, y0: 2, x1: 3, y1: 3 }];
  const grid = buildBlockOccupancyMap(obstacles);
  expect(grid instanceof Set).toBe(true);

  const bounds = { minX: 0, minY: 0, maxX: 6, maxY: 6 };
  const nearMap = buildObstacleNearMap(grid, bounds);
  expect(nearMap).toBeInstanceOf(Uint8Array);

  const occupancy = { points: new Map([['1,1', {}]]), edges: new Map() };
  const wireNear = buildWireNearMap(occupancy, bounds);
  expect(wireNear instanceof Uint8Array || wireNear === null).toBeTruthy();

  const nodes = [
    { id: 'n1', x: 0, y: 0, dir: 'right' },
    { id: 'n2', x: 5, y: 0, dir: 'left' },
  ];
  const connections = [{ from: 'n1', to: 'n2' }];
  const result = computeWireRoutes({
    nodes,
    connections,
    obstacles: [],
    settings: { maxTimeMs: 200 },
  });
  expect(result).toBeTruthy();
  expect(result.wires instanceof Map).toBe(true);

  const firstKey = Array.from(result.wires.keys())[0];
  const wire = result.wires.get(firstKey);
  expect(wire).toBeTruthy();
  expect(Array.isArray(wire.points)).toBe(true);

  const connsForAnalyze = [
    {
      from: 'a',
      to: 'b',
      points: [
        { x: 0, y: 0 },
        { x: 2, y: 0 },
      ],
    },
    {
      from: 'a',
      to: 'c',
      points: [
        { x: 0, y: 0 },
        { x: 0, y: 2 },
      ],
    },
  ];
  const analysis = analyzeConnectionGeometry(connsForAnalyze);
  expect(analysis).toHaveProperty('totals');

  const state: any = { blocks: new Map(), connections: [] };
  state.blocks.set('A', {
    id: 'A',
    x: 0,
    y: 0,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'out', index: 0 }],
  });
  state.blocks.set('B', {
    id: 'B',
    x: 120,
    y: 0,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'in', index: 0 }],
  });
  const connRef = { from: 'A', to: 'B', fromIndex: 0, toIndex: 0 };
  state.connections.push(connRef);

  const paths = routeAllConnections(state, 200, 200, { x: 0, y: 0 }, 200);
  expect(paths instanceof Map).toBe(true);

  const dirty = new Set([connRef]);
  const dirtyResult = routeDirtyConnections(state, 200, 200, { x: 0, y: 0 }, dirty, 200);
  expect(dirtyResult instanceof Map).toBe(true);

  normalizeConnectionJunctions([connRef, connRef]);
});
//=============================================================================
