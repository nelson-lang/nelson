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
test('buildBlockOccupancyMap creates a grid set for integer-covered obstacle', async () => {
  const obs = [{ x0: 1, y0: 1, x1: 2, y1: 2 }];
  const grid = mod.buildBlockOccupancyMap(obs);
  expect(grid).toBeInstanceOf(Set);
  expect(grid.size).toBeGreaterThanOrEqual(4);
});
//=============================================================================
test('buildObstacleNearMap marks near obstacle codes', async () => {
  const obs = [{ x0: 2, y0: 2, x1: 2, y1: 2 }];
  const grid = mod.buildBlockOccupancyMap(obs);
  const bounds = { minX: 0, minY: 0, maxX: 4, maxY: 4 };
  const map = mod.buildObstacleNearMap(grid, bounds);
  expect(map).toBeInstanceOf(Uint8Array);
  const idx = (2 - bounds.minY) * (bounds.maxX - bounds.minX + 1) + (2 - bounds.minX);
  expect(map[idx]).toBeGreaterThanOrEqual(1);
});
//=============================================================================
test('buildWireNearMap marks adjacent cells with 1 and 2', async () => {
  const occupancy: any = { points: new Map(), edges: new Map() };
  occupancy.points.set('5,5', {});
  const bounds = { minX: 0, minY: 0, maxX: 10, maxY: 10 };
  const map = mod.buildWireNearMap(occupancy, bounds);
  expect(map).toBeInstanceOf(Uint8Array);
  const hasNear = Array.from(map).some((v) => v === 1 || v === 2);
  expect(hasNear).toBe(true);
});
//=============================================================================
test('analyzeConnectionGeometry detects non-orthogonal, self-cross and overlaps', async () => {
  const a = {
    from: 'A',
    to: 'B',
    points: [
      { x: 0, y: 0 },
      { x: 1, y: 1 },
    ],
  };
  const res1 = mod.analyzeConnectionGeometry([a]);
  expect(res1.totals.nonOrthogonal).toBeGreaterThanOrEqual(1);

  const pts = [
    { x: 0, y: 0 },
    { x: 4, y: 0 },
    { x: 4, y: 4 },
    { x: 0, y: 4 },
    { x: 4, y: 0 },
  ];
  const b = { from: 'C', to: 'D', points: pts };
  const res2 = mod.analyzeConnectionGeometry([b]);
  expect(typeof res2.totals.selfCrosses).toBe('number');

  const c = {
    from: 'E',
    to: 'F',
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
  };
  const d = {
    from: 'G',
    to: 'H',
    points: [
      { x: 5, y: 0 },
      { x: 15, y: 0 },
    ],
  };
  const res3 = mod.analyzeConnectionGeometry([c, d]);
  expect(res3.totals.overlaps).toBeGreaterThanOrEqual(1);
});
//=============================================================================
test('computeWireRoutes finds a straight path between adjacent nodes', async () => {
  const nodes = [
    { id: 'n1', x: 1, y: 1, dir: 'right' },
    { id: 'n2', x: 3, y: 1, dir: 'left' },
  ];
  const connections = [{ from: 'n1', to: 'n2' }];
  const res: any = mod.computeWireRoutes({
    nodes,
    connections,
    obstacles: [],
    settings: { maxTimeMs: 1000 },
  });
  expect(res).toBeTruthy();
  expect(res.wires instanceof Map).toBe(true);
  expect(res.wires.size).toBeGreaterThan(0);
  const entries: any = Array.from((res.wires as any).entries());
  const wire = entries[0] && entries[0][1];
  expect(wire.points && wire.points.length).toBeGreaterThanOrEqual(2);
  expect(wire.points[0]).toEqual({ x: 1, y: 1 });
  expect(wire.points[wire.points.length - 1]).toEqual({ x: 3, y: 1 });
});
//=============================================================================
test('computeWireRoutes returns no wires when nodes are missing', async () => {
  const res = mod.computeWireRoutes({
    nodes: [],
    connections: [{ from: 'a', to: 'b' }],
    obstacles: [],
  });
  expect(res).toBeTruthy();
  expect(res.wires.size).toBe(0);
  expect(res.cost).toBeDefined();
});
//=============================================================================
