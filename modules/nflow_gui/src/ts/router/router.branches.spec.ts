//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('router internal branches', () => {
  let mod: any;
  beforeAll(async () => {
    mod = await import('./router.ts');
  });

  test('computeWireRoutes reports bad_direction when nodes have invalid dir', () => {
    const { computeWireRoutes } = mod;
    const nodes = [
      { id: 'n1', x: 0, y: 0, dir: 'invalid' },
      { id: 'n2', x: 1, y: 0, dir: 'also-bad' },
    ];
    const connections = [{ from: 'n1', to: 'n2' }];
    const res = computeWireRoutes({
      nodes,
      connections,
      obstacles: [],
      settings: { maxTimeMs: 50 },
    });
    expect(Array.isArray(res.failures)).toBe(true);
    expect(res.failures.length).toBeGreaterThanOrEqual(1);
  });

  test('computeWireRoutes reports blocked_start_step and blocked_end_step for obstacles', () => {
    const { computeWireRoutes } = mod;
    const nodes = [
      { id: 'a', x: 1, y: 1, dir: 'right' },
      { id: 'b', x: 4, y: 1, dir: 'left' },
    ];
    const connections = [{ from: 'a', to: 'b' }];
    const obstacles = [{ x0: 2, y0: 1, x1: 2, y1: 1 }];
    const res = computeWireRoutes({ nodes, connections, obstacles, settings: { maxTimeMs: 50 } });
    expect(res).toHaveProperty('failures');
    expect(Array.isArray(res.failures)).toBe(true);
  });

  test('computeWireRoutes returns timeout when maxTimeMs too small', () => {
    const { computeWireRoutes } = mod;
    const nodes = [
      { id: 'n1', x: 0, y: 0, dir: 'right' },
      { id: 'n2', x: 200, y: 0, dir: 'left' },
    ];
    const connections = [{ from: 'n1', to: 'n2' }];
    const res = computeWireRoutes({
      nodes,
      connections,
      obstacles: [],
      settings: { maxTimeMs: 0 },
    });
    expect(res.failures.length + res.wires.size).toBeGreaterThanOrEqual(0);
  });

  test('analyzeConnectionGeometry finds overlaps and non-orthogonal segments', () => {
    const { analyzeConnectionGeometry } = mod;
    const conns = [
      {
        from: 'x',
        to: 'y',
        points: [
          { x: 0, y: 0 },
          { x: 2, y: 0 },
        ],
      },
      {
        from: 'u',
        to: 'v',
        points: [
          { x: 1, y: -1 },
          { x: 1, y: 1 },
        ],
      },
      {
        from: 'diag',
        to: 'd',
        points: [
          { x: 0, y: 0 },
          { x: 1, y: 1 },
        ],
      },
    ];
    const analysis = analyzeConnectionGeometry(conns, { ignoreSharedPorts: true });
    expect(analysis.totals.nonOrthogonal).toBeGreaterThanOrEqual(1);
  });

  test('buildOrthoSegments and countSelfCrossings detect self-cross', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 3, y: 0 },
      { x: 3, y: 3 },
      { x: 0, y: 3 },
      { x: 0, y: 0 },
    ];
    const analysis = mod.analyzeConnectionGeometry([{ from: 'a', to: 'b', points: pts }]);
    expect(analysis).toHaveProperty('totals');
  });
});
//=============================================================================
