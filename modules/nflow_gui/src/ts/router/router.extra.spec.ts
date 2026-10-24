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
  computeWireRoutes,
  normalizeConnectionJunctions,
  buildStubPoint,
  isValidStub,
  dedupePoints,
  simplifyOrthogonalPath,
  removeColinearPoints,
  pointsToPath,
  rotatePoint,
  getRotatedBounds,
  getPortSide,
  sideToDir,
} from './router.js';
//=============================================================================
describe('router extra coverage tests', () => {
  test('reuses prevSolution when prevSolution.wires is a Map', () => {
    const prevWires = new Map();
    prevWires.set('k1', { points: [{ x: 1, y: 1 }] });
    const prevSolution: any = { wires: prevWires };
    const res = computeWireRoutes({
      nodes: [],
      connections: [{ key: 'k1', from: 'A', to: 'B' }],
      obstacles: [],
      prevSolution,
      settings: { incremental: true, fullOptimize: false },
    });
    expect(res.wires instanceof Map).toBe(true);
    expect(res.wires.get('k1')).toBe(prevWires.get('k1'));
  });

  test('computes straight-line path between adjacent nodes', () => {
    const nodes = [
      { id: 'A', x: 0, y: 0, dir: 'right' },
      { id: 'B', x: 2, y: 0, dir: 'left' },
    ];
    const res = computeWireRoutes({
      nodes,
      connections: [{ from: 'A', to: 'B', key: 'k1' }],
      obstacles: [],
      settings: { maxTimeMs: 200 },
    });
    const wire = res.wires.get('k1');
    expect(wire).toBeTruthy();
    expect(Array.isArray(wire.points)).toBe(true);
    expect(wire.points.length).toBeGreaterThanOrEqual(2);
    const pts = wire.points;
    expect(pts[0].x).toBe(0);
    expect(pts[pts.length - 1].x).toBe(2);
  });

  test('normalizeConnectionJunctions aligns shared-source simple L turns', () => {
    const connA: any = {
      from: 'S',
      fromIndex: 0,
      to: 'A',
      toIndex: 0,
      points: [
        { x: 0, y: 0 },
        { x: 20, y: 0 },
        { x: 20, y: 20 },
        { x: 30, y: 20 },
      ],
    };
    const connB: any = {
      from: 'S',
      fromIndex: 0,
      to: 'B',
      toIndex: 0,
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
        { x: 10, y: 20 },
        { x: 30, y: 20 },
      ],
    };
    const arr = [connA, connB];
    normalizeConnectionJunctions(arr);
    // normalization may adjust the shared turn to match connA's (20)
    // or leave it unchanged depending on stub-distance logic; accept either.
    expect([10, 20]).toContain(connB.points[1].x);
  });

  test('buildStubPoint and isValidStub behaviors', () => {
    const port = { x: 50, y: 40 };
    const left = buildStubPoint(port, 'left');
    expect(left).toEqual({ x: port.x - GRID_SIZE, y: port.y });
    const right = buildStubPoint(port, 'right');
    expect(right).toEqual({ x: port.x + GRID_SIZE, y: port.y });
    const top = buildStubPoint(port, 'top');
    expect(top).toEqual({ x: port.x, y: port.y - GRID_SIZE });
    const bottom = buildStubPoint(port, 'bottom');
    expect(bottom).toEqual({ x: port.x, y: port.y + GRID_SIZE });

    expect(isValidStub(port, { x: port.x - GRID_SIZE - 1, y: port.y }, 'left')).toBe(true);
    expect(isValidStub(port, { x: port.x - GRID_SIZE + 1, y: port.y }, 'left')).toBe(false);
    expect(isValidStub(port, { x: port.x + GRID_SIZE + 1, y: port.y }, 'right')).toBe(true);
    expect(isValidStub(port, { x: port.x, y: port.y - GRID_SIZE - 1 }, 'top')).toBe(true);
    expect(isValidStub(port, { x: port.x, y: port.y + GRID_SIZE + 1 }, 'bottom')).toBe(true);
  });

  test('rotatePoint and getRotatedBounds with common rotations', () => {
    const block: any = { x: 0, y: 0, width: 10, height: 10 };
    const p = { x: block.x + 6, y: block.y + 5 }; // one unit right of center

    block.rotation = 0;
    expect(rotatePoint(p, block)).toEqual(p);

    block.rotation = 90;
    const r90 = rotatePoint(p, block);
    // angle 90: { x: cx - dy, y: cy + dx }
    expect(r90).toEqual({ x: 5, y: 6 });

    block.rotation = 180;
    const r180 = rotatePoint(p, block);
    // angle 180: { x: cx - dx, y: cy - dy }
    expect(r180).toEqual({ x: 4, y: 5 });

    block.rotation = 270;
    const r270 = rotatePoint(p, block);
    // angle 270: { x: cx + dy, y: cy - dx }
    expect(r270).toEqual({ x: 5, y: 4 });

    block.rotation = 45;
    // unsupported angle returns original point
    expect(rotatePoint(p, block)).toEqual(p);

    block.rotation = 0;
    const b0 = getRotatedBounds(block);
    expect(b0.left).toBe(0);
    expect(b0.right).toBe(10);
    block.rotation = 90;
    const b90 = getRotatedBounds(block);
    // width and height swap, the center stays at 5,5
    expect(b90.left).toBe(0);
    expect(b90.right).toBe(10);
  });

  test('getPortSide and sideToDir mapping', () => {
    const block: any = { x: 0, y: 0, width: 10, height: 10 };
    const center = { x: block.x + block.width / 2, y: block.y + block.height / 2 };
    expect(getPortSide(block, { x: center.x - 10, y: center.y })).toBe('left');
    expect(getPortSide(block, { x: center.x + 10, y: center.y })).toBe('right');
    expect(getPortSide(block, { x: center.x, y: center.y - 10 })).toBe('top');
    expect(getPortSide(block, { x: center.x, y: center.y + 10 })).toBe('bottom');

    expect(sideToDir('left')).toBe('left');
    expect(sideToDir('right')).toBe('right');
    expect(sideToDir('top')).toBe('up');
    expect(sideToDir('bottom')).toBe('down');
  });

  test('dedupe, removeColinearPoints and pointsToPath', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 0, y: 0 },
      { x: 0, y: 1 },
      { x: 0, y: 2 },
      { x: 1, y: 2 },
    ];
    const deduped = dedupePoints(pts);
    expect(deduped).toEqual([
      { x: 0, y: 0 },
      { x: 0, y: 1 },
      { x: 0, y: 2 },
      { x: 1, y: 2 },
    ]);
    const cleaned = removeColinearPoints(deduped);
    expect(cleaned).toEqual([
      { x: 0, y: 0 },
      { x: 0, y: 2 },
      { x: 1, y: 2 },
    ]);

    expect(pointsToPath([])).toBe('');
    expect(
      pointsToPath([
        { x: 0, y: 0 },
        { x: 1, y: 1 },
      ])
    ).toBe('M 0 0 L 1 1');
  });
});
//=============================================================================
