//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { routeDirtyConnections } from './router.js';
import { GRID_SIZE, snapToGrid, alignToGrid } from '../utils/geometry.js';
//=============================================================================
describe('router.ts - enforcePortStubs behavior', () => {
  test('routeDirtyConnections inserts port stubs and updates connection points', () => {
    const A = {
      id: 'A',
      x: 100,
      y: 100,
      width: 40,
      height: 40,
      rotation: 0,
      ports: [{ type: 'out', index: 0, x: 0, y: 20 }],
    };
    const B = {
      id: 'B',
      x: 200,
      y: 100,
      width: 40,
      height: 40,
      rotation: 0,
      ports: [{ type: 'in', index: 0, x: 40, y: 20 }],
    };

    const conn = {
      from: 'A',
      to: 'B',
      points: [
        { x: 110, y: 120 },
        { x: 230, y: 120 },
      ],
    };

    const state: any = {
      blocks: new Map([
        ['A', A],
        ['B', B],
      ]),
      connections: [conn],
    };

    const expectedFrom = alignToGrid({ x: A.x + A.ports[0].x, y: A.y + A.ports[0].y });
    const expectedTo = alignToGrid({ x: B.x + B.ports[0].x, y: B.y + B.ports[0].y });

    const res = routeDirtyConnections(state, 400, 400, { x: 0, y: 0 }, new Set(), 200);

    // The state connection object must be updated in-place by applyRouter2Result
    expect(Array.isArray(state.connections[0].points)).toBe(true);
    const updated = state.connections[0].points;
    expect(updated.length).toBeGreaterThanOrEqual(conn.points.length);
    expect(Math.abs(updated[0].x - expectedFrom.x)).toBeLessThanOrEqual(GRID_SIZE);
    expect(Math.abs(updated[0].y - expectedFrom.y)).toBeLessThanOrEqual(GRID_SIZE);
    const last = updated[updated.length - 1];
    expect(Math.abs(last.x - expectedTo.x)).toBeLessThanOrEqual(GRID_SIZE);
    expect(Math.abs(last.y - expectedTo.y)).toBeLessThanOrEqual(GRID_SIZE);

    const entries = Array.from(res.values());
    expect(entries.length).toBeGreaterThanOrEqual(1);
    expect(typeof entries[0]).toBe('string');
    expect(entries[0].length).toBeGreaterThanOrEqual(0);
  });

  test('routeDirtyConnections handles blocks left of the world origin', () => {
    const A = {
      id: 'A',
      x: -120,
      y: 20,
      width: 80,
      height: 60,
      rotation: 0,
      ports: [{ type: 'out', index: 0, x: 80, y: 30 }],
    };
    const B = {
      id: 'B',
      x: 40,
      y: 20,
      width: 80,
      height: 60,
      rotation: 0,
      ports: [{ type: 'in', index: 0, x: 0, y: 30 }],
    };

    const conn = {
      from: 'A',
      to: 'B',
      fromIndex: 0,
      toIndex: 0,
      points: [
        { x: -40, y: 50 },
        { x: 40, y: 50 },
      ],
    };

    const state: any = {
      blocks: new Map([
        ['A', A],
        ['B', B],
      ]),
      connections: [conn],
    };

    const expectedFrom = alignToGrid({ x: A.x + A.ports[0].x, y: A.y + A.ports[0].y });
    const expectedTo = alignToGrid({ x: B.x + B.ports[0].x, y: B.y + B.ports[0].y });

    routeDirtyConnections(state, 400, 300, { x: -200, y: -100 }, new Set(), 200);

    const updated = state.connections[0].points;
    expect(updated.length).toBeGreaterThanOrEqual(2);
    expect(updated[0].x).toBeLessThan(0);
    expect(Math.abs(updated[0].x - expectedFrom.x)).toBeLessThanOrEqual(GRID_SIZE);
    expect(Math.abs(updated[0].y - expectedFrom.y)).toBeLessThanOrEqual(GRID_SIZE);
    const last = updated[updated.length - 1];
    expect(Math.abs(last.x - expectedTo.x)).toBeLessThanOrEqual(GRID_SIZE);
    expect(Math.abs(last.y - expectedTo.y)).toBeLessThanOrEqual(GRID_SIZE);
  });
});
//=============================================================================
