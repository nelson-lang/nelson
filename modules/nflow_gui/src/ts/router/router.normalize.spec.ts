//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { normalizeConnectionJunctions } from './router';
//=============================================================================
describe('normalizeConnectionJunctions (shared-source junctions)', () => {
  test('aligns horizontal (H) simple-L turns for shared source', () => {
    const connA = {
      from: 'S',
      fromIndex: 0,
      to: 'A',
      toIndex: 0,
      points: [
        { x: 0, y: 0 },
        { x: 20, y: 0 },
        { x: 20, y: 20 },
        { x: 40, y: 20 },
      ],
    };

    const connB = {
      from: 'S',
      fromIndex: 0,
      to: 'B',
      toIndex: 0,
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
        { x: 10, y: 20 },
        { x: 40, y: 20 },
      ],
    };

    const conns = [connA, connB];
    normalizeConnectionJunctions(conns);

    // connB's turn (index 1 and 2) should be adjusted to match connA's x (20)
    expect(connB.points[1].x).toBe(connA.points[1].x);
    expect(connB.points[2].x).toBe(connA.points[2].x);
  });

  test('aligns vertical (V) simple-L turns for shared source', () => {
    const connA = {
      from: 'S',
      fromIndex: 0,
      to: 'A',
      toIndex: 0,
      points: [
        { x: 0, y: 0 },
        { x: 0, y: 20 },
        { x: 20, y: 20 },
        { x: 20, y: 40 },
      ],
    };

    const connB = {
      from: 'S',
      fromIndex: 0,
      to: 'B',
      toIndex: 0,
      points: [
        { x: 0, y: 0 },
        { x: 0, y: 10 },
        { x: 20, y: 10 },
        { x: 20, y: 40 },
      ],
    };

    const conns = [connA, connB];
    normalizeConnectionJunctions(conns);

    // connB's turn (index 1 and 2) should be adjusted to match connA's y (20)
    expect(connB.points[1].y).toBe(connA.points[1].y);
    expect(connB.points[2].y).toBe(connA.points[2].y);
  });
});
//=============================================================================
