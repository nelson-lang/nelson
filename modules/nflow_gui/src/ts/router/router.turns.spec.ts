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
  minTurnDistanceToEndpointCells,
  firstTurnInfo,
  canAdjustSharedTurn,
  wouldCreateNonSharedOverlap,
} from './router';
//=============================================================================
describe('router turn & overlap helpers', () => {
  test('canAdjustSharedTurn rejects proposals that shorten stub below preferred', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 30, y: 0 },
      { x: 30, y: 40 },
      { x: 40, y: 40 },
    ];
    const info = firstTurnInfo(pts);
    expect(info).toBeDefined();
    // current distance from first turn to start is 30px -> 3 cells
    expect(minTurnDistanceToEndpointCells(pts, 0)).toBe(3);
    // moving turn to x=10 (1 cell) should be rejected when preferred=2
    expect(canAdjustSharedTurn(pts, info, 'x', 10, 2)).toBe(false);
    // moving turn to x=20 (2 cells) should be allowed
    expect(canAdjustSharedTurn(pts, info, 'x', 20, 2)).toBe(true);
  });

  test('wouldCreateNonSharedOverlap detects parallel overlap and ignores shared', () => {
    const conn = {
      from: 'S',
      to: 'T',
      fromIndex: 0,
      toIndex: 0,
      points: [
        { x: 10, y: 50 },
        { x: 50, y: 50 },
      ],
    };
    const proposed = [
      { x: 10, y: 50 },
      { x: 30, y: 50 },
    ];
    const other = {
      from: 'X',
      to: 'Y',
      fromIndex: 0,
      toIndex: 0,
      points: [
        { x: 20, y: 50 },
        { x: 60, y: 50 },
      ],
    };
    expect(wouldCreateNonSharedOverlap(conn, proposed, [conn, other])).toBe(true);

    // if other shares the same source, overlap should be ignored
    const otherShared = { ...other, from: 'S' };
    expect(wouldCreateNonSharedOverlap(conn, proposed, [conn, otherShared])).toBe(false);
  });
});
//=============================================================================
