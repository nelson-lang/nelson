//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, test } from 'vitest';
import { type Point, simplifyOrthogonalPath } from './router.js';
//=============================================================================
describe('simplifier regression tests', () => {
  test('inserts L-turn when orthogonal path collapses to two diagonal endpoints', () => {
    const pts: Point[] = [
      { x: 1620, y: 1520 },
      { x: 1640, y: 1520 },
      { x: 1640, y: 1500 },
      { x: 1670, y: 1500 },
    ];
    const simplified = simplifyOrthogonalPath(pts);
    expect(simplified.length).toBeGreaterThanOrEqual(3);
    expect(simplified[0]).toEqual({ x: 1620, y: 1520 });
    expect(simplified[simplified.length - 1]).toEqual({ x: 1670, y: 1500 });
    for (let i = 0; i < simplified.length - 1; i++) {
      const a = simplified[i];
      const b = simplified[i + 1];
      expect(a.x === b.x || a.y === b.y).toBe(true);
    }
  });
});
//=============================================================================
