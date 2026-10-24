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
import {
  buildFallbackPath,
  buildFallbackPathAvoidingBlocks,
  buildSegments,
  normalizeWirePoints,
  buildPathWithHops,
} from './wire/wire-paths.js';
import type { FallbackRect } from './wire/wire-paths.js';
//=============================================================================
describe('wire-paths utilities', () => {
  test('buildFallbackPath returns straight or orthogonal path', () => {
    expect(buildFallbackPath({ x: 0, y: 0 }, { x: 0, y: 5 })).toEqual([
      { x: 0, y: 0 },
      { x: 0, y: 5 },
    ]);
    const p = buildFallbackPath({ x: 0, y: 0 }, { x: 3, y: 4 });
    expect(p.length).toBe(4);
  });

  test('buildSegments creates H/V segments from points', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 0, y: 10 },
      { x: 5, y: 10 },
    ];
    const segs = buildSegments(pts as any);
    expect(segs.length).toBe(2);
    expect(segs[0].orientation).toBe('V');
    expect(segs[1].orientation).toBe('H');
  });

  test('normalizeWirePoints accepts arrays and objects and dedupes', () => {
    const raw = [[0, 0], { x: 0, y: 0 }, { x: 1, y: 0 }, [1, 0]];
    const n = normalizeWirePoints(raw as any);
    expect(n.length).toBe(2);
  });

  test('buildPathWithHops emits arc commands when crossings exist', () => {
    // two perpendicular segments crossing at (5,5)
    const segH = {
      orientation: 'H',
      a: { x: 0, y: 5 },
      b: { x: 10, y: 5 },
      minX: 0,
      maxX: 10,
      y: 5,
      isStub: false,
    } as any;
    const segV = {
      orientation: 'V',
      a: { x: 5, y: 0 },
      b: { x: 5, y: 10 },
      minY: 0,
      maxY: 10,
      x: 5,
      isStub: false,
    } as any;
    const path = buildPathWithHops([segH], [segV]);
    expect(typeof path).toBe('string');
    expect(path.includes('a ')).toBe(true);
  });

  const hitsRect = (path: Array<{ x: number; y: number }>, r: FallbackRect): boolean => {
    for (let i = 1; i < path.length; i += 1) {
      const a = path[i - 1];
      const b = path[i];
      if (a.y === b.y) {
        if (
          a.y > r.top &&
          a.y < r.bottom &&
          Math.max(a.x, b.x) > r.left &&
          Math.min(a.x, b.x) < r.right
        )
          return true;
      } else if (a.x === b.x) {
        if (
          a.x > r.left &&
          a.x < r.right &&
          Math.max(a.y, b.y) > r.top &&
          Math.min(a.y, b.y) < r.bottom
        )
          return true;
      }
    }
    return false;
  };

  test('buildFallbackPathAvoidingBlocks routes around an intervening block', () => {
    // Port span whose naive mid-spine (x=200) would slice through the block.
    const from = { x: 300, y: 100 };
    const to = { x: 100, y: 300 };
    const block: FallbackRect = { left: 170, top: 90, right: 240, bottom: 260 };
    const naive = buildFallbackPath(from, to);
    expect(hitsRect(naive, block)).toBe(true);
    const path = buildFallbackPathAvoidingBlocks(from, to, [block]);
    expect(path.length).toBeGreaterThanOrEqual(2);
    expect(path[0]).toEqual(from);
    expect(path[path.length - 1]).toEqual(to);
    expect(hitsRect(path, block)).toBe(false);
  });

  test('buildFallbackPathAvoidingBlocks keeps every segment orthogonal', () => {
    const from = { x: 300, y: 100 };
    const to = { x: 100, y: 300 };
    const block: FallbackRect = { left: 170, top: 90, right: 240, bottom: 260 };
    const path = buildFallbackPathAvoidingBlocks(from, to, [block]);
    for (let i = 1; i < path.length; i += 1) {
      const a = path[i - 1];
      const b = path[i];
      expect(a.x === b.x || a.y === b.y).toBe(true);
    }
  });

  test('buildFallbackPathAvoidingBlocks with no obstacles equals the naive route', () => {
    const from = { x: 0, y: 0 };
    const to = { x: 200, y: 120 };
    expect(buildFallbackPathAvoidingBlocks(from, to, [])).toEqual(buildFallbackPath(from, to));
  });
});
//=============================================================================
