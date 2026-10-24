//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { applyAlignment, minBlocksRequired } from './block-alignment.js';
import type { AlignBlock } from './block-alignment.js';
//=============================================================================
function b(x: number, y: number, w = 80, h = 40): AlignBlock {
  return { x, y, width: w, height: h };
}
//=============================================================================
function clone(blocks: AlignBlock[]): AlignBlock[] {
  return blocks.map((b) => ({ ...b }));
}
//=============================================================================
describe('minBlocksRequired', () => {
  test('alignment actions require 2 blocks', () => {
    const alignActions = [
      'alignLeft',
      'alignRight',
      'alignCenterH',
      'alignTop',
      'alignBottom',
      'alignMiddleV',
      'snapToGrid',
    ] as const;
    alignActions.forEach((a) => expect(minBlocksRequired(a)).toBe(2));
  });

  test('distribute actions require 3 blocks', () => {
    expect(minBlocksRequired('distributeH')).toBe(3);
    expect(minBlocksRequired('distributeV')).toBe(3);
  });
});
//=============================================================================
describe('applyAlignment guard', () => {
  test('single block: no mutation for any action', () => {
    const blocks = [b(10, 20)];
    const original = clone(blocks);
    applyAlignment(blocks, 'alignLeft');
    expect(blocks).toEqual(original);
  });

  test('two blocks: distributeH does nothing (requires 3)', () => {
    const blocks = [b(0, 0), b(200, 0)];
    const original = clone(blocks);
    applyAlignment(blocks, 'distributeH');
    expect(blocks).toEqual(original);
  });

  test('two blocks: distributeV does nothing (requires 3)', () => {
    const blocks = [b(0, 0), b(0, 200)];
    const original = clone(blocks);
    applyAlignment(blocks, 'distributeV');
    expect(blocks).toEqual(original);
  });
});
//=============================================================================
describe('alignLeft', () => {
  test('all blocks moved to minimum x', () => {
    const blocks = [b(50, 10), b(20, 30), b(80, 50)];
    applyAlignment(blocks, 'alignLeft');
    blocks.forEach((blk) => expect(blk.x).toBe(20));
  });

  test('y coordinates unchanged', () => {
    const blocks = [b(50, 10), b(20, 30), b(80, 50)];
    applyAlignment(blocks, 'alignLeft');
    expect(blocks.map((blk) => blk.y)).toEqual([10, 30, 50]);
  });

  test('already aligned: no change', () => {
    const blocks = [b(10, 0), b(10, 100)];
    const original = clone(blocks);
    applyAlignment(blocks, 'alignLeft');
    expect(blocks).toEqual(original);
  });

  test('returns the same array reference', () => {
    const blocks = [b(0, 0), b(100, 0)];
    const result = applyAlignment(blocks, 'alignLeft');
    expect(result).toBe(blocks);
  });
});
//=============================================================================
describe('alignRight', () => {
  test('all right edges align to maximum right', () => {
    const blocks = [b(0, 0, 80, 40), b(100, 0, 60, 40), b(50, 0, 40, 40)];
    applyAlignment(blocks, 'alignRight');
    blocks.forEach((blk) => expect(blk.x + blk.width).toBe(160));
  });

  test('y coordinates unchanged', () => {
    const blocks = [b(0, 10, 80, 40), b(100, 20, 60, 40)];
    applyAlignment(blocks, 'alignRight');
    expect(blocks.map((blk) => blk.y)).toEqual([10, 20]);
  });
});
//=============================================================================
describe('alignCenterH', () => {
  test('all block centres align to average centre x', () => {
    const blocks = [b(0, 0, 100, 40), b(200, 0, 100, 40)];
    applyAlignment(blocks, 'alignCenterH');
    const expectedCX = (50 + 250) / 2; // 150
    blocks.forEach((blk) => expect(blk.x + blk.width / 2).toBeCloseTo(expectedCX));
  });

  test('y coordinates unchanged', () => {
    const blocks = [b(0, 5, 100, 40), b(200, 15, 100, 40)];
    applyAlignment(blocks, 'alignCenterH');
    expect(blocks.map((blk) => blk.y)).toEqual([5, 15]);
  });
});
//=============================================================================
describe('alignTop', () => {
  test('all blocks moved to minimum y', () => {
    const blocks = [b(0, 50), b(100, 20), b(200, 80)];
    applyAlignment(blocks, 'alignTop');
    blocks.forEach((blk) => expect(blk.y).toBe(20));
  });

  test('x coordinates unchanged', () => {
    const blocks = [b(10, 50), b(100, 20), b(200, 80)];
    applyAlignment(blocks, 'alignTop');
    expect(blocks.map((blk) => blk.x)).toEqual([10, 100, 200]);
  });
});
//=============================================================================
describe('alignBottom', () => {
  test('all bottom edges align to maximum bottom', () => {
    const blocks = [b(0, 0, 80, 40), b(100, 100, 80, 60), b(200, 50, 80, 20)];
    applyAlignment(blocks, 'alignBottom');
    blocks.forEach((blk) => expect(blk.y + blk.height).toBe(160));
  });

  test('x coordinates unchanged', () => {
    const blocks = [b(10, 0, 80, 40), b(20, 100, 80, 60)];
    applyAlignment(blocks, 'alignBottom');
    expect(blocks.map((blk) => blk.x)).toEqual([10, 20]);
  });
});
//=============================================================================
describe('alignMiddleV', () => {
  test('all block centres align to average centre y', () => {
    const blocks = [b(0, 0, 80, 100), b(100, 200, 80, 100)];
    applyAlignment(blocks, 'alignMiddleV');
    const expectedCY = (50 + 250) / 2; // 150
    blocks.forEach((blk) => expect(blk.y + blk.height / 2).toBeCloseTo(expectedCY));
  });

  test('x coordinates unchanged', () => {
    const blocks = [b(5, 0, 80, 100), b(15, 200, 80, 100)];
    applyAlignment(blocks, 'alignMiddleV');
    expect(blocks.map((blk) => blk.x)).toEqual([5, 15]);
  });
});
//=============================================================================
describe('distributeH', () => {
  test('three same-width blocks evenly spaced', () => {
    const b1 = b(0, 0, 60, 40);
    const b2 = b(300, 0, 60, 40);
    const b3 = b(180, 0, 60, 40);
    applyAlignment([b1, b2, b3], 'distributeH');
    const xs = [b1, b2, b3].map((blk) => blk.x).sort((a, z) => a - z);
    expect(xs[0]).toBeCloseTo(0);
    expect(xs[1]).toBeCloseTo(150);
    expect(xs[2]).toBeCloseTo(300);
  });

  test('outer blocks unchanged in x', () => {
    const b1 = b(0, 0, 60, 40);
    const b2 = b(100, 0, 60, 40);
    const b3 = b(400, 0, 60, 40);
    applyAlignment([b1, b2, b3], 'distributeH');
    expect(b1.x).toBe(0);
    expect(b3.x).toBe(400);
  });

  test('gaps between all adjacent blocks equal after distribution', () => {
    const blocks = [b(0, 0, 50, 40), b(500, 0, 50, 40), b(250, 0, 50, 40), b(100, 0, 50, 40)];
    applyAlignment(blocks, 'distributeH');
    const sorted = [...blocks].sort((a, z) => a.x - z.x);
    const gaps = sorted.slice(1).map((blk, i) => blk.x - (sorted[i].x + sorted[i].width));
    const first = gaps[0];
    gaps.forEach((g) => expect(g).toBeCloseTo(first));
  });
});
//=============================================================================
describe('distributeV', () => {
  test('three same-height blocks evenly spaced vertically', () => {
    const b1 = b(0, 0, 80, 60);
    const b2 = b(0, 300, 80, 60);
    const b3 = b(0, 180, 80, 60);
    applyAlignment([b1, b2, b3], 'distributeV');
    const ys = [b1, b2, b3].map((blk) => blk.y).sort((a, z) => a - z);
    expect(ys[0]).toBeCloseTo(0);
    expect(ys[1]).toBeCloseTo(150);
    expect(ys[2]).toBeCloseTo(300);
  });

  test('gaps between all adjacent blocks equal after distribution', () => {
    const blocks = [b(0, 0, 50, 50), b(0, 500, 50, 50), b(0, 250, 50, 50), b(0, 100, 50, 50)];
    applyAlignment(blocks, 'distributeV');
    const sorted = [...blocks].sort((a, z) => a.y - z.y);
    const gaps = sorted.slice(1).map((blk, i) => blk.y - (sorted[i].y + sorted[i].height));
    const first = gaps[0];
    gaps.forEach((g) => expect(g).toBeCloseTo(first));
  });
});
//=============================================================================
describe('snapToGrid', () => {
  test('snaps x and y to nearest multiple of GRID_SIZE (10)', () => {
    const blocks = [b(13, 27), b(95, 44)];
    applyAlignment(blocks, 'snapToGrid');
    expect(blocks[0].x).toBe(10);
    expect(blocks[0].y).toBe(30);
    expect(blocks[1].x).toBe(100);
    expect(blocks[1].y).toBe(40);
  });

  test('already on-grid values are unchanged', () => {
    const blocks = [b(0, 0), b(80, 40)];
    const original = clone(blocks);
    applyAlignment(blocks, 'snapToGrid');
    expect(blocks).toEqual(original);
  });

  test('width and height not mutated', () => {
    const blocks = [b(13, 17, 75, 38)];
    // only 1 block but snapToGrid requires 2
    const twoBlocks = [{ ...blocks[0] }, b(100, 100)];
    applyAlignment(twoBlocks, 'snapToGrid');
    expect(twoBlocks[0].width).toBe(75);
    expect(twoBlocks[0].height).toBe(38);
  });
});
//=============================================================================
describe('idempotency', () => {
  const actions = [
    'alignLeft',
    'alignRight',
    'alignCenterH',
    'alignTop',
    'alignBottom',
    'alignMiddleV',
  ] as const;

  const seed = [b(30, 70, 80, 40), b(150, 20, 100, 60), b(80, 110, 60, 30)];

  actions.forEach((action) => {
    test(`${action}: applying twice gives same result as once`, () => {
      const once = clone(seed);
      applyAlignment(once, action);

      const twice = clone(once);
      applyAlignment(twice, action);

      once.forEach((blk, i) => {
        expect(twice[i].x).toBeCloseTo(blk.x);
        expect(twice[i].y).toBeCloseTo(blk.y);
      });
    });
  });
});
//=============================================================================
