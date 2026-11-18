//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { makeAlignConnectHandlers } from './renderer-align-connect';
//=============================================================================
// A block with one output and one input, its port positions driven by a stub.
const block = (id: string, x: number, y: number, extra: any = {}) => ({
  id,
  x,
  y,
  width: 80,
  height: 40,
  ports: [
    { type: 'out', index: 0 },
    { type: 'in', index: 0 },
  ],
  ...extra,
});
//=============================================================================
// Positions: output on the block's right edge, input on its left edge, both at
// the block's mid-height, so two blocks at the same y line up out->in.
const makeCtx = (blocks: any[], connections: any[] = []) => {
  const map = new Map(blocks.map((b) => [b.id, b]));
  const getPortPosition = (id: string, type: 'in' | 'out') => {
    const b = map.get(id);
    if (!b) return null;
    return { x: type === 'out' ? b.x + b.width : b.x, y: b.y + b.height / 2 };
  };
  return {
    svg: null,
    overlayLayer: null,
    createConnection: () => ({}),
    getPortPosition,
    state: { blocks: map, connections },
  } as any;
};
//=============================================================================
describe('makeAlignConnectHandlers.findAlignPair', () => {
  test('finds an aligned, clear, unconnected output->input pair', () => {
    const ctx = makeCtx([block('a', 0, 100), block('b', 200, 100)]);
    const { findAlignPair } = makeAlignConnectHandlers(ctx);
    expect(findAlignPair()).toMatchObject({ from: 'a', fromIndex: 0, to: 'b', toIndex: 0 });
  });

  test('returns null when the ports are not vertically aligned', () => {
    const ctx = makeCtx([block('a', 0, 100), block('b', 200, 140)]);
    const { findAlignPair } = makeAlignConnectHandlers(ctx);
    expect(findAlignPair()).toBeNull();
  });

  test('returns null when the aligned blocks are too far apart', () => {
    const ctx = makeCtx([block('a', 0, 100), block('b', 700, 100)]);
    const { findAlignPair } = makeAlignConnectHandlers(ctx);
    expect(findAlignPair()).toBeNull();
  });

  test('ignores a pair whose input is already occupied', () => {
    const ctx = makeCtx(
      [block('a', 0, 100), block('b', 200, 100)],
      [{ from: 'c', fromIndex: 0, to: 'b', toIndex: 0 }]
    );
    const { findAlignPair } = makeAlignConnectHandlers(ctx);
    expect(findAlignPair()).toBeNull();
  });

  test('ignores a pair already connected', () => {
    const ctx = makeCtx(
      [block('a', 0, 100), block('b', 200, 100)],
      [{ from: 'a', fromIndex: 0, to: 'b', toIndex: 0 }]
    );
    const { findAlignPair } = makeAlignConnectHandlers(ctx);
    expect(findAlignPair()).toBeNull();
  });

  test('returns null when a block sits in the corridor between the ports', () => {
    const ctx = makeCtx([
      block('a', 0, 100),
      block('b', 300, 100),
      // Obstacle with no ports (not an endpoint), overlapping the a->b corridor.
      block('mid', 150, 100, { ports: [] }),
    ]);
    const { findAlignPair } = makeAlignConnectHandlers(ctx);
    expect(findAlignPair()).toBeNull();
  });
});
//=============================================================================
