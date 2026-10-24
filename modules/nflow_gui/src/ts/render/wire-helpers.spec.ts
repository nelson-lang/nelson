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
  makeWireHelpers,
  samePoint,
  dedupePoints,
  removeColinearPoints,
} from './wire/wire-helpers.js';
//=============================================================================
describe('wire-helpers utilities', () => {
  test('samePoint works for equal and nulls', () => {
    expect(samePoint({ x: 1, y: 2 }, { x: 1, y: 2 })).toBe(true);
    expect(samePoint({ x: 1, y: 2 }, { x: 2, y: 2 })).toBe(false);
    expect(samePoint(null, { x: 1, y: 2 })).toBe(false);
  });

  test('dedupePoints removes consecutive duplicates', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 0, y: 0 },
      { x: 1, y: 0 },
    ];
    const out = dedupePoints(pts as any);
    expect(out.length).toBe(2);
    expect(out[1]).toEqual({ x: 1, y: 0 });
  });

  test('removeColinearPoints removes straight-line midpoints', () => {
    const pts = [
      { x: 0, y: 0 },
      { x: 1, y: 0 },
      { x: 2, y: 0 },
      { x: 2, y: 1 },
    ];
    const out = removeColinearPoints(pts as any);
    expect(out.some((p) => p.x === 1 && p.y === 0)).toBe(false);
    expect(out.length).toBe(3);
  });

  test('makeWireHelpers: port position reading and orthogonal helpers', () => {
    const state: any = { blocks: new Map() };
    state.blocks.set('b1', {
      x: 10,
      y: 20,
      ports: [
        { x: 0, y: 5, type: 'in', index: 0 },
        { x: 1, y: 2, type: 'out', index: 0 },
      ],
      rotation: 0,
      width: 10,
      height: 10,
    });
    const h = makeWireHelpers({ state } as any);
    // lookup is by (type, index), not by array position: ports[0] is the
    // 'in' port here, yet asking for 'out'/0 must resolve the 'out' port
    const raw = h.getPortPositionRaw('b1', 'out', 0);
    expect(raw).toEqual({ x: 11, y: 22 });
    const rawIn = h.getPortPositionRaw('b1', 'in', 0);
    expect(rawIn).toEqual({ x: 10, y: 25 });
    const pos = h.getPortPosition('b1', 'out', 0);
    expect(pos).toEqual(raw);
    const head = h.buildOrthogonalHead({ x: 0, y: 0 }, { x: 2, y: 2 });
    expect(head.length).toBe(3);
    const tail = h.buildOrthogonalTail({ x: 0, y: 0 }, { x: 2, y: 2 });
    expect(tail[0]).toEqual(head[2]);
  });
});
//=============================================================================
