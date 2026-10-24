//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { makeWireHelpers } from './wire/wire-helpers';
//=============================================================================
describe('render/wire-helpers focused behaviors', () => {
  test('samePoint and dedupePoints behave as expected', () => {
    const h = makeWireHelpers();
    const a = { x: 1, y: 2 };
    const b = { x: 1, y: 2 };
    const c = { x: 2, y: 3 };
    expect(h.samePoint(a, b)).toBe(true);
    expect(h.samePoint(a, c)).toBe(false);
    const pts = [a, b, c, c];
    const dedup = h.dedupePoints(pts as any);
    expect(dedup.length).toBe(2);
  });

  test('removeColinearPoints removes straight middle points', () => {
    const h = makeWireHelpers();
    const pts = [
      { x: 0, y: 0 },
      { x: 0, y: 1 },
      { x: 0, y: 2 },
      { x: 1, y: 2 },
    ];
    const out = h.removeColinearPoints(pts as any);
    expect(out).toEqual([
      { x: 0, y: 0 },
      { x: 0, y: 2 },
      { x: 1, y: 2 },
    ]);
  });

  test('segmentOrientation and getTurnIndices detect turns', () => {
    const h = makeWireHelpers();
    const pts = [
      { x: 0, y: 0 },
      { x: 1, y: 0 },
      { x: 1, y: 1 },
      { x: 2, y: 1 },
    ];
    expect(h.segmentOrientation(pts[0] as any, pts[1] as any)).toBe('H');
    expect(h.segmentOrientation(pts[1] as any, pts[2] as any)).toBe('V');
    const turns = h.getTurnIndices(pts as any);
    expect(turns.length).toBeGreaterThan(0);
  });

  test('buildOrthogonalHead and Tail produce an L-shape for diagonal points', () => {
    const h = makeWireHelpers();
    const from = { x: 0, y: 0 };
    const to = { x: 2, y: 3 };
    const head = h.buildOrthogonalHead(from as any, to as any);
    expect(head.length).toBe(3);
    const tail = h.buildOrthogonalTail(from as any, to as any);
    expect(tail.length).toBe(3);
  });

  test('getPortPositionRaw and getPortPosition apply rotation when present', () => {
    // block at (0,0) size 10x10 with a port at (1,2)
    const block = {
      x: 0,
      y: 0,
      width: 10,
      height: 10,
      rotation: 90,
      ports: [{ x: 1, y: 2, type: 'out', index: 0 }],
    };
    const blocks = new Map<string, any>([['blk', block]]);
    const h = makeWireHelpers({ state: { blocks } as any });
    const raw = h.getPortPositionRaw('blk', 'out', 0);
    expect(raw).toEqual({ x: 1, y: 2 });
    const pos = h.getPortPosition('blk', 'out', 0);
    expect(pos).not.toEqual(raw);
    // verify rotation via known rotatePoint behavior: center at (5,5)
    expect(pos).toEqual({ x: 8, y: 1 });
  });

  test('getWirePosition prefers points then falls back to ports', () => {
    const h = makeWireHelpers();
    const connWithPoints = { points: [{ x: 9, y: 9 }] };
    expect(h.getWirePosition(connWithPoints as any)).toEqual({ x: 9, y: 9 });
    const blocks = new Map<string, any>([
      [
        'a',
        {
          x: 0,
          y: 0,
          ports: [
            { x: 1, y: 1, type: 'out', index: 0 },
            { x: 0, y: 1, type: 'in', index: 0 },
          ],
        },
      ],
    ]);
    const h2 = makeWireHelpers({ state: { blocks } as any });
    const conn = { from: 'a', to: 'a', fromIndex: 0, toIndex: 0 } as any;
    const wp = h2.getWirePosition(conn);
    expect(wp).toEqual({ x: 1, y: 1 });
  });
});
//=============================================================================
