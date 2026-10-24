//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Pinned waypoints persist in the .nflow JSON as a compact third element
// ([x, y, 1]); pin-free wires serialize exactly as before (2-tuples).
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { describe, expect, test } from 'vitest';
import { serializeDiagram } from './file-operations.js';
//=============================================================================
function makeState(connections: any[]): any {
  return {
    blocks: new Map(),
    connections,
    author: '',
    name: 'test',
    comments: '',
    variables: '',
  };
}
//=============================================================================
describe('pinned waypoint serialization', () => {
  test('pins serialize as [x, y, 1]; plain points stay 2-tuples', () => {
    const s = makeState([
      {
        from: 'a',
        to: 'b',
        fromIndex: 0,
        toIndex: 0,
        points: [
          { x: 0, y: 0 },
          { x: 100, y: 0, pin: true },
          { x: 100, y: 80, pin: true },
          { x: 200, y: 80 },
        ],
      },
    ]);
    const out = serializeDiagram(s) as any;
    expect(out.connections[0].points).toEqual([
      [0, 0],
      [100, 0, 1],
      [100, 80, 1],
      [200, 80],
    ]);
  });

  test('pin-free wires keep the historical 2-tuple format', () => {
    const s = makeState([
      {
        from: 'a',
        to: 'b',
        fromIndex: 0,
        toIndex: 0,
        points: [
          { x: 0, y: 0 },
          { x: 200, y: 0 },
        ],
      },
    ]);
    const out = serializeDiagram(s) as any;
    expect(out.connections[0].points).toEqual([
      [0, 0],
      [200, 0],
    ]);
  });
});
//=============================================================================
