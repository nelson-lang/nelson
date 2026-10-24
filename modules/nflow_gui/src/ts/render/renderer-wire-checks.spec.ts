//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests for renderer-wire-checks
import { makeWireCheckHandlers } from './wire/renderer-wire-checks';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
const makeConn = (from: string, to: string, points: any[] = []) => ({
  from,
  to,
  points,
  fromIndex: 0,
  toIndex: 0,
});
//=============================================================================
test('isValidStub for each side', () => {
  const S: any = { blocks: new Map(), connections: [] };
  const ctx: any = { state: S };
  const h = makeWireCheckHandlers(ctx as any);
  const p = { x: 10, y: 10 };
  // GRID_SIZE is 10 in geometry, so offsets should be >=10
  expect(h.isValidStub(p, { x: 0, y: 10 }, 'left')).toBe(true);
  expect(h.isValidStub(p, { x: 20, y: 10 }, 'right')).toBe(true);
  expect(h.isValidStub(p, { x: 10, y: 0 }, 'top')).toBe(true);
  expect(h.isValidStub(p, { x: 10, y: 20 }, 'bottom')).toBe(true);
});
//=============================================================================
test('shareSamePort detects shared ports', () => {
  const S: any = { blocks: new Map(), connections: [] };
  const ctx: any = { state: S };
  const h = makeWireCheckHandlers(ctx as any);
  const a = makeConn('a', 'b');
  const b = makeConn('a', 'c');
  expect(h.shareSamePort(a, b)).toBe(true);
  const c = makeConn('x', 'b');
  expect(h.shareSamePort(a, c)).toBe(true);
  const d = makeConn('x', 'y');
  expect(h.shareSamePort(a, d)).toBe(false);
});
//=============================================================================
test('segmentHitsAnyBlock returns true when seg hits keepout', () => {
  const block: any = { id: 'b1', x: 0, y: 0, width: 10, height: 10 };
  const S: any = { blocks: new Map([['b1', block]]), connections: [] };
  const ctx: any = { state: S };
  const h = makeWireCheckHandlers(ctx as any);
  const segs = [{ a: { x: 5, y: 5 }, b: { x: 5, y: 15 } }];
  const issues: any[] = [];
  // segmentHitsAnyBlock ignores from/to blocks; give a different conn so block is considered
  const conn = makeConn('other', 'other2');
  const res = h.segmentHitsAnyBlock(segs as any, conn, issues);
  expect(res).toBe(true);
});
//=============================================================================
test('segmentOverlapsOtherWire detects overlap', () => {
  const otherConn: any = {
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
  };
  const S: any = { blocks: new Map(), connections: [otherConn] };
  const ctx: any = { state: S };
  const h = makeWireCheckHandlers(ctx as any);
  const segs = [{ a: { x: 5, y: 0 }, b: { x: 15, y: 0 }, orientation: 'H' }];
  const issues: any[] = [];
  const res = h.segmentOverlapsOtherWire(segs as any, makeConn('a', 'b'), issues);
  expect(res).toBe(true);
});
//=============================================================================
test('checkPortDirections validates good and reports issues', () => {
  const from: any = {
    id: 'f',
    x: 0,
    y: 0,
    width: 10,
    height: 10,
    ports: [{ type: 'out', index: 0, x: 5, y: 5 }],
    group: document.createElement('div'),
  };
  const to: any = {
    id: 't',
    x: 20,
    y: 0,
    width: 10,
    height: 10,
    ports: [{ type: 'in', index: 0, x: 5, y: 5 }],
    group: document.createElement('div'),
  };
  const conn: any = { from: 'f', to: 't', fromIndex: 0, toIndex: 0 };
  const S: any = {
    blocks: new Map([
      ['f', from],
      ['t', to],
    ]),
    connections: [],
  };
  const ctx: any = { state: S, getWirePosition: undefined };
  const h = makeWireCheckHandlers(ctx as any);
  // build a path that snaps to GRID_SIZE (10) positions and satisfies stubs
  const pts = [
    { x: 10, y: 10 },
    { x: 20, y: 10 },
    { x: 40, y: 10 },
    { x: 30, y: 10 },
  ];
  const issues: any[] = [];
  const ok = h.checkPortDirections(conn, pts, issues);
  expect(ok).toBe(true);
});
//=============================================================================
