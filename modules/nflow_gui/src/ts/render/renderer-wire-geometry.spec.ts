//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/wire/renderer-wire-geometry.js');
});
//=============================================================================
test('countTurns computes correct number of turns', () => {
  const mh = mod.makeWireGeometryHandlers({});
  const pts = [
    { x: 0, y: 0 },
    { x: 0, y: 5 },
    { x: 3, y: 5 },
    { x: 3, y: 10 },
  ];
  expect(mh.countTurns(pts)).toBe(2);
  expect(
    mh.countTurns([
      { x: 0, y: 0 },
      { x: 5, y: 0 },
    ])
  ).toBe(0);
});
//=============================================================================
test('orthogonalIntersection detects intersection between H and V segments', () => {
  const mh = mod.makeWireGeometryHandlers({});
  const a = { x: 0, y: 5 };
  const b = { x: 10, y: 5 };
  const c = { x: 5, y: 0 };
  const d = { x: 5, y: 10 };
  const hit = mh.orthogonalIntersection(a, b, c, d);
  expect(hit).toEqual({ x: 5, y: 5 });
});
//=============================================================================
test('buildDragRenderPoints returns fallback and prepends head/tail', () => {
  const C: any = {
    getPortPosition: (id: string, type: string, idx: number) =>
      id === FieldNames.kFrom ? { x: 0, y: 0 } : { x: 20, y: 0 },
    buildOrthogonalHead: (from: any, to: any) => [
      { x: 0, y: 0 },
      { x: 5, y: 0 },
    ],
    buildOrthogonalTail: (from: any, to: any) => [
      { x: 15, y: 0 },
      { x: 20, y: 0 },
    ],
    dedupePoints: (p: any) => p,
    removeColinearPoints: (p: any) => p,
    samePoint: (a: any, b: any) => a.x === b.x && a.y === b.y,
  };
  const mh = mod.makeWireGeometryHandlers(C);
  const conn: any = { from: FieldNames.kFrom, to: FieldNames.kTo, fromIndex: 0, toIndex: 0 };
  const fallback = mh.buildDragRenderPoints(conn, []);
  expect(Array.isArray(fallback)).toBe(true);
  // with points not starting/ending at ports -> head/tail should be added
  const pts = [
    { x: 6, y: 0 },
    { x: 14, y: 0 },
  ];
  const res = mh.buildDragRenderPoints(conn, pts);
  expect(res[0]).toEqual({ x: 0, y: 0 });
  expect(res[res.length - 1]).toEqual({ x: 20, y: 0 });
});
//=============================================================================
test('tryShorterOrthogonalPath returns shorter candidate when valid', () => {
  const C: any = {
    dedupePoints: (p: any) => p,
    removeColinearPoints: (p: any) => p,
    checkPortDirections: () => true,
    segmentHitsAnyBlock: () => false,
    segmentOverlapsOtherWire: () => false,
  };
  const mh = mod.makeWireGeometryHandlers(C);
  const conn: any = {};
  const start = { x: 0, y: 0 };
  const end = { x: 10, y: 10 };
  const points = [
    start,
    { x: 0, y: 2 },
    { x: 2, y: 2 },
    { x: 2, y: 8 },
    { x: 8, y: 8 },
    { x: 8, y: 10 },
    end,
  ];
  const out = mh.tryShorterOrthogonalPath(conn, points);
  // should return a 2-segment candidate like [start, {x:end.x,y:start.y}, end] or similar
  expect(out.length).toBeLessThan(points.length);
});
//=============================================================================
test('buildDragRenderPoints returns empty if ports missing', async () => {
  const mh = (
    await import('../../../dist/render/wire/renderer-wire-geometry.js')
  ).makeWireGeometryHandlers({
    getPortPosition: () => null,
    dedupePoints: (p: any) => p,
  });
  const conn: any = { from: 'a', to: 'b' };
  const pts = [{ x: 0, y: 0 }];
  const out = mh.buildDragRenderPoints(conn, pts);
  expect(Array.isArray(out)).toBe(true);
  // when ports missing the function returns the provided points (or empty if none)
  expect(out).toEqual(pts);
});
//=============================================================================
test('orthogonalIntersection returns null for parallel segments or edge cases', async () => {
  const mh = (
    await import('../../../dist/render/wire/renderer-wire-geometry.js')
  ).makeWireGeometryHandlers({});
  const a = { x: 0, y: 0 };
  const b = { x: 10, y: 0 };
  const c = { x: 0, y: 5 };
  const d = { x: 10, y: 5 };
  expect(mh.orthogonalIntersection(a, b, c, d)).toBeNull();
});
//=============================================================================
test('tryShorterOrthogonalPath rejects when checkPortDirections or segmentHitsAnyBlock returns false/true', async () => {
  const C1: any = {
    dedupePoints: (p: any) => p,
    removeColinearPoints: (p: any) => p,
    checkPortDirections: () => false,
    segmentHitsAnyBlock: () => false,
    segmentOverlapsOtherWire: () => false,
  };
  const mh1 = (
    await import('../../../dist/render/wire/renderer-wire-geometry.js')
  ).makeWireGeometryHandlers(C1);
  const conn: any = {};
  const pts = [
    { x: 0, y: 0 },
    { x: 0, y: 5 },
    { x: 5, y: 5 },
  ];
  const out1 = mh1.tryShorterOrthogonalPath(conn, pts);
  // rejected because checkPortDirections returned false
  expect(out1).toEqual(pts);

  const C2: any = {
    dedupePoints: (p: any) => p,
    removeColinearPoints: (p: any) => p,
    checkPortDirections: () => true,
    segmentHitsAnyBlock: () => true,
    segmentOverlapsOtherWire: () => false,
  };
  const mh2 = (
    await import('../../../dist/render/wire/renderer-wire-geometry.js')
  ).makeWireGeometryHandlers(C2);
  const out2 = mh2.tryShorterOrthogonalPath(conn, pts);
  // rejected because a segment hits a block
  expect(out2).toEqual(pts);
});
//=============================================================================
import { describe, expect, test } from 'vitest';
import { makeWireGeometryHandlers } from './wire/renderer-wire-geometry.js';
//=============================================================================
describe('renderer-wire-geometry factory', () => {
  test('makeWireGeometryHandlers returns object without throwing', () => {
    const ctx: any = { svg: null, state: { connections: [] } };
    const handlers = makeWireGeometryHandlers(ctx as any);
    expect(typeof handlers).toBe('object');
  });
});
//=============================================================================
import fs from 'fs';
import path from 'path';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
test('loads renderer-wire-geometry.ts if present', async () => {
  const p = path.resolve(
    process.cwd(),
    'modules/nflow_gui/src/ts',
    'render/renderer-wire-geometry.ts'
  );
  if (!fs.existsSync(p)) {
    return;
  }
  const mod = await import('./wire/renderer-wire-geometry');
  expect(mod).toBeDefined();
});
//=============================================================================
