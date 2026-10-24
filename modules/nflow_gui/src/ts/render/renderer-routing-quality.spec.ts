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
import { makeRoutingQualityHandlers } from './wire/renderer-routing-quality';
//=============================================================================
describe('routeQualityScore', () => {
  test('returns Infinity for null, empty, or non-array input', () => {
    const { routeQualityScore } = makeRoutingQualityHandlers({ state: {} } as any);
    expect(routeQualityScore(null as any)).toBe(Number.POSITIVE_INFINITY);
    expect(routeQualityScore([])).toBe(Number.POSITIVE_INFINITY);
    expect(routeQualityScore([{ x: 0, y: 0 }])).toBe(Number.POSITIVE_INFINITY);
  });

  test('returns Infinity for diagonal segment', () => {
    const { routeQualityScore } = makeRoutingQualityHandlers({ state: {} } as any);
    const result = routeQualityScore([
      { x: 0, y: 0 },
      { x: 10, y: 10 },
    ]);
    expect(result).toBe(Number.POSITIVE_INFINITY);
  });

  test('scores horizontal path as length/GRID_SIZE', () => {
    const { routeQualityScore } = makeRoutingQualityHandlers({ state: {} } as any);
    const result = routeQualityScore([
      { x: 0, y: 0 },
      { x: 20, y: 0 },
    ]);
    expect(result).toBeCloseTo(2);
  });

  test('adds 20 per turn to the score', () => {
    const { routeQualityScore } = makeRoutingQualityHandlers({ state: {} } as any);
    // H=20 + V=10: 1 turn * 20 + 30/10 = 23
    const result = routeQualityScore([
      { x: 0, y: 0 },
      { x: 20, y: 0 },
      { x: 20, y: 10 },
    ]);
    expect(result).toBeCloseTo(23);
  });
});
//=============================================================================
describe('captureConnectionQuality', () => {
  test('returns map with hasPath=true and finite score for valid connection', () => {
    const { captureConnectionQuality } = makeRoutingQualityHandlers({ state: {} } as any);
    const conn: any = {
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
      ],
    };
    const quality = captureConnectionQuality(new Set([conn]));
    expect(quality.get(conn)).toBeDefined();
    expect(quality.get(conn).hasPath).toBe(true);
    expect(Number.isFinite(quality.get(conn).score)).toBe(true);
  });

  test('returns hasPath=false for connection with no points', () => {
    const { captureConnectionQuality } = makeRoutingQualityHandlers({ state: {} } as any);
    const conn: any = { points: [] };
    const quality = captureConnectionQuality(new Set([conn]));
    expect(quality.get(conn).hasPath).toBe(false);
  });

  test('handles empty set', () => {
    const { captureConnectionQuality } = makeRoutingQualityHandlers({ state: {} } as any);
    const quality = captureConnectionQuality(new Set());
    expect(quality.size).toBe(0);
  });
});
//=============================================================================
describe('computeDirtyConnections', () => {
  test('returns initial dirtyConnections when no dirtyBlocks', () => {
    const connA: any = {
      id: 'c1',
      from: 'x',
      to: 'y',
      points: [
        { x: 0, y: 0 },
        { x: 0, y: 10 },
      ],
    };
    const state: any = {
      dirtyConnections: new Set([connA]),
      dirtyBlocks: new Set(),
      connections: [connA],
      blocks: new Map(),
    };
    const { computeDirtyConnections } = makeRoutingQualityHandlers({ state } as any);
    const result = computeDirtyConnections();
    expect(result.has(connA)).toBe(true);
  });

  test('marks connection dirty when its from-block was moved', () => {
    const conn: any = {
      from: 'b1',
      to: 'b2',
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
      ],
    };
    const blocks = new Map([['b1', { id: 'b1', x: 0, y: 0, width: 10, height: 10 }]]);
    const state: any = {
      dirtyConnections: new Set(),
      dirtyBlocks: new Set(['b1']),
      connections: [conn],
      blocks,
    };
    const { computeDirtyConnections } = makeRoutingQualityHandlers({ state } as any);
    const result = computeDirtyConnections();
    expect(result.has(conn)).toBe(true);
  });
});
//=============================================================================
test('routeQualityScore handles invalid inputs and non-orthogonal segments', () => {
  const handlers = makeRoutingQualityHandlers({ state: {} } as any);
  const { routeQualityScore } = handlers;
  expect(routeQualityScore(null)).toBe(Number.POSITIVE_INFINITY);
  expect(routeQualityScore([])).toBe(Number.POSITIVE_INFINITY);
  expect(
    routeQualityScore([
      { x: 0, y: 0 },
      { x: 10, y: 10 },
    ])
  ).toBe(Number.POSITIVE_INFINITY);
});
//=============================================================================
test('routeQualityScore computes expected score for orthogonal paths', () => {
  const handlers = makeRoutingQualityHandlers({ state: {} } as any);
  const { routeQualityScore } = handlers;
  // length 20, GRID_SIZE 10 -> 2
  expect(
    routeQualityScore([
      { x: 0, y: 0 },
      { x: 20, y: 0 },
    ])
  ).toBeCloseTo(2);
  // one turn: 20 + 30/10 = 23
  expect(
    routeQualityScore([
      { x: 0, y: 0 },
      { x: 20, y: 0 },
      { x: 20, y: 10 },
    ])
  ).toBeCloseTo(23);
});
//=============================================================================
test('captureConnectionQuality returns map with scores and hasPath flags', () => {
  const ctx: any = { state: {} };
  const handlers = makeRoutingQualityHandlers(ctx);
  const { captureConnectionQuality } = handlers;
  const conn = {
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
  };
  const set = new Set([conn]);
  const quality = captureConnectionQuality(set);
  expect(quality.get(conn)).toBeDefined();
  expect(quality.get(conn).hasPath).toBe(true);
  expect(Number.isFinite(quality.get(conn).score)).toBe(true);
});
//=============================================================================
test('computeDirtyConnections returns initial dirtyConnections when no dirtyBlocks', () => {
  const connA = { id: 'c1' };
  const state: any = {
    dirtyConnections: new Set([connA]),
    dirtyBlocks: new Set(),
    connections: [connA],
    blocks: new Map(),
  };
  const ctx: any = { state };
  const handlers = makeRoutingQualityHandlers(ctx);
  const { computeDirtyConnections } = handlers;
  const res = computeDirtyConnections();
  expect(res.has(connA)).toBe(true);
});
//=============================================================================
test('computeDirtyConnections marks connections dirty when moved block affects them', () => {
  const connB = {
    id: 'c2',
    from: 'b1',
    to: 'b2',
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
  };
  const block = { id: 'b1' };
  const blocks = new Map();
  blocks.set('b1', block);
  const state: any = {
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(['b1']),
    connections: [connB],
    blocks,
  };
  const ctx: any = { state };
  const handlers = makeRoutingQualityHandlers(ctx);
  const { computeDirtyConnections } = handlers;
  const res = computeDirtyConnections();
  expect(res.has(connB)).toBe(true);
});
//=============================================================================
import fs from 'fs';
import path from 'path';
//=============================================================================
test('loads renderer-routing-quality.ts if present', async () => {
  const p = path.resolve(
    process.cwd(),
    'modules/nflow_gui/src/ts',
    'render/renderer-routing-quality.ts'
  );
  if (!fs.existsSync(p)) {
    return;
  }
  const mod = await import('./wire/renderer-routing-quality');
  expect(mod).toBeDefined();
});
//=============================================================================
