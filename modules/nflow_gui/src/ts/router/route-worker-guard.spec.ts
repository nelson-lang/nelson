//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment node */
/* eslint-disable @typescript-eslint/no-explicit-any */
let guard: any;
beforeEach(async () => {
  guard = await import('../../../dist/router/route-worker-guard.js');
});
//=============================================================================
test('buildConnectionSignature returns empty for non-array', () => {
  expect(guard.buildConnectionSignature(null)).toBe('');
  expect(guard.buildConnectionSignature({})).toBe('');
});
//=============================================================================
test('buildConnectionSignature builds expected key string', () => {
  const conns = [
    { from: 'a', fromIndex: 0, to: 'b', toIndex: 1 },
    { from: 'c', to: 'd' },
  ];
  const sig = guard.buildConnectionSignature(conns);
  expect(sig).toContain('a:0->b:1');
  expect(sig).toContain('c:0->d:0');
});
//=============================================================================
test('canApplyWorkerRoutes rejects invalid inputs', () => {
  const base = {
    jobEpoch: 1,
    currentEpoch: 1,
    jobSignature: 'x',
    currentSignature: 'x',
    routes: ['r'],
    connectionCount: 1,
  } as any;
  expect(guard.canApplyWorkerRoutes({ ...base, routes: null })).toBe(false);
  expect(guard.canApplyWorkerRoutes({ ...base, connectionCount: -1 })).toBe(false);
  expect(guard.canApplyWorkerRoutes({ ...base, connectionCount: 2 })).toBe(false);
});
//=============================================================================
test('canApplyWorkerRoutes respects epoch and signature mismatches', () => {
  const routes = [{}, {}];
  const base = {
    jobEpoch: 2,
    currentEpoch: 1,
    jobSignature: 's1',
    currentSignature: 's1',
    routes,
    connectionCount: 2,
  } as any;
  expect(guard.canApplyWorkerRoutes(base)).toBe(false);
  const base2 = {
    jobEpoch: 1,
    currentEpoch: 1,
    jobSignature: 's1',
    currentSignature: 's2',
    routes,
    connectionCount: 2,
  } as any;
  expect(guard.canApplyWorkerRoutes(base2)).toBe(false);
});
//=============================================================================
test('canApplyWorkerRoutes returns true for valid matching inputs', () => {
  const routes = [{}, {}];
  const ok = guard.canApplyWorkerRoutes({
    jobEpoch: 1,
    currentEpoch: 1,
    jobSignature: 's',
    currentSignature: 's',
    routes,
    connectionCount: 2,
  });
  expect(ok).toBe(true);
});
//=============================================================================
describe('type contracts', () => {
  test('buildConnectionSignature returns string for arrays and empty string otherwise', () => {
    expect(typeof guard.buildConnectionSignature([])).toBe('string');
    expect(typeof guard.buildConnectionSignature([{ from: 'a' }])).toBe('string');
    expect(typeof guard.buildConnectionSignature(null)).toBe('string');
  });
  test('canApplyWorkerRoutes returns boolean for various inputs', () => {
    expect(typeof guard.canApplyWorkerRoutes({})).toBe('boolean');
    expect(typeof guard.canApplyWorkerRoutes({ jobEpoch: 1 })).toBe('boolean');
  });
});
//=============================================================================
describe('block position hash', () => {
  test('buildBlockPositionHash encodes id:x:y per block', () => {
    const hash = guard.buildBlockPositionHash([
      { id: 'a', x: 10, y: 20 },
      { id: 'b', x: 0, y: 5 },
    ]);
    expect(hash).toBe('a:10:20|b:0:5');
  });

  test('canApplyWorkerRoutes rejects when block positions changed mid-job', () => {
    const routes = [{}, {}];
    const base = {
      jobEpoch: 1,
      currentEpoch: 1,
      jobSignature: 's',
      currentSignature: 's',
      routes,
      connectionCount: 2,
    };
    expect(
      guard.canApplyWorkerRoutes({
        ...base,
        jobPositionHash: 'a:0:0',
        currentPositionHash: 'a:0:10',
      })
    ).toBe(false);
    expect(
      guard.canApplyWorkerRoutes({
        ...base,
        jobPositionHash: 'a:0:0',
        currentPositionHash: 'a:0:0',
      })
    ).toBe(true);
  });

  test('position hash is optional (backward compatible)', () => {
    const ok = guard.canApplyWorkerRoutes({
      jobEpoch: 1,
      currentEpoch: 1,
      routes: [{}],
      connectionCount: 1,
    });
    expect(ok).toBe(true);
  });
});
//=============================================================================
export {};
//=============================================================================
