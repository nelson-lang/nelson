//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Display-only min/max bucket reduction: peaks preserved, first and
// last kept, output bounded by the bucket budget, gaps (nulls) survive.
//=============================================================================
import { describe, expect, test } from 'vitest';
import { reduceMinMax, stridePoints } from './minmax-reduce.js';
//=============================================================================
function ramp(n: number): { times: number[]; values: number[] } {
  const times = Array.from({ length: n }, (_, i) => i * 0.001);
  const values = times.map((t) => Math.sin(20 * t));
  return { times, values };
}
//=============================================================================
describe('reduceMinMax', () => {
  test('small input passes through untouched', () => {
    const { times, values } = ramp(100);
    const out = reduceMinMax(times, values, 300);
    expect(out.passthrough).toBe(true);
    expect(out.times).toBe(times);
    expect(out.values).toBe(values);
  });

  test('output is bounded by ~4 points per bucket', () => {
    const { times, values } = ramp(100000);
    const out = reduceMinMax(times, values, 300);
    expect(out.passthrough).toBe(false);
    expect(out.values.length).toBeLessThanOrEqual(300 * 4);
    expect(out.values.length).toBeGreaterThan(0);
  });

  test('first and last points are preserved', () => {
    const { times, values } = ramp(50000);
    const out = reduceMinMax(times, values, 200);
    expect(out.times[0]).toBe(times[0]);
    expect(out.values[0]).toBe(values[0]);
    expect(out.times[out.times.length - 1]).toBe(times[times.length - 1]);
    expect(out.values[out.values.length - 1]).toBe(values[values.length - 1]);
  });

  test('positive and negative peaks survive the reduction', () => {
    const n = 50000;
    const times = Array.from({ length: n }, (_, i) => i);
    const values = new Array<number>(n).fill(0);
    values[12345] = 99; // isolated positive spike
    values[34567] = -77; // isolated negative spike
    const out = reduceMinMax(times, values, 100);
    expect(out.values).toContain(99);
    expect(out.values).toContain(-77);
  });

  test('constant, monotone and oscillating data keep their envelope', () => {
    const n = 20000;
    const times = Array.from({ length: n }, (_, i) => i);
    for (const gen of [() => 5, (i: number) => i * 0.01, (i: number) => (i % 2 === 0 ? 1 : -1)]) {
      const values = times.map((_, i) => gen(i));
      const out = reduceMinMax(times, values, 128);
      const inMin = Math.min(...values);
      const inMax = Math.max(...values);
      const outMin = Math.min(...(out.values as number[]));
      const outMax = Math.max(...(out.values as number[]));
      expect(outMin).toBe(inMin);
      expect(outMax).toBe(inMax);
    }
  });

  test('null gaps are preserved per bucket', () => {
    const n = 10000;
    const times = Array.from({ length: n }, (_, i) => i);
    const values: Array<number | null> = times.map((_, i) => (i % 100 === 50 ? null : i * 0.1));
    const out = reduceMinMax(times, values, 50);
    expect(out.values).toContain(null);
  });

  test('non-uniform (variable-step) time buckets by time, not index', () => {
    // 90% of the samples land in the first 10% of the time span; a spike in
    // the sparse tail must still survive because buckets partition TIME.
    const times: number[] = [];
    const values: number[] = [];
    for (let i = 0; i < 9000; i += 1) {
      times.push(i * 0.001); // 0 .. 9
      values.push(0);
    }
    for (let i = 0; i < 90; i += 1) {
      times.push(9 + i); // 9 .. 99
      values.push(i === 45 ? 42 : 1);
    }
    const out = reduceMinMax(times, values, 64);
    expect(out.values).toContain(42);
  });

  test('degenerate time span falls back to an index stride', () => {
    const n = 10000;
    const times = new Array<number>(n).fill(1.5);
    const values = Array.from({ length: n }, (_, i) => i);
    const out = reduceMinMax(times, values, 100);
    expect(out.passthrough).toBe(false);
    expect(out.values.length).toBeLessThanOrEqual(100 * 4 + 1);
    expect(out.values[out.values.length - 1]).toBe(n - 1);
  });
});
//=============================================================================
describe('stridePoints', () => {
  test('pass-through under the cap', () => {
    expect(stridePoints(100, 200)).toBeNull();
  });

  test('bounded output keeps first and last index', () => {
    const idx = stridePoints(100000, 500)!;
    expect(idx.length).toBeLessThanOrEqual(502);
    expect(idx[0]).toBe(0);
    expect(idx[idx.length - 1]).toBe(99999);
  });
});
//=============================================================================
