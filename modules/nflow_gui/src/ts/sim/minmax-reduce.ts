//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Display-only min/max bucket reduction.
//
// A series is reduced to at most ~4 points per bucket (first, min, max, last, in
// chronological order) over `maxBuckets` time buckets. Every visible peak of both
// polarities is kept and the SVG path size is bounded by the plot resolution.
//
// Exact data for export/limits/hover is untouched. Non-finite samples arrive as
// nulls (gaps); the first null of a bucket is kept so gaps survive.
//=============================================================================
export type ReducedSeries = {
  times: number[];
  values: Array<number | null>;
  /** True when the input was returned unchanged (already small enough). */
  passthrough: boolean;
};
// Buckets partition [times[0], tEnd], so variable-step time vectors reduce
// correctly per pixel column.
export function reduceMinMax(
  times: number[],
  values: Array<number | null>,
  maxBuckets: number
): ReducedSeries {
  const n = Math.min(times.length, values.length);
  const buckets = Math.max(1, Math.floor(maxBuckets));
  if (n <= buckets * 4) {
    return { times, values, passthrough: true };
  }
  const t0 = Number(times[0]);
  const t1 = Number(times[n - 1]);
  const span = t1 - t0;
  if (!(span > 0)) {
    // Degenerate time axis: index-based stride cap.
    const stride = Math.ceil(n / (buckets * 4));
    const outT: number[] = [];
    const outV: Array<number | null> = [];
    for (let i = 0; i < n; i += stride) {
      outT.push(times[i]);
      outV.push(values[i]);
    }
    if ((n - 1) % stride !== 0) {
      outT.push(times[n - 1]);
      outV.push(values[n - 1]);
    }
    return { times: outT, values: outV, passthrough: false };
  }

  const outT: number[] = [];
  const outV: Array<number | null> = [];
  let i = 0;
  for (let b = 0; b < buckets && i < n; b += 1) {
    const bucketEnd = b === buckets - 1 ? Infinity : t0 + ((b + 1) * span) / buckets;
    const first = i;
    let minIdx = -1;
    let maxIdx = -1;
    let nullIdx = -1;
    let last = i;
    for (; i < n && Number(times[i]) < bucketEnd; i += 1) {
      last = i;
      const v = values[i];
      if (v == null || !Number.isFinite(Number(v))) {
        if (nullIdx < 0) nullIdx = i;
        continue;
      }
      const num = Number(v);
      if (minIdx < 0 || num < Number(values[minIdx])) minIdx = i;
      if (maxIdx < 0 || num > Number(values[maxIdx])) maxIdx = i;
    }
    const keep = [first, minIdx, maxIdx, nullIdx, last]
      .filter((k) => k >= 0)
      .sort((a, b2) => a - b2);
    let prev = -1;
    for (const k of keep) {
      if (k === prev) continue;
      prev = k;
      outT.push(times[k]);
      outV.push(values[k]);
    }
  }
  return { times: outT, values: outV, passthrough: false };
}
/**
 * Index-stride cap for parametric (XY/XYZ) curves: at most ~maxPoints points,
 * first and last included. Time-bucket min/max does not apply to a parametric
 * trace.
 */
export function stridePoints(length: number, maxPoints: number): number[] | null {
  const cap = Math.max(2, Math.floor(maxPoints));
  if (length <= cap) return null;
  const stride = Math.ceil(length / cap);
  const out: number[] = [];
  for (let i = 0; i < length; i += stride) out.push(i);
  if (out[out.length - 1] !== length - 1) out.push(length - 1);
  return out;
}
//=============================================================================
