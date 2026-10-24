//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect } from 'vitest';
import { orderFanOutTurns } from './fan-order';
//=============================================================================
type P = { x: number; y: number };
const z = (
  from: string,
  fromIndex: number,
  to: string,
  y0: number,
  col: number,
  y1: number,
  x1 = 760
) => ({
  from,
  fromIndex,
  to,
  toIndex: 0,
  points: [
    { x: 600, y: y0 },
    { x: col, y: y0 },
    { x: col, y: y1 },
    { x: x1, y: y1 },
  ] as P[],
});
//=============================================================================
const cols = (conns: Array<{ points: P[] }>) => conns.map((c) => c.points[1].x);
const crossings = (conns: Array<{ points: P[] }>) => {
  let n = 0;
  for (let i = 0; i < conns.length; i += 1)
    for (let j = 0; j < conns.length; j += 1) {
      if (i === j) continue;
      const [a0, a1] = [conns[i].points[0], conns[i].points[1]]; // first run of i
      const [b1, b2] = [conns[j].points[1], conns[j].points[2]]; // turn of j
      const lo = Math.min(b1.y, b2.y);
      const hi = Math.max(b1.y, b2.y);
      if (b1.x > Math.min(a0.x, a1.x) && b1.x < Math.max(a0.x, a1.x) && a0.y > lo && a0.y < hi)
        n += 1;
      const [a2, a3] = [conns[i].points[2], conns[i].points[3]]; // last run of i
      if (b1.x > Math.min(a2.x, a3.x) && b1.x < Math.max(a2.x, a3.x) && a3.y > lo && a3.y < hi)
        n += 1;
    }
  return n;
};
//=============================================================================
describe('orderFanOutTurns', () => {
  it('lets the top wire of an upward fan turn first', () => {
    // Triple pendulum shape: out0 (313) climbs to 220, out1 (347) climbs to 310.
    const w0 = z('m', 0, 's', 313, 740, 220);
    const w1 = z('m', 1, 's', 347, 680, 310);
    const conns = [w0, w1];
    expect(crossings(conns)).toBeGreaterThan(0);
    expect(orderFanOutTurns(conns, [])).toBe(1);
    expect(cols(conns)).toEqual([680, 740]);
    expect(crossings(conns)).toBe(0);
    expect(w0.points[0]).toEqual({ x: 600, y: 313 });
    expect(w0.points[3]).toEqual({ x: 760, y: 220 });
  });

  it('lets the bottom wire of a downward fan turn first', () => {
    const w3 = z('m', 3, 't', 413, 710, 560);
    const w4 = z('m', 4, 't', 447, 740, 730);
    const conns = [w3, w4];
    expect(crossings(conns)).toBeGreaterThan(0);
    expect(orderFanOutTurns(conns, [])).toBe(1);
    expect(cols(conns)).toEqual([740, 710]);
    expect(crossings(conns)).toBe(0);
  });

  it('orders a fan entering one block from several sources', () => {
    // p (100) ends above q (200) on the same block: the upper wire must turn last.
    const a = { ...z('p', 0, 'sum', 100, 680, 240), toIndex: 0 };
    const b = { ...z('q', 0, 'sum', 200, 700, 260), toIndex: 1 };
    const conns = [a, b];
    expect(crossings(conns)).toBeGreaterThan(0);
    expect(orderFanOutTurns(conns, [])).toBe(1);
    expect(cols(conns)).toEqual([700, 680]);
    expect(crossings(conns)).toBe(0);
  });

  it('keeps a fan whose spans nest (the crossing is unavoidable)', () => {
    const a = { ...z('p', 0, 'sum', 100, 680, 260), toIndex: 0 };
    const b = { ...z('q', 0, 'sum', 200, 700, 240), toIndex: 1 };
    const before = JSON.stringify([a, b]);
    expect(orderFanOutTurns([a, b], [])).toBe(0);
    expect(JSON.stringify([a, b])).toBe(before);
  });

  it('leaves an already ordered fan and non-Z wires alone', () => {
    const w0 = z('m', 0, 's', 313, 680, 220);
    const w1 = z('m', 1, 's', 347, 740, 310);
    const straight = {
      from: 'm',
      fromIndex: 2,
      to: 'u',
      toIndex: 0,
      points: [
        { x: 600, y: 380 },
        { x: 760, y: 380 },
      ],
    };
    const before = JSON.stringify([w0, w1, straight]);
    expect(orderFanOutTurns([w0, w1, straight], [])).toBe(0);
    expect(JSON.stringify([w0, w1, straight])).toBe(before);
  });

  it('reverts the whole fan when a new column would cross a block', () => {
    const w0 = z('m', 0, 's', 313, 740, 220);
    const w1 = z('m', 1, 's', 347, 680, 310);
    // A block sitting on column 680 between 220 and 313 forbids w0 turning there.
    const blocker = { id: 'k', x: 660, y: 240, width: 40, height: 40 };
    const before = JSON.stringify([w0, w1]);
    expect(orderFanOutTurns([w0, w1], [blocker])).toBe(0);
    expect(JSON.stringify([w0, w1])).toBe(before);
  });

  it('orders a fan of four whose members share a column with disjoint spans', () => {
    // Triple pendulum: 340/360/400/420 fan out to
    // 210/300 (up) and 560/730 (down); w1 and w3 both turn at 620.
    const w0 = z('m', 0, 's', 340, 740, 210);
    const w1 = z('m', 1, 's', 360, 620, 300);
    const w3 = z('m', 3, 't', 400, 620, 560);
    const w4 = z('m', 4, 't', 420, 730, 730);
    const conns = [w0, w1, w3, w4];
    expect(crossings(conns)).toBe(4);
    expect(orderFanOutTurns(conns, [])).toBeGreaterThan(0);
    expect(crossings(conns)).toBe(0);
    expect(
      cols(conns)
        .slice()
        .sort((p, q) => p - q)
    ).toEqual([620, 620, 730, 740]);
  });

  it('reads a Z wire through the grid steps the router leaves on it', () => {
    const w0 = z('m', 0, 's', 313, 740, 220);
    const w1 = {
      from: 'm',
      fromIndex: 1,
      to: 's',
      toIndex: 0,
      points: [
        { x: 600, y: 347 },
        { x: 640, y: 347 },
        { x: 680, y: 347 },
        { x: 680, y: 320 },
        { x: 680, y: 310 },
        { x: 760, y: 310 },
      ] as P[],
    };
    expect(orderFanOutTurns([w0, w1], [])).toBe(1);
    expect(w1.points).toEqual([
      { x: 600, y: 347 },
      { x: 740, y: 347 },
      { x: 740, y: 310 },
      { x: 760, y: 310 },
    ]);
    expect(cols([w0, w1])).toEqual([680, 740]);
  });

  it('skips pinned wires and fans whose members overlap on one column', () => {
    const w0 = z('m', 0, 's', 313, 740, 220);
    const w1 = z('m', 1, 's', 347, 740, 310);
    expect(orderFanOutTurns([w0, w1], [])).toBe(0);
    const p0 = z('m', 0, 's', 313, 740, 220);
    (p0.points[1] as P & { pin?: boolean }).pin = true;
    const p1 = z('m', 1, 's', 347, 680, 310);
    expect(orderFanOutTurns([p0, p1], [])).toBe(0);
  });
});
//=============================================================================
