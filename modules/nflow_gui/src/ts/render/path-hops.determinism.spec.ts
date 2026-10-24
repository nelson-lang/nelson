//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Deterministic hop ownership: at a crossing, the wire whose content-based
// key sorts first draws the hop: a pure rule, independent of iteration
// order and of full-vs-fast render passes.
//=============================================================================
import { buildSegments, buildPathWithHops, connOrderKey } from './wire/wire-paths.js';
import type { Segment } from './wire/wire-paths.js';
//=============================================================================
function countHops(d: string): number {
  return (d.match(/a \d/g) || []).length;
}
//=============================================================================
const connA = { from: 'a', to: 'b', fromIndex: 0, toIndex: 0 };
const connB = { from: 'z', to: 'w', fromIndex: 0, toIndex: 0 };
// Horizontal wire (owner a-prefix) crossing vertical wire (owner z-prefix) at (100, 50).
const hPoints = [
  { x: 0, y: 50 },
  { x: 200, y: 50 },
];
//=============================================================================
const vPoints = [
  { x: 100, y: 0 },
  { x: 100, y: 100 },
];
//=============================================================================
function segmentsOf(points: { x: number; y: number }[], owner: unknown): Segment[] {
  return buildSegments(points, owner);
}
//=============================================================================
describe('connOrderKey', () => {
  it('is content-based and ignores object identity', () => {
    expect(connOrderKey({ from: 'a', to: 'b' })).toBe('a:0->b:0');
    expect(connOrderKey({ from: 'a', to: 'b', fromIndex: 2, toIndex: 3 })).toBe('a:2->b:3');
    expect(connOrderKey({})).toBeUndefined();
  });
});
//=============================================================================
describe('deterministic hop ownership', () => {
  it('exactly one of two crossing wires draws the hop', () => {
    const segA = segmentsOf(hPoints, connA);
    const segB = segmentsOf(vPoints, connB);
    const all = [...segA, ...segB];
    const dA = buildPathWithHops(segA, all);
    const dB = buildPathWithHops(segB, all);
    // 'a:0->b:0' < 'z:0->w:0', so wire A owns the hop.
    expect(countHops(dA)).toBe(1);
    expect(countHops(dB)).toBe(0);
  });

  it('ownership follows key order, not draw order', () => {
    const segA = segmentsOf(hPoints, connB);
    const segB = segmentsOf(vPoints, connA);
    const all = [...segA, ...segB];
    expect(countHops(buildPathWithHops(segA, all))).toBe(0);
    expect(countHops(buildPathWithHops(segB, all))).toBe(1);
  });

  it('result is independent of segment array order', () => {
    const segA = segmentsOf(hPoints, connA);
    const segB = segmentsOf(vPoints, connB);
    const forward = [...segA, ...segB];
    const backward = [...segB, ...segA];
    expect(buildPathWithHops(segA, forward)).toBe(buildPathWithHops(segA, backward));
    expect(buildPathWithHops(segB, forward)).toBe(buildPathWithHops(segB, backward));
  });

  it('a wire never hops over itself', () => {
    // Self-crossing loop: same ownerKey on all segments.
    const loop = segmentsOf(
      [
        { x: 0, y: 50 },
        { x: 200, y: 50 },
        { x: 200, y: 100 },
        { x: 100, y: 100 },
        { x: 100, y: 0 },
      ],
      connA
    );
    const d = buildPathWithHops(loop, loop);
    expect(countHops(d)).toBe(0);
  });

  it('ownership is pairwise per crossing: coincident H wires each own their hop', () => {
    const connC = { from: 'm', to: 'n', fromIndex: 0, toIndex: 0 };
    const h1 = segmentsOf(hPoints, connA); // a:0->b:0 < z:0->w:0, owns
    const h2 = segmentsOf(
      [
        { x: 0, y: 50 },
        { x: 200, y: 50 },
      ],
      connC
    ); // m:0->n:0 < z:0->w:0, owns too (arcs coincide)
    const v1 = segmentsOf(vPoints, connB); // z:0->w:0, never owns here
    const all = [...h1, ...h2, ...v1];
    const d1 = buildPathWithHops(h1, all);
    const d2 = buildPathWithHops(h2, all);
    expect(countHops(d1)).toBe(1);
    expect(countHops(d2)).toBe(1);
    expect(d2).toBe(d1);
    expect(countHops(buildPathWithHops(v1, all))).toBe(0);
  });

  it('legacy callers without owner info keep the permissive behavior', () => {
    const segA = segmentsOf(hPoints, undefined);
    const segB = segmentsOf(vPoints, undefined);
    const d = buildPathWithHops(segA, segB);
    expect(countHops(d)).toBe(1);
  });

  it('unchanged geometry renders byte-identical paths across repeated calls', () => {
    const segA = segmentsOf(hPoints, connA);
    const segB = segmentsOf(vPoints, connB);
    const all = [...segA, ...segB];
    const first = buildPathWithHops(segA, all);
    for (let i = 0; i < 5; i += 1) {
      expect(buildPathWithHops(segA, all)).toBe(first);
    }
  });
});
//=============================================================================
