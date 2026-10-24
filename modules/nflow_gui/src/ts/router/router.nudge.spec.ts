//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Nudging / separation pass: overlapping collinear segments of different
// nets are spread one grid cell apart; every move is validated and quality
// can only improve.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { nudgeConnections } from './nudge.js';
import { analyzeConnectionGeometry } from './post-process.js';
//=============================================================================
type Point = { x: number; y: number };
function conn(from: string, to: string, points: Point[], fromIndex = 0, toIndex = 0) {
  return { from, to, fromIndex, toIndex, points: points.map((p) => ({ ...p })) };
}
//=============================================================================
// Two nets forced through the same horizontal channel at y=200.
function overlappingPair() {
  return [
    conn('p', 'u', [
      { x: 100, y: 100 },
      { x: 110, y: 100 },
      { x: 110, y: 200 },
      { x: 400, y: 200 },
      { x: 400, y: 120 },
      { x: 410, y: 120 },
    ]),
    conn('q', 'v', [
      { x: 100, y: 300 },
      { x: 110, y: 300 },
      { x: 110, y: 200 },
      { x: 400, y: 200 },
      { x: 400, y: 320 },
      { x: 410, y: 320 },
    ]),
  ];
}
//=============================================================================
function overlapsOf(conns: any[]) {
  return analyzeConnectionGeometry(conns, { ignoreSharedPorts: true }).totals.overlaps;
}
//=============================================================================
describe('nudgeConnections', () => {
  it('separates two nets sharing an exact channel (overlaps → 0)', () => {
    const conns = overlappingPair();
    expect(overlapsOf(conns)).toBeGreaterThan(0);
    const moved = nudgeConnections(conns as any, []);
    expect(moved).toBeGreaterThan(0);
    expect(overlapsOf(conns)).toBe(0);
    // Offsets are whole grid cells.
    for (const c of conns) {
      for (const p of c.points) {
        expect(p.x % 10).toBe(0);
        expect(p.y % 10).toBe(0);
      }
    }
  });

  it('keeps endpoints and stub directions intact', () => {
    const conns = overlappingPair();
    const ends = conns.map((c) => [{ ...c.points[0] }, { ...c.points[c.points.length - 1] }]);
    nudgeConnections(conns as any, []);
    conns.forEach((c, i) => {
      expect(c.points[0]).toEqual(ends[i][0]);
      expect(c.points[c.points.length - 1]).toEqual(ends[i][1]);
      // First and last segments keep their directions.
      const first = c.points[1].x - c.points[0].x;
      expect(first).toBeGreaterThan(0);
    });
  });

  it('is idempotent: a second pass changes nothing', () => {
    const conns = overlappingPair();
    nudgeConnections(conns as any, []);
    const after = JSON.stringify(conns.map((c) => c.points));
    const movedAgain = nudgeConnections(conns as any, []);
    expect(movedAgain).toBe(0);
    expect(JSON.stringify(conns.map((c) => c.points))).toBe(after);
  });

  it('is deterministic and independent of connection order', () => {
    const a = overlappingPair();
    nudgeConnections(a as any, []);
    const b = overlappingPair().reverse();
    nudgeConnections(b as any, []);
    const byFrom = (list: any[]) =>
      Object.fromEntries(list.map((c) => [c.from, JSON.stringify(c.points)]));
    expect(byFrom(a)).toEqual(byFrom(b));
  });

  it('shared-source siblings are not separated (their prefix overlaps by design)', () => {
    const shared = [
      conn('s', 'a', [
        { x: 100, y: 200 },
        { x: 110, y: 200 },
        { x: 300, y: 200 },
        { x: 300, y: 100 },
        { x: 400, y: 100 },
      ]),
      conn('s', 'b', [
        { x: 100, y: 200 },
        { x: 110, y: 200 },
        { x: 300, y: 200 },
        { x: 300, y: 300 },
        { x: 400, y: 300 },
      ]),
    ];
    const before = JSON.stringify(shared.map((c) => c.points));
    const moved = nudgeConnections(shared as any, []);
    expect(moved).toBe(0);
    expect(JSON.stringify(shared.map((c) => c.points))).toBe(before);
  });

  it('reverts a move that would cross a block', () => {
    const conns = overlappingPair();
    // Blocks walling the channel from both sides: any ±10 shift collides.
    const blocks = [
      { id: 'wallUp', x: 120, y: 150, width: 260, height: 44, rotation: 0, ports: [] },
      { id: 'wallDown', x: 120, y: 206, width: 260, height: 44, rotation: 0, ports: [] },
    ];
    const before = JSON.stringify(conns.map((c) => c.points));
    nudgeConnections(conns as any, blocks as any);
    // Overlap remains (no valid separation exists) but nothing got worse.
    expect(JSON.stringify(conns.map((c) => c.points))).toBe(before);
  });

  it('never worsens quality on already-clean layouts', () => {
    const clean = [
      conn('a', 'b', [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
        { x: 50, y: 0 },
        { x: 50, y: 50 },
        { x: 100, y: 50 },
      ]),
      conn('c', 'd', [
        { x: 0, y: 200 },
        { x: 10, y: 200 },
        { x: 60, y: 200 },
        { x: 60, y: 260 },
        { x: 100, y: 260 },
      ]),
    ];
    const before = JSON.stringify(clean.map((c) => c.points));
    const moved = nudgeConnections(clean as any, []);
    expect(moved).toBe(0);
    expect(JSON.stringify(clean.map((c) => c.points))).toBe(before);
  });

  it('spreads three overlapping nets onto three distinct tracks', () => {
    const three = [
      conn('p', 'u', [
        { x: 100, y: 100 },
        { x: 110, y: 100 },
        { x: 110, y: 200 },
        { x: 400, y: 200 },
        { x: 400, y: 100 },
        { x: 410, y: 100 },
      ]),
      conn('q', 'v', [
        { x: 100, y: 300 },
        { x: 110, y: 300 },
        { x: 110, y: 200 },
        { x: 400, y: 200 },
        { x: 400, y: 300 },
        { x: 410, y: 300 },
      ]),
      conn('r', 'w', [
        { x: 100, y: 500 },
        { x: 110, y: 500 },
        { x: 110, y: 200 },
        { x: 400, y: 200 },
        { x: 400, y: 500 },
        { x: 410, y: 500 },
      ]),
    ];
    nudgeConnections(three as any, []);
    const tracks = three.map((c) => c.points[2].y);
    expect(new Set(tracks).size).toBe(3);
    expect(overlapsOf(three)).toBe(0);
  });
});
//=============================================================================
describe('lone-segment centering', () => {
  it('re-centers a block-hugging segment in its free channel', () => {
    // Interior H run at y=200 hugging a block whose bottom edge is at y=194: clearance is
    // 6 px, under one grid cell. There is room below, so the run is pushed toward the
    // channel middle.
    const conns = [
      conn('a', 'b', [
        { x: 100, y: 200 },
        { x: 110, y: 200 },
        { x: 400, y: 200 },
        { x: 400, y: 260 },
        { x: 410, y: 260 },
      ]),
      conn('c', 'd', [
        { x: 100, y: 400 },
        { x: 110, y: 400 },
        { x: 400, y: 400 },
        { x: 410, y: 400 },
      ]),
    ];
    const blocks = [
      { id: 'hugged', x: 150, y: 146, width: 200, height: 48, rotation: 0, ports: [] },
    ];
    const moved = nudgeConnections(conns as any, blocks as any);
    expect(moved).toBeGreaterThan(0);
    // The long run moved down to y=210 (guard bend inserted at the stub);
    // grid-aligned, clear of the block.
    const runYs = conns[0].points.map((p: any) => p.y);
    expect(runYs).toContain(210);
    expect(runYs.every((y: number) => y % 10 === 0)).toBe(true);
    // Endpoints untouched.
    expect(conns[0].points[0]).toEqual({ x: 100, y: 200 });
    expect(conns[0].points[conns[0].points.length - 1]).toEqual({ x: 410, y: 260 });
  });

  it('leaves clear segments alone and is idempotent', () => {
    const conns = [
      conn('a', 'b', [
        { x: 100, y: 200 },
        { x: 110, y: 200 },
        { x: 400, y: 200 },
        { x: 410, y: 200 },
      ]),
    ];
    const blocks = [{ id: 'far', x: 150, y: 20, width: 200, height: 40, rotation: 0, ports: [] }];
    const before = JSON.stringify(conns[0].points);
    expect(nudgeConnections(conns as any, blocks as any)).toBe(0);
    expect(JSON.stringify(conns[0].points)).toBe(before);
  });

  it('centering converges (second pass is a no-op)', () => {
    const conns = [
      conn('a', 'b', [
        { x: 100, y: 200 },
        { x: 110, y: 200 },
        { x: 400, y: 200 },
        { x: 400, y: 260 },
        { x: 410, y: 260 },
      ]),
      conn('c', 'd', [
        { x: 100, y: 400 },
        { x: 110, y: 400 },
        { x: 400, y: 400 },
        { x: 410, y: 400 },
      ]),
    ];
    const blocks = [
      { id: 'hugged', x: 150, y: 146, width: 200, height: 48, rotation: 0, ports: [] },
    ];
    nudgeConnections(conns as any, blocks as any);
    const after = JSON.stringify(conns.map((c) => c.points));
    expect(nudgeConnections(conns as any, blocks as any)).toBe(0);
    expect(JSON.stringify(conns.map((c) => c.points))).toBe(after);
  });
});
//=============================================================================
