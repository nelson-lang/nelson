//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Physical nets are drawn as one trunk + perpendicular taps (a schematic bus),
// their branches are never spread apart by the separation pass, and every
// point where three or more branches meet gets a junction dot. The fixture is
// an RC low-pass schematic.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { describe, it, expect } from 'vitest';
import {
  buildPhysicalNetTreePaths,
  collectPhysicalNets,
  collectPhysicalNetJunctionPoints,
  physicalNetKeys,
} from './physical-net';
import { nudgeConnections } from './nudge';
//=============================================================================
type P = { x: number; y: number };
// Unrotated blocks; a port is [type, index, localX, localY] on the block edge.
function mk(
  x: number,
  y: number,
  width: number,
  height: number,
  ports: Array<[string, number, number, number]>
) {
  return {
    x,
    y,
    width,
    height,
    rotation: 0,
    ports: ports.map(([type, index, px, py]) => ({ type, index, x: px, y: py })),
  };
}
//=============================================================================
const conn = (from: string, fi: number, to: string, ti: number, points: P[] = []) => ({
  from,
  to,
  fromIndex: fi,
  toIndex: ti,
  points,
});
//=============================================================================
// V1 (source) and Ground on the left, R1 series on the rail, C1 and VS (sensor,
// which also carries an 'out' at index 0) as vertical shunts, a toWorkspace sink.
function rcLowPass() {
  const blocks = new Map<string, any>([
    [
      'R1',
      mk(200, 210, 100, 80, [
        ['phys', 0, 0, 40],
        ['phys', 1, 100, 40],
      ]),
    ],
    [
      'C1',
      mk(350, 270, 60, 100, [
        ['phys', 0, 30, 0],
        ['phys', 1, 30, 100],
      ]),
    ],
    [
      'VS',
      mk(490, 270, 60, 100, [
        ['phys', 0, 30, 0],
        ['phys', 1, 30, 100],
        ['out', 0, 60, 50],
      ]),
    ],
    [
      'V1',
      mk(50, 270, 60, 100, [
        ['phys', 0, 30, 0],
        ['phys', 1, 30, 100],
      ]),
    ],
    ['G', mk(50, 400, 60, 40, [['phys', 0, 30, 0]])],
    ['tw', mk(650, 280, 60, 40, [['in', 0, 0, 20]])],
  ]);
  const hot = [conn('R1', 1, 'C1', 0), conn('C1', 0, 'VS', 0)];
  const ref = [conn('V1', 1, 'C1', 1), conn('C1', 1, 'VS', 1), conn('VS', 1, 'G', 0)];
  const src = [conn('V1', 0, 'R1', 0)];
  const signal = [
    conn('VS', 0, 'tw', 0, [
      { x: 550, y: 320 },
      { x: 650, y: 300 },
    ]),
  ];
  return { blocks, hot, ref, src, signal, all: [...hot, ...ref, ...src, ...signal] };
}
//=============================================================================
const segH = (pts: P[]) =>
  pts
    .slice(1)
    .map((b, i) => ({ a: pts[i], b }))
    .filter((s) => s.a.y === s.b.y && s.a.x !== s.b.x);
describe('collectPhysicalNets / physicalNetKeys', () => {
  it('groups pin-to-pin wires into three nets and ignores the signal wire', () => {
    const f = rcLowPass();
    const nets = collectPhysicalNets(f.all, f.blocks);
    expect(nets.length).toBe(3);
    const keys = physicalNetKeys(f.all, f.blocks);
    expect(keys.get(f.hot[0])).toBe(keys.get(f.hot[1]));
    expect(keys.get(f.ref[0])).toBe(keys.get(f.ref[2]));
    expect(keys.get(f.hot[0])).not.toBe(keys.get(f.ref[0]));
    expect(keys.has(f.signal[0])).toBe(false); // VS 'out' at index 0 is a signal port
  });
});
//=============================================================================
describe('buildPhysicalNetTreePaths', () => {
  it('draws the hot node as one trunk at the series pin level with taps up from the shunts', () => {
    const f = rcLowPass();
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    const e1 = paths.get(f.hot[0]) as P[];
    const e2 = paths.get(f.hot[1]) as P[];
    expect(e1).toEqual([
      { x: 300, y: 250 },
      { x: 380, y: 250 },
      { x: 380, y: 270 },
    ]);
    expect(e2).toEqual([
      { x: 380, y: 270 },
      { x: 380, y: 250 },
      { x: 520, y: 250 },
      { x: 520, y: 270 },
    ]);
    expect(segH(e1).every((s) => s.a.y === 250)).toBe(true);
    expect(segH(e2).every((s) => s.a.y === 250)).toBe(true);
  });

  it('draws the 4-pin reference node between its down-facing and up-facing pins, all taps straight', () => {
    const f = rcLowPass();
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    const trunkY = 390;
    for (const c of f.ref) {
      const pts = paths.get(c) as P[];
      expect(pts).toBeDefined();
      expect(segH(pts).length).toBe(1);
      expect(segH(pts)[0].a.y).toBe(trunkY);
      for (let i = 0; i + 1 < pts.length; i += 1) {
        const a = pts[i];
        const b = pts[i + 1];
        expect(a.x === b.x || a.y === b.y).toBe(true);
      }
    }
    expect(paths.get(f.ref[2])).toEqual([
      { x: 520, y: 370 },
      { x: 520, y: trunkY },
      { x: 80, y: trunkY },
      { x: 80, y: 400 },
    ]);
  });

  it('draws the source-to-series wire as a single clean L', () => {
    const f = rcLowPass();
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    expect(paths.get(f.src[0])).toEqual([
      { x: 80, y: 270 },
      { x: 80, y: 250 },
      { x: 200, y: 250 },
    ]);
  });

  it('snaps everything to the grid', () => {
    const f = rcLowPass();
    for (const pts of buildPhysicalNetTreePaths(f.all, f.blocks).values()) {
      for (const p of pts) {
        expect(p.x % 10).toBe(0);
        expect(p.y % 10).toBe(0);
      }
    }
  });

  it('leaves a net alone when its wire carries a hand-pinned point', () => {
    const f = rcLowPass();
    f.hot[1].points = [
      { x: 380, y: 270 },
      { x: 380, y: 200, pin: true } as any,
      { x: 520, y: 270 },
    ];
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    expect(paths.has(f.hot[0])).toBe(false);
    expect(paths.has(f.hot[1])).toBe(false);
    expect(paths.has(f.ref[0])).toBe(true);
  });

  it("lifts the bus clear of a foreign block sitting on the pins' own line", () => {
    const f = rcLowPass();
    f.blocks.set('X', mk(320, 230, 30, 40, [])); // sits on the hot trunk between R1 and C1
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    // The trunk moves just above the crossed blocks (y=220); the series pin
    // reaches it with an L.
    expect(paths.get(f.hot[0])).toEqual([
      { x: 300, y: 250 },
      { x: 310, y: 250 },
      { x: 310, y: 220 },
      { x: 380, y: 220 },
      { x: 380, y: 270 },
    ]);
    expect(paths.get(f.hot[1])).toEqual([
      { x: 380, y: 270 },
      { x: 380, y: 220 },
      { x: 520, y: 220 },
      { x: 520, y: 270 },
    ]);
    expect(paths.has(f.ref[0])).toBe(true);
  });
});
//=============================================================================
describe('collectPhysicalNetJunctionPoints', () => {
  it('marks every point where three or more branches of a net meet', () => {
    const f = rcLowPass();
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    for (const c of f.all) {
      const p = paths.get(c);
      if (p) c.points = p;
    }
    const dots = collectPhysicalNetJunctionPoints(f.all, f.blocks).map((p) => `${p.x},${p.y}`);
    expect(dots.sort()).toEqual(['380,250', '380,390', '80,390'].sort());
  });
});
//=============================================================================
describe('nudgeConnections with net keys', () => {
  it('never spreads the branches of one physical net apart', () => {
    const f = rcLowPass();
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    for (const c of f.all) {
      const p = paths.get(c);
      if (p) c.points = p.map((q) => ({ ...q }));
    }
    const before = f.all.map((c) => JSON.stringify(c.points));
    const moved = nudgeConnections(f.all as any, f.blocks.values() as any, {
      netKeys: physicalNetKeys(f.all, f.blocks),
    });
    expect(moved).toBe(0);
    expect(f.all.map((c) => JSON.stringify(c.points))).toEqual(before);
  });

  it('would have spread them without net keys (the historical defect)', () => {
    const f = rcLowPass();
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    for (const c of f.all) {
      const p = paths.get(c);
      if (p) c.points = p.map((q) => ({ ...q }));
    }
    const moved = nudgeConnections(f.all as any, f.blocks.values() as any);
    expect(moved).toBeGreaterThan(0);
  });
});
//=============================================================================
describe('a node spread along a row of components', () => {
  // C1.port, HF.a and TB.port are ONE node, but a trunk on the pins' own line
  // would run through HF and K1, so the bus goes just above the row with an L
  // tap from every pin. HF and TB are sensors: a 'phys' pin AND a signal 'out'
  // share index 0, and the second branch STARTS at HF.
  function thermalRow() {
    const blocks = new Map<string, any>([
      ['C1', mk(80, 200, 100, 60, [['phys', 0, 100, 30]])],
      [
        'HF',
        mk(220, 200, 100, 60, [
          ['phys', 0, 0, 30],
          ['phys', 1, 100, 30],
          ['out', 0, 100, 10],
        ]),
      ],
      [
        'K1',
        mk(360, 200, 100, 60, [
          ['phys', 0, 0, 30],
          ['phys', 1, 100, 30],
        ]),
      ],
      [
        'TB',
        mk(640, 200, 100, 60, [
          ['phys', 0, 0, 30],
          ['out', 0, 100, 30],
        ]),
      ],
    ]);
    const node = [conn('C1', 0, 'HF', 0), conn('HF', 0, 'TB', 0)];
    return { blocks, node, all: [...node, conn('HF', 1, 'K1', 0)] };
  }

  it('keeps a branch that starts at a sensor pin in the node', () => {
    const f = thermalRow();
    const nets = collectPhysicalNets(f.all, f.blocks);
    expect(nets.length).toBe(2); // the 3-pin node + the HF.b/K1.a pair
    expect(nets.some((n) => n.conns.length === 2)).toBe(true);
  });

  it('lifts the trunk clear above the row and taps every pin with an L', () => {
    const f = thermalRow();
    const paths = buildPhysicalNetTreePaths(f.all, f.blocks);
    const p1 = paths.get(f.node[0]) as P[];
    const p2 = paths.get(f.node[1]) as P[];
    expect(p1).toEqual([
      { x: 180, y: 230 },
      { x: 190, y: 230 },
      { x: 190, y: 190 },
      { x: 210, y: 190 },
      { x: 210, y: 230 },
      { x: 220, y: 230 },
    ]);
    expect(p2).toEqual([
      { x: 220, y: 230 },
      { x: 210, y: 230 },
      { x: 210, y: 190 },
      { x: 630, y: 190 },
      { x: 630, y: 230 },
      { x: 640, y: 230 },
    ]);
    expect(paths.get(f.all[2])).toEqual([
      { x: 320, y: 230 },
      { x: 360, y: 230 },
    ]);
  });
});
//=============================================================================
describe('several nets over one row of components', () => {
  // Net 1 joins A and C, net 2 joins B and D: both want the bus just above the
  // row, but must land on DISTINCT tracks, not on one line for the separation
  // pass to split by a single cell.
  it('stacks their clearance buses on distinct tracks', () => {
    const blocks = new Map<string, any>([
      ['A', mk(80, 200, 100, 60, [['phys', 0, 0, 30]])],
      ['B', mk(220, 200, 100, 60, [['phys', 0, 0, 30]])],
      ['C', mk(360, 200, 100, 60, [['phys', 0, 0, 30]])],
      ['D', mk(500, 200, 100, 60, [['phys', 0, 0, 30]])],
    ]);
    const n1 = conn('A', 0, 'C', 0);
    const n2 = conn('B', 0, 'D', 0);
    const paths = buildPhysicalNetTreePaths([n1, n2], blocks);
    // the trunk is the longest horizontal run; the short ones are L stubs
    const trunkY = (pts: P[]) =>
      segH(pts).reduce((m, s) => (Math.abs(s.b.x - s.a.x) > Math.abs(m.b.x - m.a.x) ? s : m)).a.y;
    expect(trunkY(paths.get(n1) as P[])).toBe(190); // just above the row
    expect(trunkY(paths.get(n2) as P[])).toBe(170); // stacked one bus step further out
  });
});
//=============================================================================
