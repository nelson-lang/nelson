//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit tests for the minimum-disturbance local repair used during drags.
//=============================================================================
import {
  translatePath,
  repairEndpointMove,
  repairPath,
  dedupeRepairPoints,
  resolveConnPorts,
  repairConnForDrag,
} from './wire-local-repair.js';
import type { RepairBaseline, RepairPoint } from './wire-local-repair.js';
//=============================================================================
const GRID = 10;
function pts(...coords: [number, number][]): RepairPoint[] {
  return coords.map(([x, y]) => ({ x, y }));
}
//=============================================================================
function coords(points: RepairPoint[]): [number, number][] {
  return points.map((p) => [p.x, p.y]);
}
//=============================================================================
function isOrthogonal(points: RepairPoint[]): boolean {
  for (let i = 1; i < points.length; i += 1) {
    const dx = points[i].x - points[i - 1].x;
    const dy = points[i].y - points[i - 1].y;
    if (dx !== 0 && dy !== 0) return false;
  }
  return true;
}
//=============================================================================
describe('translatePath', () => {
  it('rigidly shifts every point', () => {
    const base = pts([0, 0], [50, 0], [50, 40], [100, 40]);
    expect(coords(translatePath(base, 20, -10))).toEqual([
      [20, -10],
      [70, -10],
      [70, 30],
      [120, 30],
    ]);
  });

  it('preserves pin flags', () => {
    const base: RepairPoint[] = [
      { x: 0, y: 0 },
      { x: 50, y: 0, pin: true },
      { x: 50, y: 40 },
    ];
    const out = translatePath(base, 10, 10);
    expect(out[1].pin).toBe(true);
    expect(out[0].pin).toBeUndefined();
  });

  it('does not mutate the input', () => {
    const base = pts([0, 0], [50, 0]);
    translatePath(base, 5, 5);
    expect(coords(base)).toEqual([
      [0, 0],
      [50, 0],
    ]);
  });
});
//=============================================================================
describe('repairEndpointMove: from end, H-first path', () => {
  // Baseline: right-side port at (0,0), H segment to (50,0), V to (50,40), H to (100,40).
  const base = pts([0, 0], [50, 0], [50, 40], [100, 40]);

  it('vertical port move translates the terminal segment laterally, middle frozen', () => {
    const out = repairEndpointMove(base, 'from', { x: 0, y: 20 }, 'right', GRID);
    expect(coords(out)).toEqual([
      [0, 20],
      [50, 20],
      [50, 40],
      [100, 40],
    ]);
    expect(isOrthogonal(out)).toBe(true);
  });

  it('horizontal move stretches the terminal segment, middle frozen', () => {
    const out = repairEndpointMove(base, 'from', { x: -30, y: 0 }, 'right', GRID);
    expect(coords(out)).toEqual([
      [-30, 0],
      [50, 0],
      [50, 40],
      [100, 40],
    ]);
  });

  it('diagonal move combines lateral translate + stretch', () => {
    const out = repairEndpointMove(base, 'from', { x: -10, y: 30 }, 'right', GRID);
    expect(coords(out)).toEqual([
      [-10, 30],
      [50, 30],
      [50, 40],
      [100, 40],
    ]);
    expect(isOrthogonal(out)).toBe(true);
  });

  it('collapse below minStub inserts a fresh stub + orthogonal jog', () => {
    // Port dragged just past the first bend: terminal segment would reverse.
    const out = repairEndpointMove(base, 'from', { x: 55, y: 0 }, 'right', GRID);
    expect(out[0]).toMatchObject({ x: 55, y: 0 });
    expect(out[1]).toMatchObject({ x: 65, y: 0 });
    expect(isOrthogonal(out)).toBe(true);
    expect(coords(out).slice(-2)).toEqual([
      [50, 40],
      [100, 40],
    ]);
  });

  it('no-op move returns the same shape', () => {
    const out = repairEndpointMove(base, 'from', { x: 0, y: 0 }, 'right', GRID);
    expect(coords(out)).toEqual(coords(base));
  });
});
//=============================================================================
describe('repairEndpointMove: to end and V-first paths', () => {
  it('repairs the to end symmetrically (frozen head)', () => {
    const base = pts([0, 0], [50, 0], [50, 40], [100, 40]);
    const out = repairEndpointMove(base, 'to', { x: 100, y: 60 }, 'left', GRID);
    expect(coords(out)).toEqual([
      [0, 0],
      [50, 0],
      [50, 60],
      [100, 60],
    ]);
  });

  it('handles V-first terminal segments (bottom port)', () => {
    // Port on bottom side at (0,0), V segment down to (0,30), H to (60,30).
    const base = pts([0, 0], [0, 30], [60, 30]);
    const out = repairEndpointMove(base, 'from', { x: 20, y: 0 }, 'bottom', GRID);
    expect(coords(out)).toEqual([
      [20, 0],
      [20, 30],
      [60, 30],
    ]);
    expect(isOrthogonal(out)).toBe(true);
  });

  it('two-point colinear path stays a straight segment', () => {
    const base = pts([0, 0], [80, 0]);
    const out = repairEndpointMove(base, 'from', { x: -20, y: 0 }, 'right', GRID);
    expect(coords(out)).toEqual([
      [-20, 0],
      [80, 0],
    ]);
  });

  it('two-point path losing colinearity becomes a compact S/Z', () => {
    const base = pts([0, 0], [80, 0]);
    const out = repairEndpointMove(base, 'from', { x: 0, y: 30 }, 'right', GRID);
    expect(out[0]).toMatchObject({ x: 0, y: 30 });
    expect(out[out.length - 1]).toMatchObject({ x: 80, y: 0 });
    expect(isOrthogonal(out)).toBe(true);
    expect(out.length).toBeGreaterThanOrEqual(3);
  });
});
//=============================================================================
describe('repairPath', () => {
  const baseline: RepairBaseline = {
    points: pts([0, 0], [50, 0], [50, 40], [100, 40]),
    fromPos: { x: 0, y: 0 },
    toPos: { x: 100, y: 40 },
  };

  it('identical deltas → rigid translation (multi-select drag)', () => {
    const out = repairPath(
      baseline,
      { pos: { x: 10, y: 20 }, side: 'right' },
      { pos: { x: 110, y: 60 }, side: 'left' }
    );
    expect(coords(out)).toEqual([
      [10, 20],
      [60, 20],
      [60, 60],
      [110, 60],
    ]);
  });

  it('zero deltas → clone of the baseline', () => {
    const out = repairPath(
      baseline,
      { pos: { x: 0, y: 0 }, side: 'right' },
      { pos: { x: 100, y: 40 }, side: 'left' }
    );
    expect(coords(out)).toEqual(coords(baseline.points));
    expect(out[0]).not.toBe(baseline.points[0]);
  });

  it('different deltas repair each end independently, middle frozen', () => {
    const out = repairPath(
      baseline,
      { pos: { x: 0, y: 10 }, side: 'right' },
      { pos: { x: 100, y: 60 }, side: 'left' }
    );
    expect(coords(out)).toEqual([
      [0, 10],
      [50, 10],
      [50, 60],
      [100, 60],
    ]);
  });

  it('is idempotent: repairing from the same baseline twice gives the same result', () => {
    const fromNow = { pos: { x: -20, y: 30 }, side: 'right' } as const;
    const toNow = { pos: { x: 100, y: 40 }, side: 'left' } as const;
    const a = repairPath(baseline, fromNow, toNow);
    const b = repairPath(baseline, fromNow, toNow);
    expect(coords(a)).toEqual(coords(b));
  });
});
//=============================================================================
describe('dedupeRepairPoints', () => {
  it('drops consecutive duplicates and merges pin flags', () => {
    const out = dedupeRepairPoints([
      { x: 0, y: 0 },
      { x: 0, y: 0, pin: true },
      { x: 10, y: 0 },
    ]);
    expect(coords(out)).toEqual([
      [0, 0],
      [10, 0],
    ]);
    expect(out[0].pin).toBe(true);
  });
});
//=============================================================================
describe('resolveConnPorts / repairConnForDrag', () => {
  function makeState(blocks: any[]) {
    return { blocks: new Map(blocks.map((b) => [b.id, b])) } as any;
  }

  const blockA = {
    id: 'a',
    x: 100,
    y: 100,
    width: 80,
    height: 40,
    rotation: 0,
    ports: [
      { type: 'in', index: 0, x: 0, y: 20 },
      { type: 'out', index: 0, x: 80, y: 20 },
    ],
  };
  const blockB = {
    id: 'b',
    x: 300,
    y: 100,
    width: 80,
    height: 40,
    rotation: 0,
    ports: [{ type: 'in', index: 0, x: 0, y: 20 }],
  };

  it('resolves ports by (type, index), not array position', () => {
    const state = makeState([blockA, blockB]);
    const ports = resolveConnPorts(state, { from: 'a', to: 'b', fromIndex: 0, toIndex: 0 });
    expect(ports).not.toBeNull();
    // out port of A is at x+80 even though ports[0] is the in port
    expect(ports!.from.pos).toEqual({ x: 180, y: 120 });
    expect(ports!.from.side).toBe('right');
    expect(ports!.to.pos).toEqual({ x: 300, y: 120 });
    expect(ports!.to.side).toBe('left');
  });

  it('captures a baseline on first call and repairs from it afterwards', () => {
    const a = { ...blockA };
    const state = makeState([a, blockB]);
    const conn: any = {
      from: 'a',
      to: 'b',
      fromIndex: 0,
      toIndex: 0,
      points: pts([180, 120], [240, 120], [240, 120], [300, 120]),
    };
    const baselines = new WeakMap();

    const first = repairConnForDrag(state, conn, baselines);
    expect(first).not.toBeNull();

    a.y = 130;
    state.blocks.set('a', a);
    const repaired = repairConnForDrag(state, conn, baselines)!;
    expect(repaired[0]).toMatchObject({ x: 180, y: 150 });
    expect(coords(repaired).slice(-1)).toEqual([[300, 120]]);
    expect(isOrthogonal(repaired)).toBe(true);

    // Repairing again from the same position gives the same result (no drift).
    const again = repairConnForDrag(state, conn, baselines)!;
    expect(coords(again)).toEqual(coords(repaired));
  });

  it('returns null when the connection has no valid baseline points', () => {
    const state = makeState([blockA, blockB]);
    const conn: any = { from: 'a', to: 'b', fromIndex: 0, toIndex: 0, points: [] };
    expect(repairConnForDrag(state, conn, new WeakMap())).toBeNull();
  });
});
//=============================================================================
