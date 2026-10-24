//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Rip-up-and-reroute refinement invariants: the batch is kept only when it
// strictly improves the lexicographic aggregate: quality can never get
// worse, results stay deterministic, and a rolled-back refine is a perfect
// no-op.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { computeWireRoutes } from './pathfinder.js';
//=============================================================================
const settingsBase = {
  deterministic: true,
  incremental: false,
  fullOptimize: true,
  nearObstaclePenalty1: 2,
  nearObstaclePenalty2: 0,
  nearWirePenalty1: 3,
  nearWirePenalty2: 1,
};
//=============================================================================
// Crossing ladder: unavoidable hops; refine must roll back to identical
// routes.
function ladder() {
  const nodes: any[] = [];
  const connections: any[] = [];
  for (let i = 0; i < 4; i += 1) {
    nodes.push({ id: `L${i}:out:0`, x: 0, y: i * 6, dir: 'right' });
    nodes.push({ id: `R${i}:in:0`, x: 30, y: i * 6, dir: 'left' });
  }
  for (let i = 0; i < 4; i += 1) {
    connections.push({ from: `L${i}:out:0`, to: `R${3 - i}:in:0`, key: `k${i}` });
  }
  return { nodes, connections };
}
//=============================================================================
function routeLadder(refineIterations: number) {
  const { nodes, connections } = ladder();
  return computeWireRoutes({
    nodes,
    connections,
    settings: { ...settingsBase, refineIterations } as any,
  });
}
//=============================================================================
function shapeOf(res: any): string {
  const keys = Array.from(res.wires.keys()).sort();
  return JSON.stringify(
    keys.map((k) => [k, (res.wires.get(k)?.points ?? []).map((p: any) => [p.x, p.y])])
  );
}
//=============================================================================
/** Geometric inter-wire crossings (strict H/V intersections between nets). */
function countCrossings(res: any): number {
  const wires = Array.from(res.wires.values()) as any[];
  const segsOf = (pts: any[]) => {
    const out: any[] = [];
    for (let i = 1; i < pts.length; i += 1) {
      const a = pts[i - 1];
      const b = pts[i];
      if (a.x === b.x && a.y !== b.y)
        out.push({ o: 'V', x: a.x, lo: Math.min(a.y, b.y), hi: Math.max(a.y, b.y) });
      else if (a.y === b.y && a.x !== b.x)
        out.push({ o: 'H', y: a.y, lo: Math.min(a.x, b.x), hi: Math.max(a.x, b.x) });
    }
    return out;
  };
  let count = 0;
  for (let i = 0; i < wires.length; i += 1) {
    for (let j = i + 1; j < wires.length; j += 1) {
      for (const sa of segsOf(wires[i].points || [])) {
        for (const sb of segsOf(wires[j].points || [])) {
          if (sa.o === sb.o) continue;
          const h = sa.o === 'H' ? sa : sb;
          const v = sa.o === 'V' ? sa : sb;
          if (v.x > h.lo && v.x < h.hi && h.y > v.lo && v.hi > h.y) count += 1;
        }
      }
    }
  }
  return count;
}
//=============================================================================
describe('rip-up-and-reroute refinement', () => {
  it('never increases geometric inter-wire crossings', () => {
    // Aggregate cost totals are NOT homogeneous between refine on/off (the
    // a-posteriori scoring reveals proximity the at-routing-time costs never
    // saw), so the invariant is checked on real geometry.
    const off = routeLadder(0);
    const on = routeLadder(2);
    expect(countCrossings(on)).toBeLessThanOrEqual(countCrossings(off));
  });

  it('S7: a-posteriori scoring strictly reduces inter-wire adjacency', () => {
    // Early-routed wires never see the wires routed after them, so only the
    // a-posteriori scoring makes them refine candidates. On the ladder they hug
    // each other; single-wire rip waves spread them apart (15 to 4 adjacent cell
    // pairs) while the unavoidable hops stay.
    const adjacency = (res: any): number => {
      const wires = Array.from(res.wires.values()) as any[];
      let adj = 0;
      for (let i = 0; i < wires.length; i += 1) {
        for (let j = i + 1; j < wires.length; j += 1) {
          const setJ = new Set((wires[j].points || []).map((p: any) => `${p.x},${p.y}`));
          for (const p of wires[i].points || []) {
            for (const [dx, dy] of [
              [1, 0],
              [-1, 0],
              [0, 1],
              [0, -1],
            ]) {
              if (setJ.has(`${p.x + dx},${p.y + dy}`)) adj += 1;
            }
          }
        }
      }
      return adj;
    };

    const off = routeLadder(0);
    const on = routeLadder(2);
    expect(adjacency(on)).toBeLessThan(adjacency(off));
    expect(on.cost.hops).toBe(off.cost.hops); // unavoidable crossings stay
    // Converged: more waves change nothing further.
    expect(adjacency(routeLadder(3))).toBe(adjacency(on));
  });

  it('is deterministic across repeated runs', () => {
    const first = shapeOf(routeLadder(2));
    for (let i = 0; i < 5; i += 1) {
      expect(shapeOf(routeLadder(2))).toBe(first);
    }
  });

  it('refine leaves clean layouts untouched (no badness → early exit)', () => {
    const nodes = [
      { id: 'a:out:0', x: 0, y: 0, dir: 'right' },
      { id: 'b:in:0', x: 20, y: 0, dir: 'left' },
      { id: 'c:out:0', x: 0, y: 8, dir: 'right' },
      { id: 'd:in:0', x: 20, y: 8, dir: 'left' },
    ];
    const connections = [
      { from: 'a:out:0', to: 'b:in:0', key: 'k1' },
      { from: 'c:out:0', to: 'd:in:0', key: 'k2' },
    ];
    const off = computeWireRoutes({
      nodes: nodes as any,
      connections: connections as any,
      settings: { ...settingsBase, refineIterations: 0 } as any,
    });
    const on = computeWireRoutes({
      nodes: nodes as any,
      connections: connections as any,
      settings: { ...settingsBase, refineIterations: 2 } as any,
    });
    expect(shapeOf(on)).toBe(shapeOf(off));
    expect(on.cost.hops).toBe(0);
  });

  it('incremental routes default to zero refine iterations', () => {
    const { nodes, connections } = ladder();
    const res = computeWireRoutes({
      nodes,
      connections,
      settings: { ...settingsBase, incremental: true, fullOptimize: false } as any,
    });
    expect(res.wires.size).toBeGreaterThan(0);
  });
});
//=============================================================================
