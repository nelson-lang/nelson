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
import { computeAutoLayout } from './auto-layout.js';
import type { LayoutNode, LayoutEdge } from './auto-layout.js';
//=============================================================================
function node(id: string, width = 80, height = 60): LayoutNode {
  return { id, width, height };
}
//=============================================================================
function centreX(pos: { x: number; y: number }, n: LayoutNode) {
  return pos.x + n.width / 2;
}
//=============================================================================
function centreY(pos: { x: number; y: number }, n: LayoutNode) {
  return pos.y + n.height / 2;
}
//=============================================================================
describe('computeAutoLayout', () => {
  it('lays a simple chain left-to-right in strictly increasing columns', () => {
    const nodes = [node('a'), node('b'), node('c')];
    const edges: LayoutEdge[] = [
      { from: 'a', to: 'b' },
      { from: 'b', to: 'c' },
    ];
    const pos = computeAutoLayout(nodes, edges);
    expect(pos.get('a')!.x).toBeLessThan(pos.get('b')!.x);
    expect(pos.get('b')!.x).toBeLessThan(pos.get('c')!.x);
  });

  it('keeps a straight chain vertically aligned (edges are straight)', () => {
    const nodes = [node('a'), node('b'), node('c')];
    const edges: LayoutEdge[] = [
      { from: 'a', to: 'b' },
      { from: 'b', to: 'c' },
    ];
    const pos = computeAutoLayout(nodes, edges);
    const cy = (id: string) => centreY(pos.get(id)!, node(id));
    expect(cy('a')).toBeCloseTo(cy('b'), 5);
    expect(cy('b')).toBeCloseTo(cy('c'), 5);
  });

  it('places a fan-out source left of all its targets', () => {
    const nodes = [node('src'), node('t1'), node('t2'), node('t3')];
    const edges: LayoutEdge[] = [
      { from: 'src', to: 't1' },
      { from: 'src', to: 't2' },
      { from: 'src', to: 't3' },
    ];
    const pos = computeAutoLayout(nodes, edges);
    for (const t of ['t1', 't2', 't3']) {
      expect(pos.get('src')!.x).toBeLessThan(pos.get(t)!.x);
    }
    expect(pos.get('t1')!.x).toBe(pos.get('t2')!.x);
    expect(pos.get('t2')!.x).toBe(pos.get('t3')!.x);
    const ys = ['t1', 't2', 't3'].map((t) => pos.get(t)!.y);
    expect(new Set(ys).size).toBe(3);
  });

  it('handles a feedback loop, reversing the true feedback edge', () => {
    // Classic control loop with a reference source:
    //   ref -> sum -> ctrl -> plant -> (feedback) sum, plant -> scope.
    // The step/reference is the only in-degree-0 node, so the loop must be
    // broken at plant -> sum and the forward chain stays left-to-right.
    const nodes = [node('ref'), node('sum'), node('ctrl'), node('plant'), node('scope')];
    const edges: LayoutEdge[] = [
      { from: 'ref', to: 'sum' },
      { from: 'sum', to: 'ctrl' },
      { from: 'ctrl', to: 'plant' },
      { from: 'plant', to: 'sum' }, // feedback (back edge)
      { from: 'plant', to: 'scope' },
    ];
    const pos = computeAutoLayout(nodes, edges);
    expect(pos.size).toBe(5);
    expect(pos.get('ref')!.x).toBeLessThan(pos.get('sum')!.x);
    expect(pos.get('sum')!.x).toBeLessThan(pos.get('ctrl')!.x);
    expect(pos.get('ctrl')!.x).toBeLessThan(pos.get('plant')!.x);
  });

  it('never overlaps two blocks in the same column', () => {
    const nodes = Array.from({ length: 6 }, (_, i) => node(`t${i}`));
    const edges: LayoutEdge[] = nodes.map((n) => ({ from: 'src', to: n.id }));
    nodes.unshift(node('src'));
    const pos = computeAutoLayout(nodes, edges);
    const byCol = new Map<number, Array<{ id: string; y0: number; y1: number }>>();
    for (const n of nodes) {
      const p = pos.get(n.id)!;
      const list = byCol.get(p.x) || [];
      list.push({ id: n.id, y0: p.y, y1: p.y + n.height });
      byCol.set(p.x, list);
    }
    for (const list of byCol.values()) {
      list.sort((a, b) => a.y0 - b.y0);
      for (let i = 1; i < list.length; i += 1) {
        expect(list[i].y0).toBeGreaterThanOrEqual(list[i - 1].y1);
      }
    }
  });

  it('packs isolated blocks into a grid rather than a single tall column', () => {
    const nodes = Array.from({ length: 9 }, (_, i) => node(`iso${i}`));
    const pos = computeAutoLayout(nodes, []);
    const xs = new Set([...pos.values()].map((p) => p.x));
    const ys = new Set([...pos.values()].map((p) => p.y));
    // A 3x3 grid: several distinct columns and rows, not one column.
    expect(xs.size).toBeGreaterThan(1);
    expect(ys.size).toBeGreaterThan(1);
  });

  it('is deterministic (same input yields identical output)', () => {
    const nodes = [node('a'), node('b'), node('c'), node('d')];
    const edges: LayoutEdge[] = [
      { from: 'a', to: 'b' },
      { from: 'a', to: 'c' },
      { from: 'b', to: 'd' },
      { from: 'c', to: 'd' },
    ];
    const p1 = computeAutoLayout(nodes, edges);
    const p2 = computeAutoLayout(nodes, edges);
    for (const [id, p] of p1) {
      expect(p2.get(id)).toEqual(p);
    }
  });

  it('snaps the left edge and the vertical centre to the grid', () => {
    const nodes = [node('a', 80, 60), node('b', 100, 70)];
    const edges: LayoutEdge[] = [{ from: 'a', to: 'b' }];
    const pos = computeAutoLayout(nodes, edges, { gridSize: 10 });
    for (const n of nodes) {
      const p = pos.get(n.id)!;
      expect(p.x % 10).toBe(0); // left edge on the grid (columns)
      expect((p.y + n.height / 2) % 10).toBe(0); // vertical centre on the grid
    }
  });

  it('aligns the ports of a straight single-port chain even with odd heights', () => {
    // Two centred-port blocks of different height: their centres (hence their
    // centred ports) must land on the same grid line so the wire is straight.
    const nodes = [node('a', 80, 60), node('b', 80, 70)];
    const edges: LayoutEdge[] = [{ from: 'a', to: 'b' }];
    const pos = computeAutoLayout(nodes, edges);
    const ca = pos.get('a')!.y + 60 / 2;
    const cb = pos.get('b')!.y + 70 / 2;
    expect(ca).toBeCloseTo(cb, 5);
  });

  it('reduces crossings on a two-layer bipartite tangle', () => {
    // Edges deliberately cross if the natural order is kept: a->y, b->x.
    const nodes = [node('a'), node('b'), node('x'), node('y')];
    const edges: LayoutEdge[] = [
      { from: 'a', to: 'y' },
      { from: 'b', to: 'x' },
    ];
    const pos = computeAutoLayout(nodes, edges);
    // After ordering, a should be on the same side as y and b as x, so the
    // vertical order of {a,b} matches {y,x}: crossing removed.
    const aAboveB = pos.get('a')!.y < pos.get('b')!.y;
    const yAboveX = pos.get('y')!.y < pos.get('x')!.y;
    expect(aAboveB).toBe(yAboveX);
  });
});
//=============================================================================
