//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Pinned (user-anchored) wires: the router treats them as STATIC; their
// shape is kept verbatim and other wires route around them; automatic
// simplification never drops pinned points.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { computeWireRoutes } from './pathfinder.js';
import { simplifyOrthogonalPath, removeColinearPoints, dedupePoints } from './post-process.js';
//=============================================================================
type Point = { x: number; y: number; pin?: boolean };
const settings = {
  deterministic: true,
  incremental: false,
  fullOptimize: true,
  nearObstaclePenalty1: 2,
  nearObstaclePenalty2: 0,
  nearWirePenalty1: 3,
  nearWirePenalty2: 1,
};
//=============================================================================
function spinePathPinned(spineX: number): Point[] {
  const pts: Point[] = [];
  for (let x = 0; x <= spineX; x += 1) pts.push({ x, y: 0 });
  for (let y = 1; y <= 10; y += 1) pts.push({ x: spineX, y });
  for (let x = spineX + 1; x <= 10; x += 1) pts.push({ x, y: 10 });
  (pts.find((p) => p.x === spineX && p.y === 0) as any).pin = true;
  (pts.find((p) => p.x === spineX && p.y === 10) as any).pin = true;
  return pts;
}
//=============================================================================
describe('pinned wires are static for the router', () => {
  const nodes = [
    { id: 'a:out:0', x: 0, y: 0, dir: 'right' },
    { id: 'b:in:0', x: 10, y: 10, dir: 'left' },
  ];
  const connections = [{ from: 'a:out:0', to: 'b:in:0', key: 'k1' }];

  it('a full route keeps a pinned wire verbatim', () => {
    const pinnedPrev = spinePathPinned(7); // not the router's natural choice
    const res = computeWireRoutes({
      nodes: nodes as any,
      connections: connections as any,
      prevSolution: { wires: { k1: pinnedPrev } } as any,
      settings: settings as any,
    });
    const wire = res.wires.get('k1');
    expect(wire).toBeTruthy();
    expect(wire.points.map((p: Point) => [p.x, p.y])).toEqual(pinnedPrev.map((p) => [p.x, p.y]));
  });

  it('other wires route around a pinned wire (occupancy respected)', () => {
    const nodes2 = [
      { id: 'a:out:0', x: 0, y: 0, dir: 'right' },
      { id: 'b:in:0', x: 10, y: 10, dir: 'left' },
      { id: 'c:out:0', x: 0, y: 5, dir: 'right' },
      { id: 'd:in:0', x: 20, y: 5, dir: 'left' },
    ];
    const connections2 = [
      { from: 'a:out:0', to: 'b:in:0', key: 'k1' },
      { from: 'c:out:0', to: 'd:in:0', key: 'k2' },
    ];
    const pinnedPrev = spinePathPinned(5); // vertical run at x=5, y∈[0..10]
    const res = computeWireRoutes({
      nodes: nodes2 as any,
      connections: connections2 as any,
      prevSolution: { wires: { k1: pinnedPrev } } as any,
      settings: settings as any,
    });
    const k2 = res.wires.get('k2');
    expect(k2).toBeTruthy();
    // k2 runs along y=5 and must HOP the pinned vertical at x=5 (crossing is
    // allowed) but never run ALONG it: no shared edge.
    const k1Edges = new Set<string>();
    const p = pinnedPrev;
    for (let i = 1; i < p.length; i += 1) {
      k1Edges.add(`${p[i - 1].x},${p[i - 1].y}|${p[i].x},${p[i].y}`);
      k1Edges.add(`${p[i].x},${p[i].y}|${p[i - 1].x},${p[i - 1].y}`);
    }
    for (let i = 1; i < k2.points.length; i += 1) {
      const key = `${k2.points[i - 1].x},${k2.points[i - 1].y}|${k2.points[i].x},${k2.points[i].y}`;
      expect(k1Edges.has(key)).toBe(false);
    }
  });
});
//=============================================================================
describe('pinned wires: Stage B free sub-path routing', () => {
  const connections = [{ from: 'a:out:0', to: 'b:in:0', key: 'k1' }];

  it('a moved port reroutes only the free head; the pinned chain is verbatim', () => {
    const movedNodes = [
      { id: 'a:out:0', x: 0, y: 2, dir: 'right' }, // port moved down 2 cells
      { id: 'b:in:0', x: 10, y: 10, dir: 'left' },
    ];
    const pinnedPrev = spinePathPinned(7); // pins at (7,0) and (7,10)
    const res = computeWireRoutes({
      nodes: movedNodes as any,
      connections: connections as any,
      prevSolution: { wires: { k1: pinnedPrev } } as any,
      settings: settings as any,
    });
    const wire = res.wires.get('k1');
    expect(wire).toBeTruthy();
    expect(wire.points[0]).toMatchObject({ x: 0, y: 2 });
    const pins = wire.points.filter((p: Point) => p.pin);
    expect(pins.map((p: Point) => [p.x, p.y])).toEqual([
      [7, 0],
      [7, 10],
    ]);
    const i0 = wire.points.findIndex((p: Point) => p.pin);
    const i1 = wire.points.map((p: Point) => Boolean(p.pin)).lastIndexOf(true);
    for (let i = i0; i <= i1; i += 1) expect(wire.points[i].x).toBe(7);
    expect(wire.points[wire.points.length - 1]).toMatchObject({ x: 10, y: 10 });
  });

  it('falls back to the full previous shape when a free end cannot be routed', () => {
    // Wall of obstacles isolating the from port: the head cannot reach the
    // first anchor.
    const pinnedPrev = spinePathPinned(7);
    const res = computeWireRoutes({
      nodes: [
        { id: 'a:out:0', x: 0, y: 0, dir: 'right' },
        { id: 'b:in:0', x: 10, y: 10, dir: 'left' },
      ] as any,
      connections: connections as any,
      obstacles: [
        // Box enclosing the from port (start/startStep cells are exempt but
        // every exit is walled): the head leg cannot reach the first anchor.
        { x0: -1, y0: -1, x1: 2, y1: -1, owner: 'wallTop' },
        { x0: -1, y0: 1, x1: 2, y1: 1, owner: 'wallBottom' },
        { x0: -1, y0: -1, x1: -1, y1: 1, owner: 'wallLeft' },
        { x0: 2, y0: -1, x1: 2, y1: 1, owner: 'wallRight' },
      ] as any,
      prevSolution: { wires: { k1: pinnedPrev } } as any,
      settings: settings as any,
    });
    const wire = res.wires.get('k1');
    expect(wire).toBeTruthy();
    expect(wire.keptPrev).toBe(true);
    const pins = wire.points.filter((p: Point) => p.pin);
    expect(pins.length).toBe(2);
  });
});
//=============================================================================
describe('pin-preserving simplification', () => {
  it('simplifyOrthogonalPath keeps pinned interior points', () => {
    const pts: Point[] = [
      { x: 0, y: 0 },
      { x: 5, y: 0, pin: true },
      { x: 10, y: 0 },
    ];
    const out = simplifyOrthogonalPath(pts as any) as Point[];
    expect(out.some((p) => p.pin && p.x === 5)).toBe(true);
  });

  it('removeColinearPoints never merges a pinned point', () => {
    const pts: Point[] = [
      { x: 0, y: 0 },
      { x: 5, y: 0, pin: true },
      { x: 10, y: 0 },
    ];
    const out = removeColinearPoints(pts as any) as Point[];
    expect(out.length).toBe(3);
  });

  it('dedupePoints donates the pin flag to the kept duplicate', () => {
    const pts: Point[] = [
      { x: 0, y: 0 },
      { x: 0, y: 0, pin: true },
      { x: 10, y: 0 },
    ];
    const out = dedupePoints(pts as any) as Point[];
    expect(out.length).toBe(2);
    expect(out[0].pin).toBe(true);
  });
});
//=============================================================================
