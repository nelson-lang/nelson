//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Route hysteresis: reroutes keep the previous shape unless the fresh route
// is meaningfully better; kills flip-flopping between equal-cost routes.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { computeWireRoutes } from './pathfinder.js';
//=============================================================================
type Point = { x: number; y: number };
const baseSettings = {
  deterministic: true,
  incremental: false,
  fullOptimize: true,
  nearObstaclePenalty1: 2,
  nearObstaclePenalty2: 0,
  nearWirePenalty1: 3,
  nearWirePenalty2: 1,
};
//=============================================================================
// Diagonal pair: (0,0) right-exit → (10,10) left-entry. Any vertical spine
// x∈[2..8] yields an equal-cost route (len 20, 2 turns, no port penalty);
// the canonical flip-flop playground.
const nodes = [
  { id: 'a:out:0', x: 0, y: 0, dir: 'right' },
  { id: 'b:in:0', x: 10, y: 10, dir: 'left' },
];
//=============================================================================
const connections = [{ from: 'a:out:0', to: 'b:in:0', key: 'k1' }];
function spinePath(spineX: number): Point[] {
  const pts: Point[] = [];
  for (let x = 0; x <= spineX; x += 1) pts.push({ x, y: 0 });
  for (let y = 1; y <= 10; y += 1) pts.push({ x: spineX, y });
  for (let x = spineX + 1; x <= 10; x += 1) pts.push({ x, y: 10 });
  return pts;
}
//=============================================================================
function route(settings: any = {}, prevPoints: Point[] | null = null) {
  const prevSolution = prevPoints ? { wires: { k1: prevPoints } } : null;
  const res = computeWireRoutes({
    nodes: nodes as any,
    connections: connections as any,
    prevSolution: prevSolution as any,
    settings: { ...baseSettings, ...settings },
  });
  return res.wires.get('k1');
}
//=============================================================================
describe('route hysteresis', () => {
  it('keeps an equal-cost previous route instead of flip-flopping', () => {
    const fresh = route();
    expect(fresh).toBeTruthy();
    // Pick a spine that differs from the fresh route's natural choice.
    const freshSpine = fresh.points.find((p: Point) => p.y === 5)?.x;
    const altSpine = freshSpine === 5 ? 6 : 5;
    const prev = spinePath(altSpine);

    const rerouted = route({}, prev);
    expect(rerouted.points).toEqual(prev);
    expect(rerouted.keptPrev).toBe(true);
  });

  it('identical environment + own previous route → byte-identical result', () => {
    const first = route();
    const again = route({}, first.points);
    expect(JSON.stringify(again.points)).toBe(JSON.stringify(first.points));
  });

  it('is stable across 20 successive reroutes', () => {
    let current = route();
    const shape = JSON.stringify(current.points);
    for (let i = 0; i < 20; i += 1) {
      current = route({}, current.points);
      expect(JSON.stringify(current.points)).toBe(shape);
    }
  });

  it('replaces the previous route when the fresh one is ≥15% better', () => {
    // A long artificial detour: spine path plus a deep vertical excursion.
    const detour: Point[] = [];
    for (let x = 0; x <= 5; x += 1) detour.push({ x, y: 0 });
    for (let y = 1; y <= 20; y += 1) detour.push({ x: 5, y });
    for (let y = 19; y >= 10; y -= 1) detour.push({ x: 5, y });
    for (let x = 6; x <= 10; x += 1) detour.push({ x, y: 10 });

    const rerouted = route({}, detour);
    expect(rerouted.keptPrev).toBeUndefined();
    // Fresh optimal: 20 cells long, not 40+.
    expect(rerouted.cost.length).toBeLessThan(25);
  });

  it('ignores the previous route when endpoints moved', () => {
    const prev = spinePath(5);
    const movedNodes = [
      { id: 'a:out:0', x: 0, y: 2, dir: 'right' }, // moved down 2 cells
      { id: 'b:in:0', x: 10, y: 10, dir: 'left' },
    ];
    const res = computeWireRoutes({
      nodes: movedNodes as any,
      connections: connections as any,
      prevSolution: { wires: { k1: prev } } as any,
      settings: baseSettings as any,
    });
    const wire = res.wires.get('k1');
    expect(wire.keptPrev).toBeUndefined();
    expect(wire.points[0]).toEqual({ x: 0, y: 2 });
  });

  it('replaces an infeasible previous route (new obstacle on its spine)', () => {
    const prev = spinePath(5);
    const res = computeWireRoutes({
      nodes: nodes as any,
      connections: connections as any,
      obstacles: [{ x0: 4, y0: 4, x1: 6, y1: 6, owner: 'blocker' }] as any,
      prevSolution: { wires: { k1: prev } } as any,
      settings: baseSettings as any,
    });
    const wire = res.wires.get('k1');
    expect(wire).toBeTruthy();
    expect(wire.keptPrev).toBeUndefined();
    // New route avoids the obstacle interior.
    const inside = wire.points.some(
      (p: Point, i: number) =>
        i > 0 && i < wire.points.length - 1 && p.x >= 4 && p.x <= 6 && p.y >= 4 && p.y <= 6
    );
    expect(inside).toBe(false);
  });

  it('disabling hysteresis restores legacy behavior', () => {
    const fresh = route({ reuseBonus: 0, acceptThreshold: 0 });
    const prevSpine = spinePath(fresh.points.find((p: Point) => p.y === 5)?.x === 5 ? 6 : 5);
    const rerouted = route({ reuseBonus: 0, acceptThreshold: 0 }, prevSpine);
    // Without hysteresis the router re-derives its own preferred shape.
    expect(JSON.stringify(rerouted.points)).toBe(JSON.stringify(fresh.points));
    expect(rerouted.keptPrev).toBeUndefined();
  });
});
//=============================================================================
