//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Live (mid-drag) routing: debounced native routes refine the repaired
// preview without flicker, and results stick via baseline replacement.
//=============================================================================
import { vi } from 'vitest';
import { makeRoutingHandlers } from './wire/renderer-routing';
//=============================================================================
beforeEach(() => {
  vi.useFakeTimers();
  // @ts-ignore
  global.requestAnimationFrame = (cb: any) => {
    cb();
    return 0;
  };
});
//=============================================================================
afterEach(() => {
  vi.useRealTimers();
});
//=============================================================================
function makeBlock(id: string, x: number, y: number) {
  return {
    id,
    x,
    y,
    width: 80,
    height: 40,
    rotation: 0,
    ports: [
      { type: 'in', index: 0, x: 0, y: 20 },
      { type: 'out', index: 0, x: 80, y: 20 },
    ],
  };
}
//=============================================================================
function makeHarness({ routeImpl }: { routeImpl?: (ms: number) => Promise<any> } = {}) {
  const a = makeBlock('a', 100, 100); // out port (180, 120)
  const b = makeBlock('b', 400, 100); // in port (400, 120)
  const conn: any = {
    from: 'a',
    to: 'b',
    fromIndex: 0,
    toIndex: 0,
    points: [
      { x: 180, y: 120 },
      { x: 290, y: 120 },
      { x: 290, y: 200 },
      { x: 400, y: 200 },
      { x: 400, y: 120 },
    ],
  };
  const state: any = {
    blocks: new Map([
      ['a', a],
      ['b', b],
    ]),
    connections: [conn],
    isPanning: false,
    isPinching: false,
    routingDirty: true,
    routingScheduled: false,
    fastRouting: true,
    dirtyConnections: new Set([conn]),
    dirtyBlocks: new Set(),
  };
  const routeCalls: number[] = [];
  const ctx: any = {
    state,
    applyWirePathsFast: () => {},
    buildFastDragPathFromPorts: () => [
      { x: 0, y: 0 },
      { x: 1, y: 1 },
    ],
    updateSelectionBox: () => {},
    countTurns: (pts: any[]) => {
      let turns = 0;
      for (let i = 2; i < pts.length; i += 1) {
        const d1 = pts[i - 1].x === pts[i - 2].x ? 'V' : 'H';
        const d2 = pts[i].x === pts[i - 1].x ? 'V' : 'H';
        if (d1 !== d2) turns += 1;
      }
      return turns;
    },
    segmentHitsAnyBlock: () => false,
    routeHelpers: {
      routeAllInWorker: (ms: number) => {
        routeCalls.push(ms);
        return routeImpl ? routeImpl(ms) : Promise.resolve({ stale: true });
      },
    },
  };
  const handlers = makeRoutingHandlers(ctx);
  return { a, b, conn, state, ctx, handlers, routeCalls };
}
//=============================================================================
function fastPass(harness: any) {
  harness.state.routingDirty = true;
  harness.state.dirtyConnections.add(harness.conn);
  harness.handlers.updateConnections(false);
}
//=============================================================================
test('debounce coalesces rapid fast passes into a single route call', async () => {
  const h = makeHarness();
  for (let i = 0; i < 6; i += 1) {
    fastPass(h);
    vi.advanceTimersByTime(20); // pointer keeps moving, never 50ms quiet
  }
  expect(h.routeCalls.length).toBe(0);
  vi.advanceTimersByTime(60); // pointer quiet
  await Promise.resolve();
  expect(h.routeCalls.length).toBe(1);
  expect(h.routeCalls[0]).toBe(60);
});
//=============================================================================
test('a routed result with fewer bends replaces the preview and its baseline', async () => {
  const better = [
    [
      { x: 180, y: 120 },
      { x: 290, y: 120 },
      { x: 290, y: 120 },
      { x: 400, y: 120 },
    ],
  ];
  const h = makeHarness({
    routeImpl: () => Promise.resolve({ stale: false, routes: better }),
  });
  fastPass(h);
  vi.advanceTimersByTime(60);
  await Promise.resolve();
  await Promise.resolve();

  // Straight line (0 bends) beats the 4-bend preview.
  expect(h.conn.points.map((p: any) => [p.x, p.y])).toEqual([
    [180, 120],
    [290, 120],
    [290, 120],
    [400, 120],
  ]);

  // Baseline replaced: the next fast pass deforms the ROUTED shape.
  h.a.y = 130; // port moves to (180, 150)
  fastPass(h);
  expect(h.conn.points[0]).toMatchObject({ x: 180, y: 150 });
  // Frozen tail comes from the routed shape, not the old preview.
  expect(h.conn.points[h.conn.points.length - 1]).toMatchObject({ x: 400, y: 120 });
});
//=============================================================================
test('an equal-bends result is discarded (no flicker)', async () => {
  const equal = [
    [
      { x: 180, y: 120 },
      { x: 250, y: 120 },
      { x: 250, y: 200 },
      { x: 400, y: 200 },
      { x: 400, y: 120 },
    ],
  ];
  const h = makeHarness({
    routeImpl: () => Promise.resolve({ stale: false, routes: equal }),
  });
  const before = JSON.stringify(h.conn.points);
  fastPass(h);
  vi.advanceTimersByTime(60);
  await Promise.resolve();
  await Promise.resolve();
  expect(JSON.stringify(h.conn.points)).toBe(before);
});
//=============================================================================
test('stale results are discarded', async () => {
  const h = makeHarness({
    routeImpl: () =>
      Promise.resolve({
        stale: true,
        routes: [
          [
            { x: 0, y: 0 },
            { x: 9, y: 9 },
          ],
        ],
      }),
  });
  const before = JSON.stringify(h.conn.points);
  fastPass(h);
  vi.advanceTimersByTime(60);
  await Promise.resolve();
  await Promise.resolve();
  expect(JSON.stringify(h.conn.points)).toBe(before);
});
//=============================================================================
test('a result arriving after the drag ended is discarded', async () => {
  let resolveRoute: (v: any) => void = () => {};
  const h = makeHarness({
    routeImpl: () =>
      new Promise((resolve) => {
        resolveRoute = resolve;
      }),
  });
  const before = JSON.stringify(h.conn.points);
  fastPass(h);
  vi.advanceTimersByTime(60);
  await Promise.resolve();

  // Drag ends before the result lands.
  h.state.fastRouting = false;
  resolveRoute({
    stale: false,
    routes: [
      [
        { x: 180, y: 120 },
        { x: 400, y: 120 },
      ],
    ],
  });
  await Promise.resolve();
  await Promise.resolve();
  expect(JSON.stringify(h.conn.points)).toBe(before);
});
//=============================================================================
test('a preview crossing a block is replaced even at equal bends', async () => {
  const equal = [
    [
      { x: 180, y: 120 },
      { x: 250, y: 120 },
      { x: 250, y: 60 },
      { x: 400, y: 60 },
      { x: 400, y: 120 },
    ],
  ];
  const h = makeHarness({
    routeImpl: () => Promise.resolve({ stale: false, routes: equal }),
  });
  h.ctx.segmentHitsAnyBlock = () => true; // preview collides
  fastPass(h);
  vi.advanceTimersByTime(60);
  await Promise.resolve();
  await Promise.resolve();
  expect(h.conn.points.map((p: any) => [p.x, p.y])).toEqual(equal[0].map((p) => [p.x, p.y]));
});
//=============================================================================
test('live routing is gated off on large diagrams (>300 connections)', async () => {
  const h = makeHarness();
  // Inflate the connection list past the gate; the dirty conn stays first.
  for (let i = 0; i < 305; i += 1) {
    h.state.connections.push({ from: `x${i}`, to: `y${i}`, points: [] });
  }
  fastPass(h);
  vi.advanceTimersByTime(120);
  await Promise.resolve();
  expect(h.routeCalls.length).toBe(0);
});
//=============================================================================
