//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Behavioral tests for minimum-disturbance repair during the fast-routing
// (drag) path: routed wires deform rigidly instead of snapping to an S/Z
// preview, middles stay frozen, and the baseline resets after routing.
//=============================================================================
import { makeRoutingHandlers } from './wire/renderer-routing';
//=============================================================================
beforeEach(() => {
  // Make rAF synchronous for tests
  // @ts-ignore
  global.requestAnimationFrame = (cb: any) => {
    cb();
    return 0;
  };
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
function makeDragHarness() {
  const a = makeBlock('a', 100, 100); // out port at (180, 120)
  const b = makeBlock('b', 400, 100); // in port at (400, 120)
  const conn: any = {
    from: 'a',
    to: 'b',
    fromIndex: 0,
    toIndex: 0,
    // Routed shape with a distinctive middle bend that an S/Z preview
    // would destroy.
    points: [
      { x: 180, y: 120 },
      { x: 250, y: 120 },
      { x: 250, y: 200 },
      { x: 340, y: 200 },
      { x: 340, y: 120 },
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
  const ctx: any = {
    state,
    applyWirePathsFast: () => {},
    buildFastDragPathFromPorts: () => [
      { x: 0, y: 0 },
      { x: 1, y: 1 },
    ],
    updateSelectionBox: () => {},
  };
  const handlers = makeRoutingHandlers(ctx);
  return { a, b, conn, state, ctx, handlers };
}
//=============================================================================
test('drag deforms the routed shape rigidly: middle points frozen, no S/Z', () => {
  const { a, conn, state, handlers } = makeDragHarness();

  // First fast pass captures the baseline (no movement yet).
  handlers.updateConnections(false);
  expect(conn.points.map((p: any) => [p.x, p.y])).toEqual([
    [180, 120],
    [250, 120],
    [250, 200],
    [340, 200],
    [340, 120],
    [400, 120],
  ]);

  a.y = 130;
  state.routingDirty = true;
  state.dirtyConnections.add(conn);
  handlers.updateConnections(false);

  // Head follows the port; middle and tail frozen. NOT a 4-point S/Z.
  expect(conn.points.map((p: any) => [p.x, p.y])).toEqual([
    [180, 150],
    [250, 150],
    [250, 200],
    [340, 200],
    [340, 120],
    [400, 120],
  ]);
});
//=============================================================================
test('repair is drift-free: repeated fast passes at the same position are stable', () => {
  const { a, conn, state, handlers } = makeDragHarness();
  handlers.updateConnections(false);

  a.y = 140;
  for (let i = 0; i < 5; i += 1) {
    state.routingDirty = true;
    state.dirtyConnections.add(conn);
    handlers.updateConnections(false);
  }
  const after = JSON.stringify(conn.points);

  state.routingDirty = true;
  state.dirtyConnections.add(conn);
  handlers.updateConnections(false);
  expect(JSON.stringify(conn.points)).toBe(after);
});
//=============================================================================
test('a full drag round-trip restores the original shape exactly', () => {
  const { a, conn, state, handlers } = makeDragHarness();
  const original = JSON.stringify(conn.points);
  handlers.updateConnections(false);

  a.y = 160;
  state.routingDirty = true;
  state.dirtyConnections.add(conn);
  handlers.updateConnections(false);

  a.y = 100;
  state.routingDirty = true;
  state.dirtyConnections.add(conn);
  handlers.updateConnections(false);

  expect(JSON.stringify(conn.points)).toBe(original);
});
//=============================================================================
test('fresh wires without a route still use the S/Z fallback', () => {
  const { conn, state, ctx, handlers } = makeDragHarness();
  conn.points = [];
  const fallback = [
    { x: 5, y: 5 },
    { x: 6, y: 6 },
  ];
  ctx.buildFastDragPathFromPorts = () => fallback;
  state.routingDirty = true;
  state.dirtyConnections.add(conn);
  handlers.updateConnections(false);
  expect(conn.points).toBe(fallback);
});
//=============================================================================
test('dirty blocks pull their connections into the repaired set', () => {
  const { a, conn, state, handlers } = makeDragHarness();
  handlers.updateConnections(false);

  a.x = 90;
  state.routingDirty = true;
  state.dirtyConnections = new Set();
  state.dirtyBlocks = new Set(['a']);
  handlers.updateConnections(false);

  // Terminal segment stretched; tail frozen.
  expect(conn.points[0]).toMatchObject({ x: 170, y: 120 });
  expect(conn.points[conn.points.length - 1]).toMatchObject({ x: 400, y: 120 });
});
//=============================================================================
