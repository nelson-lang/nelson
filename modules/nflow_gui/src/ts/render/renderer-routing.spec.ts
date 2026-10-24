//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/wire/renderer-routing.js');
});
//=============================================================================
test('applyWorkerFullRoute applies points to connections and calls helpers', () => {
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = { from: 'a', to: 'b', points: null, path };
  const ctx: any = {
    state: { connections: [conn], dirtyBlocks: new Set(['x']), dirtyConnections: new Set([conn]) },
    applyWirePaths: vi.fn(),
    updateSelectionBox: vi.fn(),
    refreshDebugLog: vi.fn(),
    updateWireCornerHandles: vi.fn(),
  };
  const h = mod.makeRoutingHandlers(ctx);
  const routes = [
    [
      { x: 1, y: 2 },
      { x: 3, y: 4 },
    ],
  ];
  h.applyWorkerFullRoute(routes);
  expect(conn.points).toEqual(routes[0]);
  expect(ctx.applyWirePaths).toHaveBeenCalled();
  expect(ctx.state.routingDirty).toBe(false);
  expect(ctx.state.dirtyBlocks.size).toBe(0);
  expect(ctx.state.dirtyConnections.size).toBe(0);
});
//=============================================================================
test('updateConnections fastRouting path calls applyWirePathsFast', () => {
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = {
    from: 'a',
    to: 'b',
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
    path,
  };
  const applyFast = vi.fn();
  const ctx: any = {
    state: {
      connections: [conn],
      fastRouting: true,
      autoRoute: true,
      routingDirty: true,
      routingScheduled: false,
      dirtyConnections: new Set([conn]),
      dirtyBlocks: new Set(),
    },
    buildFastDragPathFromPorts: (c: any) => [
      { x: 0, y: 0 },
      { x: 11, y: 0 },
    ],
    applyWirePathsFast: applyFast,
    updateSelectionBox: vi.fn(),
    updateWireCornerHandles: vi.fn(),
  };
  const h = mod.makeRoutingHandlers(ctx as any);
  h.updateConnections();
  expect(applyFast).toHaveBeenCalled();
  expect(conn.points).toEqual([
    { x: 0, y: 0 },
    { x: 11, y: 0 },
  ]);
  expect(ctx.state.routingDirty).toBe(false);
  expect(ctx.state.dirtyConnections.size).toBe(0);
});
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, test, vi } from 'vitest';
import { makeRoutingHandlers } from './wire/renderer-routing.js';
//=============================================================================
describe('renderer-routing handlers', () => {
  test('applyWorkerFullRoute sets connection points and clears dirty state', () => {
    const connA: any = { from: 1, to: 2 };
    const connB: any = { from: 3, to: 4 };
    const state: any = {
      connections: [connA, connB],
    };
    const ctx: any = {
      state,
      applyWirePaths: vi.fn(),
      updateSelectionBox: vi.fn(),
      refreshDebugLog: vi.fn(),
      updateWireCornerHandles: vi.fn(),
    };
    state.dirtyBlocks = new Set(['x']);
    state.dirtyConnections = new Set([1]);
    const handlers = makeRoutingHandlers(ctx as any);
    handlers.applyWorkerFullRoute([
      [
        { x: 1, y: 2 },
        { x: 3, y: 4 },
      ],
      null,
    ]);
    expect(Array.isArray(state.connections[0].points)).toBe(true);
    expect(state.connections[0].points.length).toBeGreaterThanOrEqual(2);
    expect(state.routingDirty).toBe(false);
    expect((state.dirtyBlocks && state.dirtyBlocks.size) === 0).toBe(true);
    expect((state.dirtyConnections && state.dirtyConnections.size) === 0).toBe(true);
    expect(ctx.applyWirePaths).toHaveBeenCalled();
  });
});
//=============================================================================
