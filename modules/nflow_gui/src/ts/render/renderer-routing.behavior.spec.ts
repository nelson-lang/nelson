//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { makeRoutingHandlers } from './wire/renderer-routing';
//=============================================================================
beforeEach(() => {
  // @ts-ignore
  global.requestAnimationFrame = (cb) => {
    cb();
    return 0;
  };
});
//=============================================================================
test('applyWorkerFullRoute updates connection points and clears dirty sets', () => {
  const conn1: any = { id: 'c1', points: [] };
  const ctx: any = { state: { connections: [conn1] } };
  const applied: any[] = [];
  ctx.applyWirePaths = (m: any) => applied.push(m);
  const handlers = makeRoutingHandlers(ctx);
  // @ts-ignore
  handlers.applyWorkerFullRoute([
    [
      { x: 1, y: 2 },
      { x: 3, y: 4 },
    ],
  ]);
  expect(conn1.points.length).toBeGreaterThanOrEqual(2);
  expect(applied.length).toBeGreaterThanOrEqual(1);
  expect(ctx.state.routingDirty).toBe(false);
});
//=============================================================================
test('updateConnections with fastRouting sets points and calls applyWirePathsFast', () => {
  const connA: any = { id: 'a', from: 'b1', to: 'b2', points: [] };
  const state: any = {
    connections: [connA],
    isPanning: false,
    isPinching: false,
    routingDirty: true,
    routingScheduled: false,
    fastRouting: true,
    dirtyConnections: new Set([connA]),
    dirtyBlocks: new Set(),
  };
  const appliedFast: any[] = [];
  const ctx: any = {
    state,
    applyWirePathsFast: (s: any) => appliedFast.push(s),
    buildFastDragPathFromPorts: (c: any) => [
      { x: 0, y: 0 },
      { x: 1, y: 1 },
    ],
    updateSelectionBox: () => {},
  };
  const handlers = makeRoutingHandlers(ctx);
  // @ts-ignore
  handlers.updateConnections(false);
  expect(appliedFast.length).toBe(1);
  expect(connA.points.length).toBeGreaterThanOrEqual(2);
  expect(state.routingDirty).toBe(false);
});
//=============================================================================
