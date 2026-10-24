//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import fs from 'fs';
import path from 'path';
import { describe, expect, test, beforeEach } from 'vitest';
import { makeConnectionHandlers } from './interaction/renderer-connection.js';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('./interaction/renderer-connection.js');
  document.body.innerHTML = '';
});
//=============================================================================
describe('renderer-connection factory', () => {
  test('makeConnectionHandlers returns object', () => {
    const ctx: any = { svg: null, state: { connections: [] } };
    const handlers = makeConnectionHandlers(ctx as any);
    expect(typeof handlers).toBe('object');
  });
});
//=============================================================================
test('loads renderer-connection.ts if present', async () => {
  const m = await import('./interaction/renderer-connection');
  expect(m).toBeDefined();
});
//=============================================================================
test('createConnection detects duplicate and returns null with error', () => {
  const existing: any = { from: 'a', to: 'b', toIndex: 0, fromIndex: 0 };
  const state: any = { connections: [existing], blocks: new Map() };
  const wireLayer = document.createElement('div');
  const ctx: any = { state };
  const handlers = mod.makeConnectionHandlers(Object.assign(ctx, { wireLayer }));
  const res = handlers.createConnection('a', 'b', 0, 0);
  expect(res).toBeNull();
  expect((ctx as any).lastConnectionError).toBeDefined();
  expect((ctx as any).lastConnectionError.reason).toBe('duplicate');
});
//=============================================================================
test('createConnection returns null when diagram is locked', async () => {
  const state: any = { connections: [], blocks: new Map() };
  const wireLayer = document.createElement('div');
  let reported = '';
  const ctx: any = {
    state,
    wireLayer,
    onConnectionError: (m: string) => {
      reported = m;
    },
  };
  const handlers = mod.makeConnectionHandlers(ctx);
  const stateMod = await import('../app/state.js');
  const originalLocked = stateMod.state.locked;
  stateMod.state.locked = true;
  const res = handlers.createConnection('a', 'b', 0, 0);
  expect(res).toBeNull();
  expect((ctx as any).lastConnectionError).toBeDefined();
  expect((ctx as any).lastConnectionError.reason).toBe(FieldNames.kLocked);
  expect(reported).toContain(FieldNames.kLocked);
  stateMod.state.locked = originalLocked;
});
//=============================================================================
test('createConnection rejects input occupied and calls onConnectionError', () => {
  const occupied: any = { from: 'x', to: 'to1', toIndex: 1, fromIndex: 0 };
  const state: any = { connections: [occupied], blocks: new Map() };
  let reported = '';
  const ctx: any = {
    state,
    wireLayer: document.createElement('div'),
    onConnectionError: (m: string) => {
      reported = m;
    },
  };
  const handlers = mod.makeConnectionHandlers(ctx);
  const res = handlers.createConnection('new', 'to1', 1, 0);
  expect(res).toBeNull();
  expect((ctx as any).lastConnectionError).toBeDefined();
  expect((ctx as any).lastConnectionError.reason).toBe('input_occupied');
  expect(reported).toContain('Input to1.in1');
});
//=============================================================================
test('createConnection succeeds and appends connection, sets routingDirty and dispatches events', () => {
  const state: any = { connections: [], blocks: new Map(), loadingDiagram: false, autoRoute: true };
  const wireLayer = document.createElement('div');
  document.body.appendChild(wireLayer);
  const appliedFast = { called: false };
  const ctx: any = {
    state,
    wireLayer,
    buildFastDragPathFromPorts: (_c: any) => [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
    applyWirePathsFast: (_s: Set<any>) => {
      appliedFast.called = true;
    },
    applyWirePaths: (_m: Map<any, any>) => {},
    updateWireCornerHandles: () => {},
    queueRouteAfterPreview: () => {},
  };
  let diag = false;
  let upd = false;
  window.addEventListener('diagramChanged', () => {
    diag = true;
  });
  window.addEventListener('updateStatusBar', () => {
    upd = true;
  });
  const handlers = mod.makeConnectionHandlers(ctx);
  const conn = handlers.createConnection('f', 't', 0, 0);
  expect(conn).not.toBeNull();
  expect(state.connections.includes(conn)).toBe(true);
  expect(appliedFast.called).toBe(true);
  expect(state.routingDirty).toBe(true);
  expect(diag).toBe(true);
  expect(upd).toBe(true);
});
//=============================================================================
test('createConnection during loadingDiagram sets empty path and does not route', () => {
  const state: any = { connections: [], blocks: new Map(), loadingDiagram: true };
  const wireLayer = document.createElement('div');
  document.body.appendChild(wireLayer);
  const ctx: any = { state, wireLayer };
  const handlers = mod.makeConnectionHandlers(ctx);
  const conn = handlers.createConnection('f', 't', 0, 0);
  expect(conn).not.toBeNull();
  expect(conn.path.getAttribute('d')).toBe('');
  expect(conn.hitPath?.getAttribute('d')).toBe('');
});
//=============================================================================
test('createConnection with autoRoute false applies wire paths without fast routing', () => {
  const state: any = {
    connections: [],
    blocks: new Map(),
    loadingDiagram: false,
    autoRoute: false,
  };
  const wireLayer = document.createElement('div');
  document.body.appendChild(wireLayer);
  const applied = { called: false };
  const ctx: any = {
    state,
    wireLayer,
    applyWirePaths: (_m: Map<any, any>) => {
      applied.called = true;
    },
    updateWireCornerHandles: () => {},
  };
  const handlers = mod.makeConnectionHandlers(ctx);
  const conn = handlers.createConnection('f', 't', 0, 0);
  expect(conn).not.toBeNull();
  expect(applied.called).toBe(true);
  expect(state.routingDirty).toBe(false);
});
//=============================================================================
test('findNearestConnection returns nearest connection within threshold', () => {
  const conn: any = {
    points: [
      { x: 0, y: 0 },
      { x: 100, y: 0 },
    ],
  };
  const state: any = { connections: [conn], blocks: new Map() };
  const ctx: any = { state };
  const handlers = mod.makeConnectionHandlers(ctx);
  const found = handlers.findNearestConnection({ x: 5, y: 3 }, 10);
  expect(found).toBe(conn);
  const notFound = handlers.findNearestConnection({ x: 1000, y: 1000 }, 10);
  expect(notFound).toBeNull();
});
//=============================================================================
test('findNearestConnection with no points returns null', () => {
  const conn: any = { points: [] };
  const state: any = { connections: [conn], blocks: new Map() };
  const ctx: any = { state };
  const handlers = mod.makeConnectionHandlers(ctx);
  const found = handlers.findNearestConnection({ x: 0, y: 0 }, 10);
  expect(found).toBeNull();
});
//=============================================================================
test('findNearestConnection chooses closest among multiple', () => {
  const conn1: any = {
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
  };
  const conn2: any = {
    points: [
      { x: 0, y: 10 },
      { x: 10, y: 10 },
    ],
  };
  const state: any = { connections: [conn1, conn2], blocks: new Map() };
  const ctx: any = { state };
  const handlers = mod.makeConnectionHandlers(ctx);
  const found = handlers.findNearestConnection({ x: 5, y: 1 }, 10);
  expect(found).toBe(conn1);
});
//=============================================================================
test('deleteConnection removes paths and updates state', () => {
  const connPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  document.body.appendChild(connPath);
  document.body.appendChild(hitPath);
  const conn: any = { path: connPath, hitPath };
  const state: any = { connections: [conn], blocks: new Map() };
  const ctx: any = { state };
  const handlers = mod.makeConnectionHandlers(ctx);
  handlers.deleteConnection(conn);
  expect(state.connections.length).toBe(0);
});
//=============================================================================
test('deleteConnection does nothing when locked', async () => {
  const conn: any = { path: { remove: () => {} }, hitPath: { remove: () => {} } };
  const state: any = { connections: [conn], blocks: new Map() };
  const ctx: any = { state };
  const stateMod = await import('../app/state.js');
  const originalLocked = stateMod.state.locked;
  stateMod.state.locked = true;
  const handlers = mod.makeConnectionHandlers(ctx);
  handlers.deleteConnection(conn);
  expect(state.connections.length).toBe(1);
  stateMod.state.locked = originalLocked;
});
//=============================================================================
test('deleteBlock removes related connections and deletes block', () => {
  const connPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = { from: 'b1', to: 'b2', path: connPath, hitPath };
  const blockGroup = document.createElement('div');
  document.body.appendChild(blockGroup);
  const block = { id: 'b1', group: blockGroup };
  const blocks = new Map();
  blocks.set('b1', block);
  const state: any = { connections: [conn], blocks };
  const ctx: any = { state };
  const handlers = mod.makeConnectionHandlers(ctx);
  handlers.deleteBlock('b1');
  expect(state.connections.length).toBe(0);
  expect(blocks.has('b1')).toBe(false);
});
//=============================================================================
test('deleteBlock does nothing when locked', async () => {
  const conn: any = {
    from: 'b1',
    to: 'b2',
    path: { remove: () => {} },
    hitPath: { remove: () => {} },
  };
  const block = { id: 'b1', group: { remove: () => {} } };
  const blocks = new Map();
  blocks.set('b1', block);
  const state: any = { connections: [conn], blocks };
  const ctx: any = { state };
  const stateMod = await import('../app/state.js');
  const originalLocked = stateMod.state.locked;
  stateMod.state.locked = true;
  const handlers = mod.makeConnectionHandlers(ctx);
  handlers.deleteBlock('b1');
  expect(state.connections.length).toBe(1);
  expect(blocks.has('b1')).toBe(true);
  stateMod.state.locked = originalLocked;
});
//=============================================================================
test('createConnection with existing dirtyConnections does not recreate set', () => {
  const state: any = {
    connections: [],
    blocks: new Map(),
    loadingDiagram: false,
    autoRoute: true,
    dirtyConnections: new Set(),
  };
  const wireLayer = document.createElement('div');
  document.body.appendChild(wireLayer);
  const ctx: any = {
    state,
    wireLayer,
    buildFastDragPathFromPorts: () => [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
    applyWirePathsFast: () => {},
    applyWirePaths: () => {},
    updateWireCornerHandles: () => {},
    queueRouteAfterPreview: () => {},
  };
  const handlers = mod.makeConnectionHandlers(ctx);
  const conn = handlers.createConnection('f', 't', 0, 0);
  expect(conn).not.toBeNull();
  expect(state.dirtyConnections.has(conn)).toBe(true);
});
//=============================================================================
