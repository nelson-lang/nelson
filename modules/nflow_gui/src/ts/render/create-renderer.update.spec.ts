//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, afterEach, test, expect } from 'vitest';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const modulePath = '../../../dist/render/create-renderer.js';
beforeEach(() => {
  (global as any).requestAnimationFrame = (cb: any) => {
    cb();
    return 1;
  };
  (global as any).cancelAnimationFrame = () => {};
  document.body.innerHTML = '';
});
//=============================================================================
afterEach(() => {
  delete (global as any).requestAnimationFrame;
  delete (global as any).cancelAnimationFrame;
});
//=============================================================================
test('updateConnections fastRouting applies fast wire path and sets debugRenderPoints', async () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    routingDirty: true,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    autoRoute: true,
    fastRouting: true,
  };

  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = {
    from: 'b1',
    to: 'b2',
    points: [
      { x: 10, y: 10 },
      { x: 40, y: 10 },
    ],
    path,
    hitPath,
  };
  state.connections.push(conn);
  state.dirtyConnections.add(conn);
  state.blocks.set('b1', {
    id: 'b1',
    x: 10,
    y: 10,
    ports: [{ type: 'out', index: 0, x: 0, y: 0 }],
  });
  state.blocks.set('b2', { id: 'b2', x: 40, y: 10, ports: [{ type: 'in', index: 0, x: 0, y: 0 }] });

  const mod: any = await import('../../../dist/render/create-renderer.js');
  const r = mod.createRenderer({
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    onSelectBlock: () => {},
    onSelectConnection: () => {},
    onOpenSubsystem: () => {},
    onConnectionError: () => {},
  });

  r.updateConnections(true);

  expect(Array.isArray(conn.debugRenderPoints)).toBe(true);
  expect(conn.debugRenderPoints.length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
test('updateConnections with autoRoute=false applies empty paths and clears routingDirty', async () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    routingDirty: true,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    autoRoute: false,
    fastRouting: false,
  };

  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = { from: 'b1', to: 'b2', points: [], path, hitPath };
  state.connections.push(conn);

  const mod: any = await import(modulePath);
  const r = mod.createRenderer({
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    onSelectBlock: () => {},
    onSelectConnection: () => {},
    onOpenSubsystem: () => {},
    onConnectionError: () => {},
  });

  r.updateConnections(true);

  expect(state.routingDirty).toBe(false);
  expect(conn.path).toBeDefined();
});
//=============================================================================
beforeEach(() => {
  (global as any).requestAnimationFrame = (cb: any) => {
    cb();
    return 1;
  };
  (global as any).cancelAnimationFrame = () => {};
  document.body.innerHTML = '';
});
//=============================================================================
afterEach(() => {
  delete (global as any).requestAnimationFrame;
  delete (global as any).cancelAnimationFrame;
});
//=============================================================================
test('updateConnections fastRouting applies fast wire path and sets debugRenderPoints', async () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    routingDirty: true,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    autoRoute: true,
    fastRouting: true,
  };

  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = {
    from: 'b1',
    to: 'b2',
    points: [
      { x: 10, y: 10 },
      { x: 40, y: 10 },
    ],
    path,
    hitPath,
  };
  state.connections.push(conn);
  state.dirtyConnections.add(conn);
  state.blocks.set('b1', {
    id: 'b1',
    x: 10,
    y: 10,
    ports: [{ type: 'out', index: 0, x: 0, y: 0 }],
  });
  state.blocks.set('b2', { id: 'b2', x: 40, y: 10, ports: [{ type: 'in', index: 0, x: 0, y: 0 }] });

  const mod = await import('../../../dist/render/create-renderer.js');
  const r = (mod as any).createRenderer({
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    onSelectBlock: () => {},
    onSelectConnection: () => {},
    onOpenSubsystem: () => {},
    onConnectionError: () => {},
  });

  r.updateConnections(true);

  expect(Array.isArray(conn.debugRenderPoints)).toBe(true);
  expect(conn.debugRenderPoints.length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
test('updateConnections with autoRoute=false applies empty paths and clears routingDirty', async () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    routingDirty: true,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    autoRoute: false,
    fastRouting: false,
  };

  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = { from: 'b1', to: 'b2', points: [], path, hitPath };
  state.connections.push(conn);

  const mod = await import('../../../dist/render/create-renderer.js');
  const r = (mod as any).createRenderer({
    svg,
    blockLayer,
    wireLayer,
    overlayLayer,
    state,
    onSelectBlock: () => {},
    onSelectConnection: () => {},
    onOpenSubsystem: () => {},
    onConnectionError: () => {},
  });

  r.updateConnections(true);

  expect(state.routingDirty).toBe(false);
  expect(conn.path).toBeDefined();
});
//=============================================================================
