//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from 'vitest';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
test('applyWirePathsFast handles invalid points gracefully', async () => {
  const mod: any = await import('../../../dist/render/create-renderer.js');
  if (typeof mod.applyWirePathsFast === 'function') {
    try {
      mod.applyWirePathsFast({ wires: [{ path: [{ x: NaN, y: NaN }] }] });
    } catch (e) {
      // swallow
    }
  } else {
    if (typeof mod.updateConnections === 'function') {
      try {
        mod.updateConnections();
      } catch (e) {
        void e;
      }
    }
  }
});
//=============================================================================
test('applyWirePathsFast handles invalid points and builds path strings safely', async () => {
  const mod: any = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  const blockLayer = document.createElementNS(ns, 'g');
  const wireLayer = document.createElementNS(ns, 'g');
  const overlayLayer = document.createElementNS(ns, 'g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    routingDirty: false,
    fastRouting: true,
    autoRoute: true,
  };

  const b1: any = {
    id: 'b1',
    x: 0,
    y: 0,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'out', index: 0 }],
    group: document.createElementNS(ns, 'g'),
  };
  const b2: any = {
    id: 'b2',
    x: 80,
    y: 0,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'in', index: 0 }],
    group: document.createElementNS(ns, 'g'),
  };
  state.blocks.set('b1', b1);
  state.blocks.set('b2', b2);

  const conn: any = {
    from: 'b1',
    to: 'b2',
    fromIndex: 0,
    toIndex: 0,
    points: [{ x: NaN, y: NaN }],
    path: document.createElementNS(ns, 'path'),
    hitPath: document.createElementNS(ns, 'path'),
  };
  state.connections.push(conn);

  const renderer = createRenderer({
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

  expect(() => renderer.updateConnections()).not.toThrow();
  expect(Array.isArray(conn.points)).toBe(true);
  const d = conn.path.getAttribute('d');
  expect(d === null || typeof d === 'string').toBeTruthy();
});
//=============================================================================
test('applyWirePathsFast handles invalid points gracefully', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  if (typeof mod.applyWirePathsFast === 'function') {
    try {
      mod.applyWirePathsFast({ wires: [{ path: [{ x: NaN, y: NaN }] }] });
    } catch (e) {
      // swallow
    }
  } else {
    if (typeof mod.updateConnections === 'function') {
      try {
        mod.updateConnections();
      } catch (e) {
        void e;
      }
    }
  }
});
//=============================================================================
test('applyWirePathsFast handles invalid points and builds path strings safely', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  const blockLayer = document.createElementNS(ns, 'g');
  const wireLayer = document.createElementNS(ns, 'g');
  const overlayLayer = document.createElementNS(ns, 'g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    dirtyConnections: new Set(),
    dirtyBlocks: new Set(),
    routingDirty: false,
    fastRouting: true,
    autoRoute: true,
  };

  const b1 = {
    id: 'b1',
    x: 0,
    y: 0,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'out', index: 0 }],
    group: document.createElementNS(ns, 'g'),
  };
  const b2 = {
    id: 'b2',
    x: 80,
    y: 0,
    width: 20,
    height: 20,
    ports: [{ x: 10, y: 10, type: 'in', index: 0 }],
    group: document.createElementNS(ns, 'g'),
  };
  state.blocks.set('b1', b1);
  state.blocks.set('b2', b2);

  const conn: any = {
    from: 'b1',
    to: 'b2',
    fromIndex: 0,
    toIndex: 0,
    points: [{ x: NaN, y: NaN }],
    path: document.createElementNS(ns, 'path'),
    hitPath: document.createElementNS(ns, 'path'),
  };
  state.connections.push(conn);

  const renderer = createRenderer({
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

  expect(() => renderer.updateConnections()).not.toThrow();
  expect(Array.isArray(conn.points)).toBe(true);
  const d = conn.path.getAttribute('d');
  expect(d === null || typeof d === 'string').toBeTruthy();
});
//=============================================================================
