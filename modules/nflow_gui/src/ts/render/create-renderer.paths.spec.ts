//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, beforeAll, test, expect } from 'vitest';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
describe('create-renderer path rendering and routing', () => {
  let createRendererModule: any;

  beforeAll(async () => {
    createRendererModule = await import('../../../dist/render/create-renderer.js');
  });

  test('renderCurrentWirePaths clears invalid path strings', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      autoRoute: true,
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });

    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    path.setAttribute('d', 'M0,0');
    const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = {
      from: 'a',
      to: 'b',
      fromIndex: 0,
      toIndex: 0,
      path,
      hitPath,
      points: [
        { x: NaN, y: 0 },
        { x: 10, y: 0 },
      ],
    };
    state.connections.push(conn);
    wireLayer.appendChild(hitPath);
    wireLayer.appendChild(path);

    renderer.renderCurrentWirePaths(false);
    expect(conn.path.getAttribute('d') === '' || conn.path.getAttribute('d') === null).toBeTruthy();
  });

  test('renderCurrentWirePaths writes valid path d for orthogonal points', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      autoRoute: true,
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });

    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = {
      from: 'a',
      to: 'b',
      fromIndex: 0,
      toIndex: 0,
      path,
      hitPath,
      points: [
        { x: 0, y: 0 },
        { x: 100, y: 0 },
      ],
    };
    state.connections.push(conn);
    wireLayer.appendChild(hitPath);
    wireLayer.appendChild(path);

    renderer.renderCurrentWirePaths(false);
    const d = conn.path.getAttribute('d');
    expect(typeof d).toBe('string');
    expect(d!.length).toBeGreaterThan(0);
  });

  test('updateConnections handles fastRouting branch synchronously', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      fastRouting: true,
      routingDirty: true,
      autoRoute: true,
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });

    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = {
      from: 'a',
      to: 'b',
      fromIndex: 0,
      toIndex: 0,
      path,
      hitPath,
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
      ],
    };
    state.connections.push(conn);
    state.dirtyConnections.add(conn);
    wireLayer.appendChild(hitPath);
    wireLayer.appendChild(path);

    renderer.updateConnections(true);
    expect(state.routingDirty).toBe(false);
  });
});
//=============================================================================
describe('create-renderer path rendering and routing', () => {
  let createRendererModule: any;

  beforeAll(async () => {
    createRendererModule = await import('../../../dist/render/create-renderer.js');
  });

  test('renderCurrentWirePaths clears invalid path strings', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      autoRoute: true,
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });

    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    path.setAttribute('d', 'M0,0');
    const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = {
      from: 'a',
      to: 'b',
      fromIndex: 0,
      toIndex: 0,
      path,
      hitPath,
      points: [
        { x: NaN, y: 0 },
        { x: 10, y: 0 },
      ],
    };
    state.connections.push(conn);
    wireLayer.appendChild(hitPath);
    wireLayer.appendChild(path);

    renderer.renderCurrentWirePaths(false);
    expect(conn.path.getAttribute('d') === '' || conn.path.getAttribute('d') === null).toBeTruthy();
  });

  test('renderCurrentWirePaths writes valid path d for orthogonal points', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      autoRoute: true,
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });

    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = {
      from: 'a',
      to: 'b',
      fromIndex: 0,
      toIndex: 0,
      path,
      hitPath,
      points: [
        { x: 0, y: 0 },
        { x: 100, y: 0 },
      ],
    };
    state.connections.push(conn);
    wireLayer.appendChild(hitPath);
    wireLayer.appendChild(path);

    renderer.renderCurrentWirePaths(false);
    const d = conn.path.getAttribute('d');
    expect(typeof d).toBe('string');
    expect(d.length).toBeGreaterThan(0);
  });

  test('updateConnections handles fastRouting branch synchronously', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      fastRouting: true,
      routingDirty: true,
      autoRoute: true,
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });

    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = {
      from: 'a',
      to: 'b',
      fromIndex: 0,
      toIndex: 0,
      path,
      hitPath,
      points: [
        { x: 0, y: 0 },
        { x: 10, y: 0 },
      ],
    };
    state.connections.push(conn);
    state.dirtyConnections.add(conn);
    wireLayer.appendChild(hitPath);
    wireLayer.appendChild(path);

    renderer.updateConnections(true);
    expect(state.routingDirty).toBe(false);
  });
});
//=============================================================================
