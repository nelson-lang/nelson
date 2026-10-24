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
test('applyWirePaths sets path d for valid connection points', async () => {
  const mod: any = await import(modulePath);
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
    routingDirty: false,
    autoRoute: true,
    fastRouting: false,
  };
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = {
    from: 'b1',
    to: 'b2',
    points: [
      { x: 0, y: 0 },
      { x: 20, y: 0 },
    ],
    path,
    hitPath,
  };
  state.connections.push(conn);

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

  r.renderCurrentWirePaths(true);
  const d = conn.path.getAttribute('d');
  expect(typeof d).toBe('string');
  expect(d!.length).toBeGreaterThan(0);
  expect(Array.isArray(conn.debugRenderPoints)).toBe(true);
});
//=============================================================================
test('applyWirePaths clears d when points are invalid', async () => {
  const mod: any = await import(modulePath);
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
    routingDirty: false,
    autoRoute: true,
    fastRouting: false,
  };
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = {
    from: 'b1',
    to: 'b2',
    points: [
      { x: NaN, y: 0 },
      { x: 20, y: 0 },
    ],
    path,
    hitPath,
  };
  state.connections.push(conn);

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
  r.renderCurrentWirePaths(false);
  const d = conn.path.getAttribute('d');
  expect(d === '' || d === null).toBe(true);
});
//=============================================================================
test('clientToSvg returns client coords when getScreenCTM is falsy and transforms when available', async () => {
  const mod: any = await import(modulePath);
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = { blocks: new Map(), connections: [], nextId: 1 };
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

  svg.createSVGPoint = () =>
    ({ x: 100, y: 200, matrixTransform: () => ({ x: 100, y: 200 }) } as any);
  svg.getScreenCTM = () => null as any;
  const p1 = r.clientToSvg(10, 20);
  expect(p1.x).toBe(10);
  expect(p1.y).toBe(20);

  svg.getScreenCTM = () => ({ inverse: () => ({}) } as any);
  svg.createSVGPoint = () => ({ x: 10, y: 20, matrixTransform: () => ({ x: 10, y: 20 }) } as any);
  const p2 = r.clientToSvg(10, 20);
  expect(p2.x).toBe(10);
  expect(p2.y).toBe(20);
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
test('applyWirePaths sets path d for valid connection points', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
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
    routingDirty: false,
    autoRoute: true,
    fastRouting: false,
  };
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = {
    from: 'b1',
    to: 'b2',
    points: [
      { x: 0, y: 0 },
      { x: 20, y: 0 },
    ],
    path,
    hitPath,
  };
  state.connections.push(conn);

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

  r.renderCurrentWirePaths(true);
  const d = conn.path.getAttribute('d');
  expect(typeof d).toBe('string');
  expect(d && d.length).toBeGreaterThan(0);
  expect(Array.isArray(conn.debugRenderPoints)).toBe(true);
});
//=============================================================================
test('applyWirePaths clears d when points are invalid', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
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
    routingDirty: false,
    autoRoute: true,
    fastRouting: false,
  };
  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  const conn: any = {
    from: 'b1',
    to: 'b2',
    points: [
      { x: NaN, y: 0 },
      { x: 20, y: 0 },
    ],
    path,
    hitPath,
  };
  state.connections.push(conn);

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
  r.renderCurrentWirePaths(false);
  const d = conn.path.getAttribute('d');
  expect(d === '' || d === null).toBe(true);
});
//=============================================================================
test('clientToSvg returns client coords when getScreenCTM is falsy and transforms when available', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = { blocks: new Map(), connections: [], nextId: 1 };
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

  svg.createSVGPoint = () =>
    ({ x: 100, y: 200, matrixTransform: () => ({ x: 100, y: 200 }) } as any);
  (svg as any).getScreenCTM = () => null;
  const p1 = r.clientToSvg(10, 20);
  expect(p1.x).toBe(10);
  expect(p1.y).toBe(20);

  (svg as any).getScreenCTM = () => ({ inverse: () => ({}) });
  svg.createSVGPoint = () => ({ x: 10, y: 20, matrixTransform: () => ({ x: 10, y: 20 }) } as any);
  const p2 = r.clientToSvg(10, 20);
  expect(p2.x).toBe(10);
  expect(p2.y).toBe(20);
});
//=============================================================================
