//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, test, expect } from 'vitest';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const modulePath = '../../../dist/render/create-renderer.js';
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
test('setLoopHighlight toggles classes on blocks and connections', async () => {
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
  };

  const group = document.createElement('g');
  group.classList.add('svg-block');
  state.blocks.set('b1', { id: 'b1', group });

  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  path.classList.add('wire');
  const conn: any = { from: 'b1', to: 'b2', path };
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

  expect(group.classList.contains('loop-highlight')).toBe(false);
  expect(path.classList.contains('loop-highlight')).toBe(false);

  r.setLoopHighlight(['b1'], [conn]);
  expect(group.classList.contains('loop-highlight')).toBe(true);
  expect(path.classList.contains('loop-highlight')).toBe(true);

  r.setLoopHighlight([], []);
  expect(group.classList.contains('loop-highlight')).toBe(false);
  expect(path.classList.contains('loop-highlight')).toBe(false);
});
//=============================================================================
test('getLastConnectionError initially null and renderCurrentWirePaths runs', async () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = { blocks: new Map(), connections: [], nextId: 1, routingDirty: false };
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

  expect(r.getLastConnectionError()).toBe(null);

  r.renderCurrentWirePaths(true);
  r.renderCurrentWirePaths(false);
});
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
test('setLoopHighlight toggles classes on blocks and connections', async () => {
  const mod = await import('../../../dist/render/create-renderer.js');
  const r = (mod as any).createRenderer;
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = { blocks: new Map(), connections: [], nextId: 1, routingDirty: false };

  const group = document.createElement('g');
  group.classList.add('svg-block');
  state.blocks.set('b1', { id: 'b1', group });

  const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
  path.classList.add('wire');
  const conn: any = { from: 'b1', to: 'b2', path };
  state.connections.push(conn);

  const modImp = await import('../../../dist/render/create-renderer.js');
  const renderer = (modImp as any).createRenderer({
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

  expect(group.classList.contains('loop-highlight')).toBe(false);
  expect(path.classList.contains('loop-highlight')).toBe(false);

  renderer.setLoopHighlight(['b1'], [conn]);
  expect(group.classList.contains('loop-highlight')).toBe(true);
  expect(path.classList.contains('loop-highlight')).toBe(true);

  renderer.setLoopHighlight([], []);
  expect(group.classList.contains('loop-highlight')).toBe(false);
  expect(path.classList.contains('loop-highlight')).toBe(false);
});
//=============================================================================
test('getLastConnectionError initially null and renderCurrentWirePaths runs', async () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElement('g');
  const wireLayer = document.createElement('g');
  const overlayLayer = document.createElement('g');
  svg.appendChild(blockLayer);
  svg.appendChild(wireLayer);
  svg.appendChild(overlayLayer);

  const state: any = { blocks: new Map(), connections: [], nextId: 1, routingDirty: false };
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

  expect(r.getLastConnectionError()).toBe(null);
  r.renderCurrentWirePaths(true);
  r.renderCurrentWirePaths(false);
});
//=============================================================================
