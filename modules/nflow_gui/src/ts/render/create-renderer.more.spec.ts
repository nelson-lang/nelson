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
describe('create-renderer additional checks', () => {
  let createRendererModule: any;
  let templates: any;

  beforeAll(async () => {
    createRendererModule = await import('../../../dist/render/create-renderer.js');
    templates = await import('../../../dist/render/block/templates.js');
  });

  test('updateBlockLabel updates constant block math and body', () => {
    document.body.innerHTML = '';
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');

    templates.blockTemplates.constant = {
      width: 60,
      height: 30,
      inputs: [],
      outputs: [],
      defaultParams: { value: 1 },
      render: (block: any) => {
        const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        rect.classList.add('nf-block');
        block.group.appendChild(rect);
        const math = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        math.classList.add('nf-constant-formula');
        const span = document.createElement('span');
        math.appendChild(span);
        block.group.appendChild(math);
      },
    };

    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });
    const block = renderer.createBlock('constant', 0, 0, { id: 'c1' });
    expect(block).not.toBeNull();
    block.params.value = -5;
    renderer.updateBlockLabel(block);
    const mathGroup = block.group.querySelector('.nf-constant-formula');
    expect(mathGroup).not.toBeNull();
    const body = block.group.querySelector('rect.nf-block');
    expect(body).not.toBeNull();

    delete templates.blockTemplates.constant;
  });

  test('renderCurrentWirePaths sets path d attribute for connection', () => {
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
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });

    state.blocks.set('b1', { id: 'b1', x: 0, y: 0, width: 10, height: 10, ports: [] });
    state.blocks.set('b2', { id: 'b2', x: 100, y: 0, width: 10, height: 10, ports: [] });
    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = {
      from: 'b1',
      to: 'b2',
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
});
//=============================================================================
describe('create-renderer additional checks', () => {
  let createRendererModule: any;
  let templates: any;

  beforeAll(async () => {
    createRendererModule = await import('../../../dist/render/create-renderer.js');
    templates = await import('../../../dist/render/block/templates.js');
  });

  test('updateBlockLabel updates constant block math and body', () => {
    document.body.innerHTML = '';
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');

    templates.blockTemplates.constant = {
      width: 60,
      height: 30,
      inputs: [],
      outputs: [],
      defaultParams: { value: 1 },
      render: (block: any) => {
        const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        rect.classList.add('nf-block');
        block.group.appendChild(rect);
        const math = document.createElementNS('http://www.w3.org/2000/svg', 'g');
        math.classList.add('nf-constant-formula');
        const span = document.createElement('span');
        math.appendChild(span);
        block.group.appendChild(math);
      },
    };

    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });
    const block = renderer.createBlock('constant', 0, 0, { id: 'c1' });
    expect(block).not.toBeNull();
    block.params.value = -5;
    renderer.updateBlockLabel(block);
    const mathGroup = block.group.querySelector('.nf-constant-formula');
    expect(mathGroup).not.toBeNull();
    const body = block.group.querySelector('rect.nf-block');
    expect(body).not.toBeNull();

    delete templates.blockTemplates.constant;
  });

  test('renderCurrentWirePaths sets path d attribute for connection', () => {
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
    };
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
    });

    state.blocks.set('b1', { id: 'b1', x: 0, y: 0, width: 10, height: 10, ports: [] });
    state.blocks.set('b2', { id: 'b2', x: 100, y: 0, width: 10, height: 10, ports: [] });
    const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const hitPath = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    const conn: any = {
      from: 'b1',
      to: 'b2',
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
});
//=============================================================================
