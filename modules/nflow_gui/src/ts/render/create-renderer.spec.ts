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
//=============================================================================
describe('create-renderer', () => {
  let createRenderer: any;

  beforeAll(async () => {
    createRenderer = (await import('../../../dist/render/create-renderer.js')).createRenderer;
  });

  test('initializes svg defs and overlay layers', () => {
    document.body.innerHTML = '';
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    svg.appendChild(blockLayer);
    svg.appendChild(wireLayer);
    svg.appendChild(overlayLayer);

    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      pendingPort: null,
      routingDirty: false,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      autoRoute: true,
    };

    const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });
    expect(typeof renderer.createBlock).toBe('function');

    const defs = svg.querySelector('defs');
    expect(defs).not.toBeNull();
    expect(defs!.querySelector('#wire-arrow')).not.toBeNull();

    expect(overlayLayer.querySelector('.selection-rect')).not.toBeNull();
    expect(overlayLayer.querySelector('.selection-marquee')).not.toBeNull();
    expect(overlayLayer.querySelector('.wire-junction-layer')).not.toBeNull();
    expect(overlayLayer.querySelector('.wire-corner-layer')).not.toBeNull();
    expect(overlayLayer.querySelector('.selection-rect')!.getAttribute('display')).toBe('none');
  });

  test('clearWorkspace clears layers and state', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    svg.appendChild(blockLayer);
    svg.appendChild(wireLayer);
    svg.appendChild(overlayLayer);

    const state: any = {
      blocks: new Map([['b1', {}]]),
      connections: [{ from: 'b1' }],
      nextId: 2,
      pendingPort: null,
      routingDirty: true,
      dirtyBlocks: new Set(['b1']),
      dirtyConnections: new Set([{ from: 'b1' }]),
      selectedIds: new Set(['b1']),
      selectedConnections: new Set(),
      deleteMode: true,
    };

    const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });
    renderer.clearWorkspace();
    expect(state.blocks.size).toBe(0);
    expect(state.connections.length).toBe(0);
    expect(blockLayer.innerHTML).toBe('');
    expect(wireLayer.innerHTML).toBe('');
    expect(overlayLayer.querySelector('.selection-rect')!.getAttribute('display')).toBe('none');
  });

  test('createBlock returns null for unknown type', () => {
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
    const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });
    expect(renderer.createBlock('no-such-type')).toBeNull();
  });
});
//=============================================================================
describe('create-renderer', () => {
  let createRenderer: any;

  beforeAll(async () => {
    createRenderer = (await import('../../../dist/render/create-renderer.js')).createRenderer;
  });

  test('initializes svg defs and overlay layers', () => {
    document.body.innerHTML = '';
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    svg.appendChild(blockLayer);
    svg.appendChild(wireLayer);
    svg.appendChild(overlayLayer);

    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      pendingPort: null,
      routingDirty: false,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      autoRoute: true,
    };

    const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });
    expect(typeof renderer.createBlock).toBe('function');

    const defs = svg.querySelector('defs');
    expect(defs).not.toBeNull();
    expect(defs!.querySelector('#wire-arrow')).not.toBeNull();

    expect(overlayLayer.querySelector('.selection-rect')).not.toBeNull();
    expect(overlayLayer.querySelector('.selection-marquee')).not.toBeNull();
    expect(overlayLayer.querySelector('.wire-junction-layer')).not.toBeNull();
    expect(overlayLayer.querySelector('.wire-corner-layer')).not.toBeNull();
    expect(overlayLayer.querySelector('.selection-rect')!.getAttribute('display')).toBe('none');
  });

  test('clearWorkspace clears layers and state', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    svg.appendChild(blockLayer);
    svg.appendChild(wireLayer);
    svg.appendChild(overlayLayer);

    const state: any = {
      blocks: new Map([['b1', {}]]),
      connections: [{ from: 'b1' }],
      nextId: 2,
      pendingPort: null,
      routingDirty: true,
      dirtyBlocks: new Set(['b1']),
      dirtyConnections: new Set([{ from: 'b1' }]),
      selectedIds: new Set(['b1']),
      selectedConnections: new Set(),
      deleteMode: true,
    };

    const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });
    renderer.clearWorkspace();
    expect(state.blocks.size).toBe(0);
    expect(state.connections.length).toBe(0);
    expect(blockLayer.innerHTML).toBe('');
    expect(wireLayer.innerHTML).toBe('');
    expect(overlayLayer.querySelector('.selection-rect')!.getAttribute('display')).toBe('none');
  });

  test('createBlock returns null for unknown type', () => {
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
    const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });
    expect(renderer.createBlock('no-such-type')).toBeNull();
  });
});
//=============================================================================
