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
//=============================================================================
test('createRenderer sets up overlay children and wire-arrow marker', async () => {
  const { createRenderer } = await import('../../../dist/render/create-renderer.js');

  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.setAttribute('width', '800');
  svg.setAttribute('height', '600');
  const defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
  svg.appendChild(defs);

  const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    routingDirty: false,
    dirtyBlocks: new Set(),
    dirtyConnections: new Set(),
    selectedConnection: null,
    autoRoute: false,
  };

  const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });
  expect(typeof renderer).toBe('object');

  const sel = overlayLayer.querySelector('.selection-rect');
  const marquee = overlayLayer.querySelector('.selection-marquee');
  expect(sel).not.toBeNull();
  expect(marquee).not.toBeNull();

  const wireArrow = svg.querySelector('#wire-arrow');
  expect(wireArrow).not.toBeNull();
});
//=============================================================================
test('createRenderer sets up overlay children and wire-arrow marker', async () => {
  const { createRenderer } = await import('../../../dist/render/create-renderer.js');

  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.setAttribute('width', '800');
  svg.setAttribute('height', '600');
  const defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
  svg.appendChild(defs);

  const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');

  const state: any = {
    blocks: new Map(),
    connections: [],
    nextId: 1,
    routingDirty: false,
    dirtyBlocks: new Set(),
    dirtyConnections: new Set(),
    selectedConnection: null,
    autoRoute: false,
  };

  const renderer = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });
  expect(typeof renderer).toBe('object');

  const sel = overlayLayer.querySelector('.selection-rect');
  const marquee = overlayLayer.querySelector('.selection-marquee');
  expect(sel).not.toBeNull();
  expect(marquee).not.toBeNull();

  const wireArrow = svg.querySelector('#wire-arrow');
  expect(wireArrow).not.toBeNull();
});
//=============================================================================
