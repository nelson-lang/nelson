//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect, vi } from 'vitest';
//=============================================================================
test('createRenderer create/delete block and connection lifecycle', async () => {
  // mock templates by mutating the real templates module before creating renderer
  const templates = await import('../../../dist/render/block/templates.js');
  templates.blockTemplates.testblock = {
    width: 60,
    height: 40,
    inputs: [],
    outputs: [],
    defaultParams: {},
    render: (block: any) => {
      const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
      rect.classList.add('nf-block');
      rect.setAttribute('width', String(block.width));
      rect.setAttribute('height', String(block.height));
      block.group.appendChild(rect);
    },
  };

  const mod = await import('../../../dist/render/create-renderer.js');
  const { createRenderer } = mod as any;

  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  document.body.appendChild(svg);

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

  const r = createRenderer({ svg, blockLayer, wireLayer, overlayLayer, state });

  const b1 = r.createBlock('testblock', 10, 10, { id: 'b1' });
  const b2 = r.createBlock('testblock', 200, 10, { id: 'b2' });

  expect(b1).toBeTruthy();
  expect(b2).toBeTruthy();
  expect(state.blocks.has('b1')).toBe(true);
  expect(state.blocks.has('b2')).toBe(true);

  const conn = r.createConnection('b1', 'b2', 0, 0);
  expect(conn).not.toBeNull();
  expect(state.connections.length).toBe(1);

  conn.points = [
    { x: 50, y: 30 },
    { x: 150, y: 30 },
  ];
  const near = r.findNearestConnection({ x: 100, y: 30 }, 20);
  expect(near).toBe(conn);

  r.deleteConnection(conn);
  expect(state.connections.length).toBe(0);

  r.deleteBlock('b1');
  expect(state.blocks.has('b1')).toBe(false);
});
//=============================================================================
