//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, beforeAll, test, expect, vi } from 'vitest';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
describe('create-renderer behavior', () => {
  let createRendererModule: any;
  let templates: any;

  beforeAll(async () => {
    createRendererModule = await import('../../../dist/render/create-renderer.js');
    templates = await import('../../../dist/render/block/templates.js');
  });

  test('createBlock and createConnection, duplicate and occupied errors, findNearestConnection, deleteConnection', () => {
    document.body.innerHTML = '';
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    svg.appendChild(blockLayer);
    svg.appendChild(wireLayer);
    svg.appendChild(overlayLayer);

    templates.blockTemplates.test = {
      width: 80,
      height: 40,
      inputs: [{ x: 0, y: 10 }],
      outputs: [{ x: 80, y: 10 }],
      defaultParams: {},
      render: (block: any) => {
        const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        rect.classList.add('nf-block');
        block.group.appendChild(rect);
      },
    };

    const state: any = {
      blocks: new Map(),
      connections: [],
      nextId: 1,
      dirtyBlocks: new Set(),
      dirtyConnections: new Set(),
      selectedIds: new Set(),
      selectedConnections: new Set(),
    };

    const onConnError = vi.fn();
    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
      onConnectionError: onConnError,
    });

    const b1 = renderer.createBlock('test', 10, 10, { id: 'b1' });
    const b2 = renderer.createBlock('test', 200, 10, { id: 'b2' });
    expect(b1).not.toBeNull();
    expect(b2).not.toBeNull();
    expect(state.blocks.has('b1')).toBe(true);

    const conn = renderer.createConnection('b1', 'b2', 0, 0);
    expect(conn).not.toBeNull();
    expect(state.connections.length).toBe(1);
    expect(conn.path.tagName.toLowerCase()).toBe('path');

    const dup = renderer.createConnection('b1', 'b2', 0, 0);
    expect(dup).toBeNull();
    const lastErr = renderer.getLastConnectionError();
    expect(lastErr).not.toBeNull();
    expect(lastErr.reason).toBe('duplicate');

    const b3 = renderer.createBlock('test', 300, 10, { id: 'b3' });
    const occ = renderer.createConnection('b3', 'b2', 0, 0);
    expect(occ).toBeNull();
    const occErr = renderer.getLastConnectionError();
    expect(occErr.reason).toBe('input_occupied');
    expect(onConnError).toHaveBeenCalled();

    conn.points = [
      { x: 10, y: 10 },
      { x: 200, y: 10 },
    ];
    const nearest = renderer.findNearestConnection({ x: 50, y: 12 }, 20);
    expect(nearest).toBe(conn);

    renderer.deleteConnection(conn);
    expect(state.connections.length).toBe(0);

    delete templates.blockTemplates.test;
  });
});
//=============================================================================
