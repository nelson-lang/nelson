//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Non-regression: ensure subsystem external ports are synthesized
// into visible block ports when loading a diagram.
//=============================================================================
import { describe, beforeAll, test, expect } from 'vitest';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
describe('subsystem ports synthesis', () => {
  let createRendererModule: any;
  let templates: any;

  beforeAll(async () => {
    createRendererModule = await import('../../../dist/render/create-renderer.js');
    templates = await import('../../../dist/render/block/templates.js');
  });

  test('createBlock synthesizes externalInputs/externalOutputs as visible ports', () => {
    document.body.innerHTML = '';
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const wireLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const overlayLayer = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    svg.appendChild(blockLayer);
    svg.appendChild(wireLayer);
    svg.appendChild(overlayLayer);

    // minimal template for subsystem so createBlock can render
    templates.blockTemplates.subsystem = {
      width: 100,
      height: 60,
      inputs: [],
      outputs: [],
      defaultParams: {},
      render: (block: any) => {
        const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
        rect.classList.add('nf-block');
        rect.setAttribute('x', '0');
        rect.setAttribute('y', '0');
        rect.setAttribute('width', String(block.width));
        rect.setAttribute('height', String(block.height));
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

    const renderer = createRendererModule.createRenderer({
      svg,
      blockLayer,
      wireLayer,
      overlayLayer,
      state,
      onConnectionError: () => {},
    });

    const subspec = {
      externalInputs: [{ id: 'b10' }, { id: 'b7' }, { id: 'b9' }],
      externalOutputs: [{ id: 'b6' }, { id: 'b33' }, { id: 'b34' }],
    };

    const blk = renderer.createBlock(FieldNames.kSubsystem, 10, 10, {
      id: 'b_sub',
      params: { subsystem: subspec, width: 220, height: 160 },
    });

    expect(blk).not.toBeNull();
    expect((blk as any).width).toBe(220);
    expect((blk as any).height).toBe(160);
    const ports = (blk as any).ports || [];
    const ins = ports.filter((p: any) => p.type === 'in');
    const outs = ports.filter((p: any) => p.type === 'out');
    expect(ins.length).toBe(3);
    expect(outs.length).toBe(3);

    delete templates.blockTemplates.subsystem;
  });
});
//=============================================================================
