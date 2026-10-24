//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { vi } from 'vitest';
import { makeBlockCreation } from './block/renderer-block-create';
import * as stateMod from '../app/state';
import * as templatesMod from './block/templates';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const TEST_TYPE = '__testBlockCreate__';
const makeCtx = () => ({
  state: { nextId: 1, blocks: new Map(), routingDirty: false },
  blockLayer: document.createElementNS(ns, 'g'),
  createPortCircle: () => document.createElementNS(ns, 'circle'),
  updateBlockTransform: vi.fn(),
  enableDrag: vi.fn(),
  enableSelection: vi.fn(),
  enableSubsystemOpen: vi.fn(),
  enableScopeHover: vi.fn(),
  updateConnections: vi.fn(),
});
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
  (stateMod.state as any).locked = false;
  (templatesMod.blockTemplates as any)[TEST_TYPE] = {
    width: 60,
    height: 30,
    inputs: [{ x: 0, y: 15, side: 'left', wireX: 0, wireY: 15 }],
    outputs: [{ x: 60, y: 15, side: 'right', wireX: 60, wireY: 15 }],
    defaultParams: { value: 1 },
    render: vi.fn(),
  };
});
//=============================================================================
afterEach(() => {
  delete (templatesMod.blockTemplates as any)[TEST_TYPE];
  (stateMod.state as any).locked = false;
});
//=============================================================================
describe('makeBlockCreation', () => {
  describe('createBlock', () => {
    test('returns null for unknown block type', () => {
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      const result = createBlock('no-such-type-xyz');

      expect(result).toBeNull();
    });

    test('renders an engine-native mechanism with its serialized outputs', () => {
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      const block = createBlock('planarMechanicalIsland', 0, 0, {
        id: 'island',
        inputs: 0,
        outputs: 5,
      });

      expect(block).not.toBeNull();
      expect(block.outputs).toBe(5);
      expect(block.ports.filter((port: any) => port.type === 'out')).toHaveLength(5);
      expect(block.group.textContent).toContain('Planar mechanism');
    });

    test('removes the default input from an output-only subsystem', () => {
      (templatesMod.blockTemplates as any).subsystem = {
        width: 120,
        height: 80,
        inputs: [{ x: 0, y: 40, side: 'left' }],
        outputs: [{ x: 120, y: 40, side: 'right' }],
        defaultParams: {},
        render: vi.fn(),
      };
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      const block = createBlock('subsystem', 0, 0, {
        id: 'output-only',
        params: {
          subsystem: {
            externalInputs: [],
            externalOutputs: [{ id: 'out1' }, { id: 'out2' }],
          },
        },
      });

      expect(block.ports.filter((port: any) => port.type === 'in')).toHaveLength(0);
      expect(block.ports.filter((port: any) => port.type === 'out')).toHaveLength(2);
      delete (templatesMod.blockTemplates as any).subsystem;
    });

    test('returns null when state is locked', () => {
      (stateMod.state as any).locked = true;
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      const result = createBlock(TEST_TYPE);

      expect(result).toBeNull();
    });

    test('creates block with correct type and provided id', () => {
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      const block = createBlock(TEST_TYPE, 10, 20, { id: 'b99' });

      expect(block).not.toBeNull();
      expect(block.id).toBe('b99');
      expect(block.type).toBe(TEST_TYPE);
    });

    test('creates block with correct dimensions from template', () => {
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      const block = createBlock(TEST_TYPE, 0, 0, { id: 'bDim' });

      expect(block.width).toBe(60);
      expect(block.height).toBe(30);
    });

    test('registers block in ctx.state.blocks map', () => {
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      const block = createBlock(TEST_TYPE, 0, 0, { id: 'bReg' });

      expect(ctx.state.blocks.get('bReg')).toBe(block);
    });

    test('appends group element to blockLayer', () => {
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      createBlock(TEST_TYPE, 0, 0, { id: 'bLayer' });

      expect(ctx.blockLayer.querySelector('[data-block-id="bLayer"]')).not.toBeNull();
    });

    test('merges defaultParams with provided params', () => {
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      const block = createBlock(TEST_TYPE, 0, 0, { id: 'bP', params: { value: 99 } });

      expect(block.params.value).toBe(99);
    });

    test('uses auto-incremented id when none provided', () => {
      const ctx = makeCtx();
      ctx.state.nextId = 7;
      const { createBlock } = makeBlockCreation(ctx as any);

      const block = createBlock(TEST_TYPE);

      expect(block.id).toBe('b7');
      expect(ctx.state.nextId).toBeGreaterThan(7);
    });

    test('calls template.render on the new block', () => {
      const renderFn = vi.fn();
      (templatesMod.blockTemplates as any)[TEST_TYPE].render = renderFn;
      const ctx = makeCtx();
      const { createBlock } = makeBlockCreation(ctx as any);

      createBlock(TEST_TYPE, 0, 0, { id: 'bRen' });

      expect(renderFn).toHaveBeenCalledTimes(1);
    });

    test('sets routingDirty to true after creation', () => {
      const ctx = makeCtx();
      ctx.state.routingDirty = false;
      const { createBlock } = makeBlockCreation(ctx as any);

      createBlock(TEST_TYPE, 0, 0, { id: 'bDirty' });

      expect(ctx.state.routingDirty).toBe(true);
    });
  });
});
//=============================================================================
