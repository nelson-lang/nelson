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
import { makeBlockTransformHandlers } from './block/renderer-block-transform';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
describe('updateBlockTransform', () => {
  test('sets transform attribute with translation, scale and rotation', () => {
    const group = document.createElementNS(ns, 'g');
    const block: any = { x: 5, y: 10, width: 40, height: 20, rotation: 30, mirrored: true, group };
    const { updateBlockTransform } = makeBlockTransformHandlers({} as any);
    updateBlockTransform(block);
    expect(group.getAttribute('transform')).toContain('translate(5, 10)');
  });

  test('sets data-rotation attribute', () => {
    const group = document.createElementNS(ns, 'g');
    const block: any = { x: 0, y: 0, width: 40, height: 20, rotation: 45, group };
    const { updateBlockTransform } = makeBlockTransformHandlers({} as any);
    updateBlockTransform(block);
    expect(group.getAttribute('data-rotation')).toBe('45');
  });

  test('sets data-mirrored attribute', () => {
    const group = document.createElementNS(ns, 'g');
    const block: any = { x: 0, y: 0, width: 40, height: 20, rotation: 0, mirrored: true, group };
    const { updateBlockTransform } = makeBlockTransformHandlers({} as any);
    updateBlockTransform(block);
    expect(group.getAttribute('data-mirrored')).toBe('true');
  });

  test('scale is -1 when mirrored, 1 when not', () => {
    const group = document.createElementNS(ns, 'g');
    const block: any = { x: 0, y: 0, width: 40, height: 20, rotation: 0, mirrored: false, group };
    const { updateBlockTransform } = makeBlockTransformHandlers({} as any);
    updateBlockTransform(block);
    expect(group.getAttribute('transform')).toContain('scale(1, 1)');
  });
});
//=============================================================================
describe('resizeBlock', () => {
  test('updates width/height and marks state dirty for scope type', () => {
    const updateScopeLayout = vi.fn();
    const updateConnections = vi.fn();
    const state: any = { routingDirty: false, dirtyBlocks: new Set() };
    const ctx: any = { updateScopeLayout, updateConnections, state };
    const { resizeBlock } = makeBlockTransformHandlers(ctx);
    const block: any = { id: 'sc1', type: 'scope', width: 10, height: 10 };
    resizeBlock(block, 200, 150);
    expect(block.width).toBe(200);
    expect(block.height).toBe(150);
    expect(state.routingDirty).toBe(true);
    expect(state.dirtyBlocks.has('sc1')).toBe(true);
    expect(updateScopeLayout).toHaveBeenCalledWith(block);
  });

  test('calls updateCommentLayout for comment type', () => {
    const updateCommentLayout = vi.fn();
    const updateConnections = vi.fn();
    const state: any = { routingDirty: false, dirtyBlocks: new Set() };
    const ctx: any = { updateCommentLayout, updateConnections, state };
    const { resizeBlock } = makeBlockTransformHandlers(ctx);
    const block: any = { id: 'c1', type: 'comment', width: 10, height: 10 };
    resizeBlock(block, 80, 90);
    expect(updateCommentLayout).toHaveBeenCalledWith(block);
  });

  test('returns without changes for non-resizable block type', () => {
    const updateConnections = vi.fn();
    const state: any = { routingDirty: false, dirtyBlocks: new Set() };
    const ctx: any = { updateConnections, state };
    const { resizeBlock } = makeBlockTransformHandlers(ctx);
    const block: any = { id: 'g1', type: FieldNames.kGain, width: 50, height: 50 };
    resizeBlock(block, 100, 100);
    expect(block.width).toBe(50);
    expect(state.routingDirty).toBe(false);
  });
});
//=============================================================================
describe('applyUserFuncSize', () => {
  test('updates body width/height and port positions', () => {
    const body = document.createElementNS(ns, 'rect');
    body.classList.add('nf-block');
    const group = document.createElementNS(ns, 'g');
    group.appendChild(body);
    const portIn = { type: 'in', index: 0 };
    const portOut = { type: 'out', index: 0 };
    const updatePortElement = vi.fn();
    const updateConnections = vi.fn();
    const state: any = { routingDirty: false };
    const ctx: any = { updatePortElement, updateConnections, state };
    const { applyUserFuncSize } = makeBlockTransformHandlers(ctx);
    const block: any = {
      id: 'u1',
      group,
      ports: [portIn, portOut],
      dragRect: document.createElementNS(ns, 'rect'),
      width: 10,
      height: 10,
    };
    applyUserFuncSize(block, 120, 60, { force: true });
    expect(body.getAttribute('width')).toBe('120');
    expect(body.getAttribute('height')).toBe('60');
    expect(block.dynamicInputs[0].y).toBe(30);
    expect(state.routingDirty).toBe(true);
    expect(updatePortElement).toHaveBeenCalled();
  });

  test('no-ops when dimensions are same and force is false', () => {
    const updateConnections = vi.fn();
    const state: any = { routingDirty: false };
    const ctx: any = { updateConnections, state };
    const { applyUserFuncSize } = makeBlockTransformHandlers(ctx);
    const block: any = {
      id: 'u2',
      group: document.createElementNS(ns, 'g'),
      ports: [],
      width: 80,
      height: 40,
    };
    applyUserFuncSize(block, 80, 40, { force: false });
    expect(state.routingDirty).toBe(false);
  });
});
//=============================================================================
describe('rectsOverlap', () => {
  test('returns true for overlapping rects', () => {
    const { rectsOverlap } = makeBlockTransformHandlers({ state: { blocks: new Map() } } as any);
    expect(
      rectsOverlap(
        { left: 0, right: 10, top: 0, bottom: 10 },
        { left: 5, right: 15, top: 5, bottom: 15 }
      )
    ).toBe(true);
  });

  test('returns false for non-overlapping rects', () => {
    const { rectsOverlap } = makeBlockTransformHandlers({ state: { blocks: new Map() } } as any);
    expect(
      rectsOverlap(
        { left: 0, right: 10, top: 0, bottom: 10 },
        { left: 20, right: 30, top: 20, bottom: 30 }
      )
    ).toBe(false);
  });
});
//=============================================================================
describe('detectBlockOverlap', () => {
  test('returns false when no overlapping blocks', () => {
    const state: any = {
      blocks: new Map([
        ['b1', { id: 'b1', x: 0, y: 0, width: 10, height: 10 }],
        ['b2', { id: 'b2', x: 50, y: 50, width: 10, height: 10 }],
      ]),
    };
    const { detectBlockOverlap } = makeBlockTransformHandlers({ state } as any);
    const result = detectBlockOverlap(['b1']);
    expect(result).toBe(false);
  });

  test('returns false for empty ids list', () => {
    const state: any = { blocks: new Map() };
    const { detectBlockOverlap } = makeBlockTransformHandlers({ state } as any);
    expect(detectBlockOverlap([])).toBe(false);
  });
});
//=============================================================================
