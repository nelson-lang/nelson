//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { makeUtilHandlers } from './core/renderer-util';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
describe('render/renderer-util focused behaviors', () => {
  test('clientToSvg returns same coords when no svg', () => {
    const ctx: any = { svg: null };
    const handlers = makeUtilHandlers(ctx);
    expect(handlers.clientToSvg(10, 20)).toEqual({ x: 10, y: 20 });
  });

  test('clientToSvg uses createSVGPoint and matrixTransform when available', () => {
    const svg: any = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.createSVGPoint = () => ({ x: 0, y: 0, matrixTransform: (m: any) => ({ x: 11, y: 22 }) });
    svg.getScreenCTM = () => ({ inverse: () => ({}) });
    const ctx: any = { svg };
    const handlers = makeUtilHandlers(ctx);
    expect(handlers.clientToSvg(1, 2)).toEqual({ x: 11, y: 22 });
  });

  test('setLoopHighlight applies and clears classes using state and DOM', () => {
    const svg: any = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const blockEl = document.createElement('g');
    blockEl.setAttribute('data-block-id', 'blk1');
    svg.appendChild(blockEl);
    const state = {
      blocks: new Map([['blk2', { group: document.createElement('g') }]]),
      connections: [{ path: document.createElement(FieldNames.kPath) }],
    };
    const ctx: any = { svg, state };
    const { setLoopHighlight } = makeUtilHandlers(ctx);
    setLoopHighlight(['blk1'], null);
    expect(blockEl.classList.contains('loop-highlight')).toBe(true);
    setLoopHighlight(null, null);
    expect(blockEl.classList.contains('loop-highlight')).toBe(false);
  });

  test('updateSelectionBox sets and clears attributes', () => {
    const node = document.createElement('rect');
    const ctx: any = {};
    const { updateSelectionBox } = makeUtilHandlers(ctx);
    updateSelectionBox(node, { left: 1, top: 2, right: 5, bottom: 8 });
    expect(node.getAttribute('x')).toBe('1');
    expect(node.getAttribute('y')).toBe('2');
    expect(node.getAttribute('width')).toBe('4');
    expect(node.getAttribute('height')).toBe('6');
    updateSelectionBox(node, null);
    expect(node.hasAttribute('x')).toBe(false);
  });

  test('clearWorkspace clears layers and preserves overlay elements', () => {
    const blockLayer = document.createElement('div');
    blockLayer.innerHTML = '<span>keep</span>';
    const wireLayer = document.createElement('div');
    wireLayer.innerHTML = '<path/>';
    const overlayLayer = document.createElement('div');
    const preserveEl = document.createElement('div');
    preserveEl.classList.add('selection-rect');
    const removeEl = document.createElement('div');
    removeEl.classList.add('tmp');
    overlayLayer.appendChild(preserveEl);
    overlayLayer.appendChild(removeEl);
    const state: any = { blocks: new Map([['a', {}]]), connections: [{}, {}] };
    const ctx: any = { blockLayer, wireLayer, overlayLayer, state };
    const { clearWorkspace } = makeUtilHandlers(ctx);
    clearWorkspace();
    expect(blockLayer.innerHTML).toBe('');
    expect(wireLayer.innerHTML).toBe('');
    expect(overlayLayer.querySelector('.selection-rect')).not.toBeNull();
    expect(overlayLayer.querySelector('.tmp')).toBeNull();
    expect(preserveEl.getAttribute('display')).toBe('none');
    expect(state.blocks.size).toBe(0);
    expect(state.connections.length).toBe(0);
  });
});
//=============================================================================
