//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, test } from 'vitest';
import { makeUtilHandlers } from './core/renderer-util.js';
//=============================================================================
describe('renderer-util handlers', () => {
  test('updateSelectionBox sets attributes', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const node = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    svg.appendChild(node);
    const handlers = makeUtilHandlers({ svg } as any);
    handlers.updateSelectionBox(node as any, { left: 1, top: 2, right: 11, bottom: 22 } as any);
    expect(node.getAttribute('x')).toBe('1');
    expect(node.getAttribute('y')).toBe('2');
    expect(node.getAttribute('width')).toBe('10');
    expect(node.getAttribute('height')).toBe('20');
  });

  test('clearWorkspace clears layers and state safely', () => {
    const blockLayer = document.createElement('div');
    blockLayer.innerHTML = '<span>x</span>';
    const wireLayer = document.createElement('div');
    wireLayer.innerHTML = '<path/>';
    const overlay = document.createElement('div');
    const sel = document.createElement('div');
    sel.classList.add('selection-rect');
    overlay.appendChild(sel);
    const state: any = {
      blocks: new Map([['b', { group: document.createElement('div') }]]),
      connections: [{ path: document.createElement('div') }],
    };
    const handlers = makeUtilHandlers({
      svg: document.createElementNS('http://www.w3.org/2000/svg', 'svg'),
      blockLayer,
      wireLayer,
      overlay,
      state,
    } as any);
    handlers.clearWorkspace();
    expect(blockLayer.innerHTML).toBe('');
    expect(wireLayer.innerHTML).toBe('');
    expect(state.blocks.size).toBe(0);
    expect(state.connections.length).toBe(0);
  });

  test('setLoopHighlight applies and clears loop-highlight class', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    const el = document.createElement('g');
    el.setAttribute('data-block-id', 'b1');
    svg.appendChild(el);
    const blkGroup = document.createElement('g');
    const state: any = { blocks: new Map([['b1', { group: blkGroup }]]), connections: [] };
    const handlers = makeUtilHandlers({ svg, state } as any);
    handlers.setLoopHighlight(['b1'], []);
    expect(blkGroup.classList.contains('loop-highlight')).toBe(true);
    handlers.setLoopHighlight(null, null);
    expect(blkGroup.classList.contains('loop-highlight')).toBe(false);
  });
});
//=============================================================================
