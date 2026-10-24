//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { initCtrlDragDuplicate, setRendererRef, wasRecentDuplicate } from './dup-drag.js';
import { state } from './state.js';
import { _resetForTests } from './status-service.js';
//=============================================================================
function makeSvgWithBlock(): { svg: SVGSVGElement; group: SVGGElement } {
  document.body.innerHTML = `
    <svg id="svgCanvas">
      <g id="wireLayer"></g>
      <g id="blockLayer"><g class="svg-block" data-block-id="b1"></g></g>
      <g id="overlayLayer"></g>
    </svg>`;
  return {
    svg: document.getElementById('svgCanvas') as unknown as SVGSVGElement,
    group: document.querySelector('#blockLayer g.svg-block') as SVGGElement,
  };
}
//=============================================================================
beforeEach(() => {
  _resetForTests();
  state.blocks.clear();
  state.connections.length = 0;
  state.locked = false;
  state.blocks.set(
    'b1' as never,
    {
      id: 'b1',
      type: 'gain',
      x: 100,
      y: 80,
      params: { Gain: 3 },
    } as never
  );
});
//=============================================================================
test('Ctrl+drag past the threshold duplicates the block at its source position', () => {
  const { svg, group } = makeSvgWithBlock();
  const createBlock = vi.fn(() => ({ id: 'b2' }));
  const updateConnections = vi.fn();
  setRendererRef({ current: { createBlock, updateConnections } });
  initCtrlDragDuplicate(svg);

  group.dispatchEvent(
    new PointerEvent('pointerdown', { bubbles: true, ctrlKey: true, clientX: 10, clientY: 10 })
  );
  window.dispatchEvent(new PointerEvent('pointermove', { clientX: 30, clientY: 30 }));
  window.dispatchEvent(new PointerEvent('pointerup', {}));

  expect(createBlock).toHaveBeenCalledTimes(1);
  const [type, x, y, options] = createBlock.mock.calls[0] as unknown as [
    string,
    number,
    number,
    { params: { gain: number } }
  ];
  expect(type).toBe('gain');
  expect(x).toBe(100);
  expect(y).toBe(80);
  expect(options.params.Gain).toBe(3);
  expect(wasRecentDuplicate()).toBe(true);
});
//=============================================================================
test('a Ctrl+click without movement does not duplicate', () => {
  const { svg, group } = makeSvgWithBlock();
  const createBlock = vi.fn(() => ({ id: 'b2' }));
  setRendererRef({ current: { createBlock } });
  initCtrlDragDuplicate(svg);

  group.dispatchEvent(
    new PointerEvent('pointerdown', { bubbles: true, ctrlKey: true, clientX: 10, clientY: 10 })
  );
  window.dispatchEvent(new PointerEvent('pointermove', { clientX: 11, clientY: 11 }));
  window.dispatchEvent(new PointerEvent('pointerup', {}));
  expect(createBlock).not.toHaveBeenCalled();
});
//=============================================================================
test('drag without Ctrl never duplicates', () => {
  const { svg, group } = makeSvgWithBlock();
  const createBlock = vi.fn(() => ({ id: 'b2' }));
  setRendererRef({ current: { createBlock } });
  initCtrlDragDuplicate(svg);

  group.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true, clientX: 10, clientY: 10 }));
  window.dispatchEvent(new PointerEvent('pointermove', { clientX: 60, clientY: 60 }));
  window.dispatchEvent(new PointerEvent('pointerup', {}));
  expect(createBlock).not.toHaveBeenCalled();
});
//=============================================================================
