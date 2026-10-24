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
import { createInspector } from './inspector';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
test('renderInspector handles null selection and disables rotate', () => {
  const body = document.createElement('div');
  const rotateBtn = document.createElement('button');
  const inspector = createInspector({ inspectorBody: body, rotateSelectionBtn: rotateBtn });
  inspector.renderInspector(null);
  expect(body.querySelector('input[data-edit="canvasBackground"]')).not.toBeNull();
  expect(body.textContent).toContain('Canvas');
  expect(rotateBtn.disabled).toBe(true);
});
//=============================================================================
test('renderInspector connection shows from/to labels using renderer state', () => {
  const body = document.createElement('div');
  const map = new Map();
  map.set('A', { params: { name: 'Src' }, label: 'Source', type: 'src' });
  map.set('B', { params: {}, label: 'Dst', type: 'dst' });
  const renderer: any = { state: { blocks: map } };
  const inspector = createInspector({ inspectorBody: body, renderer });
  inspector.renderInspector({
    kind: 'connection',
    fromId: 'A',
    toId: 'B',
    fromType: 'src',
    toType: 'dst',
    toIndex: 0,
  });
  expect(body.innerHTML).toMatch(/From:/);
  expect(body.innerHTML).toMatch(/To:/);
  expect(body.innerHTML).toMatch(/input 1/);
});
//=============================================================================
test('constant input updates block params and calls updateBlockLabel', () => {
  const body = document.createElement('div');
  const called: any[] = [];
  const renderer: any = { updateBlockLabel: (b: any) => called.push(b) };
  const inspector = createInspector({ inspectorBody: body, renderer });
  const block: any = { type: 'constant', params: { Value: '42' } };
  inspector.renderInspector(block);
  const input = body.querySelector(
    `input[data-edit="${FieldNames.kValue}"]`
  ) as HTMLInputElement | null;
  expect(input).not.toBeNull();
  if (input) {
    input.value = '100';
    input.dispatchEvent(new Event('input', { bubbles: true }));
  }
  expect(block.params.Value).toBe('100');
  expect(called.length).toBeGreaterThanOrEqual(1);
});
//=============================================================================
test('scope inputs call renderScope and resize triggers resizeBlock', () => {
  const body = document.createElement('div');
  const rendered: any[] = [];
  const resized: any[] = [];
  const renderer: any = { resizeBlock: (b: any, w: number, h: number) => resized.push([b, w, h]) };
  const inspector = createInspector({
    inspectorBody: body,
    renderer,
    renderScope: (b: any) => rendered.push(b),
    getRuntimeSeconds: () => 5,
  });
  const block: any = { type: 'scope', params: {}, width: 200, height: 150 };
  inspector.renderInspector(block);
  const tMin = body.querySelector(
    `input[data-edit="${FieldNames.kTMin}"]`
  ) as HTMLInputElement | null;
  const widthInput = body.querySelector(
    `input[data-edit="${FieldNames.kWidth}"]`
  ) as HTMLInputElement | null;
  if (tMin) {
    tMin.value = '2';
    tMin.dispatchEvent(new Event('input', { bubbles: true }));
  }
  if (widthInput) {
    widthInput.value = '300';
    widthInput.dispatchEvent(new Event('change', { bubbles: true }));
  }
  expect(rendered.length).toBeGreaterThanOrEqual(1);
  expect(resized.length).toBeGreaterThanOrEqual(1);
});
//=============================================================================
