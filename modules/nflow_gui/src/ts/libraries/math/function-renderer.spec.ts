//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { functionRenderer, trigFunctionResizer, mathFunctionResizer } from './blocks';
import { applyBlockTypeLabels } from '../../render/block/renderer-block-label-types';
import { desiredPortCounts } from '../../blocks/port-rebuild';
const ns = 'http://www.w3.org/2000/svg';
function helpers() {
  return {
    createSvgElement: (tag: string, attrs: Record<string, unknown>, content?: string) => {
      const el = document.createElementNS(ns, tag);
      Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
      if (content) el.textContent = content;
      return el;
    },
  };
}
test('function blocks display the configured function and update on edit', () => {
  const group = document.createElementNS(ns, 'g');
  const block: any = {
    type: 'mathFunction',
    width: 80,
    height: 80,
    params: { Function: 'square' },
    group,
  };
  functionRenderer(block, helpers());
  expect(group.querySelector('.nf-function-symbol')?.textContent).toBe('u²');
  block.params.Function = 'sqrt';
  applyBlockTypeLabels(block, {});
  expect(group.querySelector('.nf-function-symbol')?.textContent).toBe('√u');
  const trig: any = {
    type: 'trigFunction',
    width: 80,
    height: 80,
    params: { Function: 'sin' },
    group: document.createElementNS(ns, 'g'),
  };
  functionRenderer(trig, helpers());
  expect(trig.group.textContent).toBe('sin');
  trig.params.Function = 'cos';
  applyBlockTypeLabels(trig, {});
  expect(trig.group.textContent).toBe('cos');
});
test('function selection and actual port counts remain consistent', () => {
  const trig: any = { type: 'trigFunction', params: { Function: 'sincos' } };
  trigFunctionResizer(trig);
  expect(desiredPortCounts(trig)).toEqual({
    inputs: trig.dynamicInputs.length,
    outputs: trig.dynamicOutputs.length,
  });
  const math: any = { type: 'mathFunction', params: { Function: 'pow' } };
  mathFunctionResizer(math);
  expect(desiredPortCounts(math)).toEqual({
    inputs: math.dynamicInputs.length,
    outputs: math.dynamicOutputs.length,
  });
});
