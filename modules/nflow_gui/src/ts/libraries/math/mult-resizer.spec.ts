//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { multResizer, multRenderer } from './blocks';
import { desiredPortCounts } from '../../blocks/port-rebuild';
test('product renders all configured input ports', () => {
  const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const block: any = { type: 'mult', params: { Inputs: 5 }, group };
  multResizer(block);
  multRenderer(block, {
    createSvgElement: (tag: string, attrs: Record<string, string>, content?: string) => {
      const el = document.createElementNS('http://www.w3.org/2000/svg', tag);
      Object.entries(attrs).forEach(([key, value]) => el.setAttribute(key, String(value)));
      if (content) el.textContent = content;
      return el;
    },
  });
  expect(block.dynamicInputs).toHaveLength(5);
  expect(group.querySelectorAll('text')).toHaveLength(5);
  expect(desiredPortCounts(block)).toEqual({ inputs: 5, outputs: 1 });
});

test('product shows the operation for each connected input', () => {
  const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const block: any = { type: 'mult', params: { Inputs: '*/*' }, group };
  multResizer(block);
  multRenderer(block, {
    createSvgElement: (tag: string, attrs: Record<string, string>, content?: string) => {
      const el = document.createElementNS('http://www.w3.org/2000/svg', tag);
      Object.entries(attrs).forEach(([key, value]) => el.setAttribute(key, String(value)));
      if (content) el.textContent = content;
      return el;
    },
  });
  expect(block.dynamicInputs).toHaveLength(3);
  expect(Array.from(group.querySelectorAll('text')).map((el) => el.textContent)).toEqual([
    '×',
    '÷',
    '×',
  ]);
  expect(desiredPortCounts(block)).toEqual({ inputs: 3, outputs: 1 });
});

test('matrix product supports a single inverse input', () => {
  const block: any = { type: 'mult', params: { Inputs: '/', Multiplication: 'Matrix(*)' } };
  multResizer(block);
  expect(block.dynamicInputs).toHaveLength(1);
  expect(desiredPortCounts(block)).toEqual({ inputs: 1, outputs: 1 });
});
