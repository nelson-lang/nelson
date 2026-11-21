//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Dynamic-port math blocks: trigFunction and mathFunction morph their port
// count depending on the selected Function parameter. atan2 and the binary
// math functions (pow/hypot/rem/mod) take two inputs; sincos produces two
// outputs. The image renderer is reused from the manifest so the palette icon
// is preserved (the module registry overrides the manifest entry).
/* eslint-disable @typescript-eslint/no-explicit-any */
import { parseProductInputs } from '../../blocks/product-inputs.js';
import { functionLabel } from './function-label.js';
//=============================================================================
const MATH_BINARY = new Set(['pow', 'hypot', 'rem', 'mod']);
function resizeFunctionBlock(block: any, inputs: number, outputs: number): void {
  const width = Math.max(40, Number(block.params?.width) || 80);
  const height = Math.max(40, Math.max(inputs, outputs) * 16, Number(block.params?.height) || 80);
  const ports = (count: number, x: number, side: string) =>
    Array.from({ length: count }, (_, index) => ({
      x,
      y: height / 2 + (index - (count - 1) / 2) * 20,
      side,
    }));
  block.width = width;
  block.height = height;
  block.dynamicInputs = ports(inputs, 0, 'left');
  block.dynamicOutputs = ports(outputs, width, 'right');
}
//=============================================================================
export function trigFunctionResizer(block: any): void {
  const fn = String(block.params?.Function ?? 'sin');
  resizeFunctionBlock(block, fn === 'atan2' ? 2 : 1, fn === 'sincos' ? 2 : 1);
}
//=============================================================================
export function mathFunctionResizer(block: any): void {
  const fn = String(block.params?.Function ?? 'exp');
  resizeFunctionBlock(block, MATH_BINARY.has(fn) ? 2 : 1, 1);
}
//=============================================================================
function arithmeticSize(params: any, count: number): { width: number; height: number } {
  return {
    width: Math.max(20, Number(params?.width) || 30),
    height: Math.max(count * 16, Number(params?.height) || Math.max(40, count * 20)),
  };
}
//=============================================================================
export function sumResizer(blockDef: any): void {
  const raw = String(blockDef.params?.Inputs ?? '+++');
  const signs = raw.match(/[+-]/g) ?? [];
  const count = /^\d+$/.test(raw.trim()) ? Math.max(1, Number(raw)) : Math.max(1, signs.length);
  const { width, height } = arithmeticSize(blockDef.params, count);
  blockDef.width = width;
  blockDef.height = height;
  blockDef.dynamicInputs = Array.from({ length: count }, (_, i) => ({
    x: 0,
    y: ((i + 0.5) * height) / count,
    side: 'left',
  }));
  blockDef.dynamicOutputs = [{ x: width, y: height / 2, side: 'right' }];
}
//=============================================================================
export function sumRenderer(block: any, helpers: any): void {
  const make = helpers.createSvgElement;
  const raw = String(block.params?.Inputs ?? '+++');
  const signs = raw.match(/[+-]/g) ?? [];
  const count = /^\d+$/.test(raw.trim()) ? Math.max(1, Number(raw)) : Math.max(1, signs.length);
  block.group.appendChild(
    make('rect', { x: 0, y: 0, width: block.width, height: block.height, class: 'nf-block' })
  );
  for (let i = 0; i < count; i++) {
    block.group.appendChild(
      make(
        'text',
        {
          x: 5,
          y: ((i + 0.5) * block.height) / count,
          'dominant-baseline': 'middle',
          class: 'sum-sign',
          style: 'font-size: 14px; text-anchor: start',
          'data-sign-index': i,
        },
        signs[i] === '-' ? '-' : '+'
      )
    );
  }
}
//=============================================================================
export function multResizer(block: any): void {
  const count =
    parseProductInputs(block.params?.Inputs ?? 3, block.params?.Multiplication === 'Matrix(*)')
      ?.count ?? 3;
  const { width, height } = arithmeticSize(block.params, count);
  block.width = width;
  block.height = height;
  block.dynamicInputs = Array.from({ length: count }, (_, i) => ({
    x: 0,
    y: ((i + 0.5) * height) / count,
    side: 'left',
  }));
  block.dynamicOutputs = [{ x: width, y: height / 2, side: 'right' }];
}
//=============================================================================
export function multRenderer(block: any, helpers: any): void {
  const make = helpers.createSvgElement;
  const parsed = parseProductInputs(
    block.params?.Inputs ?? 3,
    block.params?.Multiplication === 'Matrix(*)'
  ) ?? { count: 3, operations: '***' };
  const count = parsed.count;
  block.group.appendChild(
    make('rect', { x: 0, y: 0, width: block.width, height: block.height, class: 'nf-block' })
  );
  for (let i = 0; i < count; i++)
    block.group.appendChild(
      make(
        'text',
        {
          x: 5,
          y: ((i + 0.5) * block.height) / count,
          'dominant-baseline': 'middle',
          class: 'nf-product-sign',
          style: 'font-size: 14px',
        },
        parsed.operations[i] === '/' ? '÷' : '×'
      )
    );
}
//=============================================================================
export function functionRenderer(block: any, helpers: any): void {
  const make = helpers.createSvgElement;
  block.group.appendChild(
    make('rect', { x: 0, y: 0, width: block.width, height: block.height, class: 'nf-block' })
  );
  block.group.appendChild(
    make(
      'text',
      {
        x: block.width / 2,
        y: block.height / 2,
        'text-anchor': 'middle',
        'dominant-baseline': 'middle',
        class: 'nf-function-symbol',
        style: 'font-size: 18px',
      },
      functionLabel(block.type, block.params?.Function)
    )
  );
}
//=============================================================================
export const blockDefinitions = {
  sum: { render: sumRenderer, resize: sumResizer },
  mult: { render: multRenderer, resize: multResizer },
  trigFunction: {
    render: functionRenderer,
    resize: trigFunctionResizer,
  },
  mathFunction: {
    render: functionRenderer,
    resize: mathFunctionResizer,
  },
};
//=============================================================================
