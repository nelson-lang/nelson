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
import mathManifest from '../../../../../nflow_blocks/libraries/math/library.json';
import { buildRegistryFromManifest } from '../generic-block-renderer.js';
//=============================================================================
const BLOCK_SIZE = 80;
// Two-input math functions (second operand on input port 1).
const MATH_BINARY = new Set(['pow', 'hypot', 'rem', 'mod']);
const oneInput = () => [{ x: 0, y: BLOCK_SIZE / 2, side: 'left' }];
const twoInputs = () => [
  { x: 0, y: BLOCK_SIZE / 2 - 10, side: 'left' },
  { x: 0, y: BLOCK_SIZE / 2 + 10, side: 'left' },
];
//=============================================================================
const oneOutput = () => [{ x: BLOCK_SIZE, y: BLOCK_SIZE / 2, side: 'right' }];
const twoOutputs = () => [
  { x: BLOCK_SIZE, y: BLOCK_SIZE / 2 - 10, side: 'right' },
  { x: BLOCK_SIZE, y: BLOCK_SIZE / 2 + 10, side: 'right' },
];
//=============================================================================
export function trigFunctionResizer(blockDef: any): void {
  const fn = String(blockDef.params?.Function ?? 'sin');
  blockDef.width = BLOCK_SIZE;
  blockDef.height = BLOCK_SIZE;
  if (fn === 'atan2') {
    blockDef.dynamicInputs = twoInputs();
    blockDef.dynamicOutputs = oneOutput();
  } else if (fn === 'sincos') {
    blockDef.dynamicInputs = oneInput();
    blockDef.dynamicOutputs = twoOutputs();
  } else {
    blockDef.dynamicInputs = oneInput();
    blockDef.dynamicOutputs = oneOutput();
  }
}
//=============================================================================
export function mathFunctionResizer(blockDef: any): void {
  const fn = String(blockDef.params?.Function ?? 'exp');
  blockDef.width = BLOCK_SIZE;
  blockDef.height = BLOCK_SIZE;
  blockDef.dynamicInputs = MATH_BINARY.has(fn) ? twoInputs() : oneInput();
  blockDef.dynamicOutputs = oneOutput();
}
//=============================================================================
const manifestDefs = buildRegistryFromManifest(mathManifest as any).getDefinitions();
export const blockDefinitions = {
  trigFunction: {
    render: manifestDefs['trigFunction']?.render,
    resize: trigFunctionResizer,
  },
  mathFunction: {
    render: manifestDefs['mathFunction']?.render,
    resize: mathFunctionResizer,
  },
};
//=============================================================================
