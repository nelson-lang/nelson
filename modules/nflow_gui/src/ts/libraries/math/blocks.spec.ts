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
import mathManifest from '../../../../../nflow_blocks/libraries/math/library.json';
import { buildRegistryFromManifest } from '../../libraries/generic-block-renderer.js';
//=============================================================================
const registry = buildRegistryFromManifest(mathManifest as any);
const renderers: Record<string, any> = Object.fromEntries(
  Object.entries(registry.getDefinitions()).map(([k, v]) => [k, (v as any)?.render])
);
//=============================================================================
describe('math blocks registry', () => {
  test('exports expected renderer names', () => {
    expect(typeof renderers).toBe('object');
    const keys = Object.keys(renderers).sort();
    expect(keys).toEqual(
      [
        'abs',
        'atan2',
        'bias',
        'complexToMagnitudeAngle',
        'complexToRealImag',
        'conjugate',
        'crossProduct',
        'divide',
        'dotProduct',
        'gain',
        'magnitudeAngleToComplex',
        'mathFunction',
        'matmul',
        'max',
        'min',
        'mult',
        'negate',
        'polynomial',
        'productOfElements',
        'realImagToComplex',
        'roundingFunction',
        'sign',
        'sqrt',
        'sum',
        'sumElements',
        'trigFunction',
        'wrapToZero',
      ].sort()
    );
    keys.forEach((k) => expect(typeof (renderers as any)[k]).toBe('function'));
  });
});
//=============================================================================
test('loads blocks.ts if present or registry contains math blocks', async () => {
  const defs = registry.getDefinitions();
  const hasAny = defs['abs'] || defs[FieldNames.kGain] || defs['mult'];
  expect(hasAny).toBeDefined();
});
//=============================================================================
