//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import lookupManifest from '../../../../../nflow_blocks/libraries/lookup/library.json';
import { buildRegistryFromManifest } from '../../libraries/generic-block-renderer.js';
import { lookupNDResizer, directLookupResizer, blockDefinitions } from './blocks';
//=============================================================================
const registry = buildRegistryFromManifest(lookupManifest as any);
describe('lookup blocks registry', () => {
  test('exposes the lookup renderers', () => {
    const keys = Object.keys(registry.getDefinitions()).sort();
    expect(keys).toEqual([
      'directLookup',
      'interpolationPrelookup',
      'lookup1D',
      'lookup2D',
      'lookupDynamic',
      'lookupND',
      'prelookup',
    ]);
  });

  test('blockDefinitions expose resizers and preserve manifest renderers', () => {
    expect(typeof blockDefinitions.lookupND.resize).toBe('function');
    expect(typeof blockDefinitions.directLookup.resize).toBe('function');
    expect(typeof blockDefinitions.lookupND.render).toBe('function');
    expect(typeof blockDefinitions.directLookup.render).toBe('function');
  });
});
//=============================================================================
describe('lookup dimension resizer', () => {
  test.each([
    [1, 1],
    [2, 2],
    [3, 3],
    [4, 4],
  ])('NumberOfTableDimensions %i -> %i input ports', (n, expected) => {
    const probe: any = { params: { NumberOfTableDimensions: n } };
    lookupNDResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(expected);
    expect(probe.dynamicOutputs).toHaveLength(1);
    probe.dynamicInputs.forEach((p: any) => expect(p.side).toBe('left'));
  });

  test('directLookupResizer falls back to the default when the count is invalid', () => {
    const probe: any = { params: { NumberOfTableDimensions: 0 } };
    directLookupResizer(probe);
    expect(probe.dynamicInputs.length).toBeGreaterThanOrEqual(1);
  });

  test('directLookupResizer honors an explicit dimension count', () => {
    const probe: any = { params: { NumberOfTableDimensions: 3 } };
    directLookupResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(3);
  });

  test('falls back to 2 inputs when params absent', () => {
    const probe: any = {};
    lookupNDResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(2);
  });
});
//=============================================================================
