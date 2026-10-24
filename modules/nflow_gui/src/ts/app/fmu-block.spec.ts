//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect } from 'vitest';
import builtinRegistry from './blocks-registry.js';
import fmiManifest from '../../../../nflow_blocks/libraries/fmi/library.json';
import { blockDefinitions } from '../libraries/fmi/blocks.js';
//=============================================================================
describe('FMU block registration', () => {
  it('exposes the fmu block type in the builtin registry', () => {
    const defs = builtinRegistry.getDefinitions();
    expect(defs).toHaveProperty('fmu');
  });

  it('has no inputs and a single default output port', () => {
    const fmu = builtinRegistry.getDefinitions()['fmu'];
    expect(fmu.inputs ?? []).toHaveLength(0);
    expect(fmu.outputs ?? []).toHaveLength(1);
  });

  it('defaults to an empty FMU path parameter', () => {
    const fmu = builtinRegistry.getDefinitions()['fmu'];
    expect(fmu.defaultParams).toMatchObject({ path: '' });
  });

  it('declares the FMI 3.0 library manifest', () => {
    expect(fmiManifest.id).toBe('builtin.fmi');
    const block = fmiManifest.blocks.find((b) => b.type === 'fmu');
    expect(block).toBeDefined();
    expect(block?.phases).toEqual(['INIT', 'OUTPUT', 'UPDATE']);
  });

  it('exposes the Model Exchange fmuMe block type', () => {
    const defs = builtinRegistry.getDefinitions();
    expect(defs).toHaveProperty('fmuMe');
  });

  it('labels the FMI modes explicitly in the library manifest', () => {
    expect(fmiManifest.blocks.find((block) => block.type === 'fmu')?.label).toBe('FMU (CS)');
    expect(fmiManifest.blocks.find((block) => block.type === 'fmuMe')?.label).toBe('FMU (ME)');
  });

  it('declares the fmuMe block with continuous-solver phases', () => {
    const block = fmiManifest.blocks.find((b) => b.type === 'fmuMe');
    expect(block).toBeDefined();
    expect(block?.phases).toEqual(['INIT', 'OUTPUT', 'DERIVATIVE', 'ZERO_CROSSING', 'EVENT']);
  });

  it('gives both fmu and fmuMe the dynamic port resizer', () => {
    // Without a resizer the block keeps the static manifest ports and ignores
    // params.numOutputs, so extra FMU outputs get no port (empty scopes).
    expect(typeof blockDefinitions.fmu?.resize).toBe('function');
    expect(typeof blockDefinitions.fmuMe?.resize).toBe('function');
    expect(blockDefinitions.fmuMe?.resize).toBe(blockDefinitions.fmu?.resize);
  });
});
//=============================================================================
