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
//=============================================================================
describe('Nelson workspace / function blocks are registered', () => {
  const defs = builtinRegistry.getDefinitions();

  it('exposes nelsonFunction, fromWorkspace and toWorkspace', () => {
    expect(defs).toHaveProperty('nelsonFunction');
    expect(defs).toHaveProperty('fromWorkspace');
    expect(defs).toHaveProperty('toWorkspace');
  });

  it('nelsonFunction has one input and one output', () => {
    const b = defs['nelsonFunction'];
    expect(b.inputs ?? []).toHaveLength(1);
    expect(b.outputs ?? []).toHaveLength(1);
    expect(b.defaultParams).toMatchObject({ Fcn: expect.anything() });
  });

  it('fromWorkspace is a source (no input, one output) with a variable name', () => {
    const b = defs['fromWorkspace'];
    expect(b.inputs ?? []).toHaveLength(0);
    expect(b.outputs ?? []).toHaveLength(1);
    expect(b.defaultParams).toMatchObject({ VariableName: expect.anything() });
  });

  it('toWorkspace is a sink (one input, no output) with a variable name', () => {
    const b = defs['toWorkspace'];
    expect(b.inputs ?? []).toHaveLength(1);
    expect(b.outputs ?? []).toHaveLength(0);
    expect(b.defaultParams).toMatchObject({ VariableName: expect.anything() });
  });
});
//=============================================================================
