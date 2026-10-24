//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { trigFunctionResizer, mathFunctionResizer, blockDefinitions } from './blocks';
//=============================================================================
describe('trigFunctionResizer', () => {
  test('unary function: 1 input, 1 output', () => {
    const probe: any = { params: { Function: 'sin' } };
    trigFunctionResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(1);
    expect(probe.dynamicOutputs).toHaveLength(1);
  });

  test('atan2: 2 inputs, 1 output', () => {
    const probe: any = { params: { Function: 'atan2' } };
    trigFunctionResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(2);
    expect(probe.dynamicOutputs).toHaveLength(1);
    probe.dynamicInputs.forEach((p: any) => expect(p.side).toBe('left'));
  });

  test('sincos: 1 input, 2 outputs', () => {
    const probe: any = { params: { Function: 'sincos' } };
    trigFunctionResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(1);
    expect(probe.dynamicOutputs).toHaveLength(2);
    probe.dynamicOutputs.forEach((p: any) => expect(p.side).toBe('right'));
  });

  test('falls back to unary when params absent', () => {
    const probe: any = {};
    trigFunctionResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(1);
    expect(probe.dynamicOutputs).toHaveLength(1);
  });
});
//=============================================================================
describe('mathFunctionResizer', () => {
  test.each(['exp', 'log', '10^u', 'square', 'sqrt', 'reciprocal'])(
    'unary function %s: 1 input',
    (fn) => {
      const probe: any = { params: { Function: fn } };
      mathFunctionResizer(probe);
      expect(probe.dynamicInputs).toHaveLength(1);
      expect(probe.dynamicOutputs).toHaveLength(1);
    }
  );

  test.each(['pow', 'hypot', 'rem', 'mod'])('binary function %s: 2 inputs', (fn) => {
    const probe: any = { params: { Function: fn } };
    mathFunctionResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(2);
    expect(probe.dynamicOutputs).toHaveLength(1);
  });

  test('falls back to unary when params absent', () => {
    const probe: any = {};
    mathFunctionResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(1);
  });
});
//=============================================================================
describe('math blockDefinitions', () => {
  test('exports resizers and preserves manifest renderers', () => {
    expect(typeof blockDefinitions.trigFunction.resize).toBe('function');
    expect(typeof blockDefinitions.mathFunction.resize).toBe('function');
    expect(typeof blockDefinitions.trigFunction.render).toBe('function');
    expect(typeof blockDefinitions.mathFunction.render).toBe('function');
  });
});
//=============================================================================
