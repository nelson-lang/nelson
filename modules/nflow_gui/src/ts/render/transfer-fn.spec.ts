//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import {
  buildTransferTeX,
  buildPolyTeX,
  buildTransferMathML,
  buildPolyMathML,
} from './transfer-fn';
//=============================================================================
describe('buildPolyTeX', () => {
  test('empty coefficients return zero string', () => {
    expect(buildPolyTeX([])).toBe('0');
  });

  test('single constant [2] returns "2"', () => {
    const result = buildPolyTeX([2]);

    expect(result).toBe('2');
  });

  test('zero coefficient is skipped', () => {
    const result = buildPolyTeX([0, 1]);

    expect(result).not.toContain('0s');
  });

  test('[1, 0] produces "s"', () => {
    const result = buildPolyTeX([1, 0]);

    expect(result).toBe('s');
  });

  test('[2, 3] produces "2s3"  (2s+3)', () => {
    const result = buildPolyTeX([2, 3]);

    expect(result).toContain('s');
    expect(result).toContain('2');
    expect(result).toContain('3');
  });

  test('[1, 2, 1] produces s^2 + 2s + 1 representation', () => {
    const result = buildPolyTeX([1, 2, 1]);

    expect(result).toContain('s^{2}');
    expect(result).toContain('2');
  });

  test('negative leading coefficient adds minus sign', () => {
    const result = buildPolyTeX([-1, 0]);

    expect(result).toContain('-');
  });

  test('uses custom variable z for z-domain', () => {
    const result = buildPolyTeX([1, 2], 'z');

    expect(result).toContain('z');
    expect(result).not.toContain('s');
  });

  test('all-zero coefficients return "0"', () => {
    const result = buildPolyTeX([0, 0, 0]);

    expect(result).toBe('0');
  });

  test('symbolic string coefficient treated as variable name', () => {
    const result = buildPolyTeX(['a', 0]);

    expect(result).toContain('a');
  });

  test('negative symbolic coefficient adds minus sign', () => {
    const result = buildPolyTeX(['-b', 1]);

    expect(result).toContain('-');
  });
});
//=============================================================================
describe('buildTransferTeX', () => {
  test('wraps numerator and denominator in \\frac{}{}', () => {
    const result = buildTransferTeX([1], [1, 2]);

    expect(result).toMatch(/^\\frac\{/);
  });

  test('uses z variable when specified', () => {
    const result = buildTransferTeX([1], [1, 2], 'z');

    expect(result).toContain('z');
  });

  test('empty arrays produce frac{0}{0}', () => {
    const result = buildTransferTeX([], []);

    expect(result).toBe('\\frac{0}{0}');
  });
});
//=============================================================================
describe('buildPolyMathML', () => {
  test('empty coefficients return zero mrow', () => {
    const result = buildPolyMathML([]);

    expect(result).toContain('<mn>0</mn>');
  });

  test('[1, 0] produces mrow with <mi>s</mi>', () => {
    const result = buildPolyMathML([1, 0]);

    expect(result).toContain('<mi>s</mi>');
  });

  test('negative coefficient adds <mo>-</mo>', () => {
    const result = buildPolyMathML([-2, 0]);

    expect(result).toContain('<mo>-</mo>');
  });

  test('all zeros produce zero mrow', () => {
    const result = buildPolyMathML([0, 0]);

    expect(result).toContain('<mn>0</mn>');
  });
});
//=============================================================================
describe('buildTransferMathML', () => {
  test('wraps result in <math> with <mfrac>', () => {
    const result = buildTransferMathML([1], [1, 1]);

    expect(result).toContain('<mfrac>');
    expect(result).toContain('<math xmlns=');
  });
});
//=============================================================================
