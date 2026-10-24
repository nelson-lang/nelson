//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as FieldNames from '../FieldNames.js';
import {
  replaceLatexVars,
  normalizeVarName,
  resolveNumeric,
  resolveArray,
  parseVariables,
  estimateLatexWidth,
  evalExpression,
  exprToLatex,
} from './expr';
//=============================================================================
describe('utils/expr focused behaviors', () => {
  test('replaceLatexVars removes leading backslashes', () => {
    expect(replaceLatexVars('\\alpha + x')).toBe('alpha + x');
  });

  test('normalizeVarName trims and removes leading backslash', () => {
    expect(normalizeVarName('  \\foo ')).toBe('foo');
    expect(normalizeVarName(null)).toBe('');
  });

  test('resolveNumeric handles numbers, named constants and expressions', () => {
    expect(resolveNumeric(3)).toBe(3);
    expect(resolveNumeric('1e3')).toBe(1000);
    expect(resolveNumeric(FieldNames.kPi)).toBeCloseTo(Math.PI);
    expect(resolveNumeric('2+3*4')).toBe(14);
    expect(resolveNumeric('\\pi')).toBeCloseTo(Math.PI);
  });

  test('resolveArray parses arrays and CSV strings', () => {
    expect(resolveArray([1, '2', '3.5'] as unknown)).toEqual([1, 2, 3.5]);
    expect(resolveArray('4, 5,6')).toEqual([4, 5, 6]);
  });

  test('parseVariables parses multiline variable blocks and populates display', () => {
    const src = `# comment\na = 1\nb = a + 2\ninvalid\n\\c = 3`;
    const { vars, display } = parseVariables(src);
    expect(vars.a).toBe(1);
    expect(vars.b).toBe(3);
    expect(display).toEqual(expect.arrayContaining(['a=1', 'b=3']));
  });

  test('estimateLatexWidth returns at least the minimum width', () => {
    expect(estimateLatexWidth('x')).toBeGreaterThanOrEqual(80);
  });

  test('evalExpression supports math aliases and functions', () => {
    expect(evalExpression('2^3')).toBe(8);
    expect(evalExpression('sin(pi/2)')).toBeCloseTo(1);
    expect(Number.isNaN(evalExpression('not_a_number'))).toBe(true);
  });

  test('exprToLatex produces LaTeX for simple expressions', () => {
    const latex = exprToLatex('sin(pi/2) + x');
    expect(typeof latex).toBe('string');
    expect(latex.length).toBeGreaterThan(0);
  });
});
//=============================================================================
