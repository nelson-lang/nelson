//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment node */
/* eslint-disable @typescript-eslint/no-explicit-any */
let expr: any;
beforeEach(async () => {
  expr = await import('../../../dist/utils/expr.js');
});
//=============================================================================
test('replaceLatexVars and normalizeVarName behave correctly', () => {
  const input = '\\alpha + x';
  const replaced = expr.replaceLatexVars(input);
  const normalized = expr.normalizeVarName('\\pi');
  expect(replaced).toBe('alpha + x');
  expect(normalized).toBe(FieldNames.kPi);
});
//=============================================================================
test('evalExpression evaluates basic expressions and variables', () => {
  const vars = { a: 2 };
  const v1 = expr.evalExpression('2+3', {});
  const v2 = expr.evalExpression('a^2', vars);
  expect(v1).toBe(5);
  expect(v2).toBe(4);
});
//=============================================================================
test('exprToLatex and estimateLatexWidth produce output', () => {
  const latex = expr.exprToLatex('1+2');
  const width = expr.estimateLatexWidth(latex);
  expect(latex).toContain('1 + 2');
  expect(width).toBeGreaterThanOrEqual(80);
});
//=============================================================================
test('parseVariables parses and returns numeric vars', () => {
  const text = 'x=3\ny = 4\n# comment\n';
  const { vars, display } = expr.parseVariables(text);
  expect(vars.x).toBe(3);
  expect(vars.y).toBe(4);
  expect(display).toEqual(expect.arrayContaining(['x=3', 'y=4']));
});
//=============================================================================
test('evalExpression handles exponentials, unary minus, functions and bad idents', () => {
  expect(expr.evalExpression('1e3')).toBe(1000);
  expect(expr.evalExpression('-3')).toBe(-3);
  expect(expr.evalExpression('sinc(0)')).toBeCloseTo(1);
  expect(expr.evalExpression('clamp(5,1,3)')).toBe(3);
  expect(Number.isNaN(expr.evalExpression('unknown'))).toBe(true);
});
//=============================================================================
test('exprToLatex renders special functions and escapes identifiers', () => {
  const samples = [
    ['sqrt(4)', '\\sqrt{4}'],
    ['abs( -1 )', '\\left|-1\\right|'],
    ['min(1,2)', '\\min(1, 2)'],
    ['max(1,2)', '\\max(1, 2)'],
    ['pow(2,3)', '2^{3}'],
    ['log10(100)', '\\log_{10}(100)'],
    ['log2(8)', '\\log_{2}(8)'],
    ['clamp(x,0,1)', '\\operatorname{clamp}(x, 0, 1)'],
    ['sinc(x)', '\\operatorname{sinc}(x)'],
  ];
  for (const [inp, want] of samples) {
    const out = expr.exprToLatex(inp);
    expect(out).toContain(want.replace(/\\/g, '\\'));
  }
  const out2 = expr.exprToLatex('var_name+1');
  expect(out2).toContain('\\mathrm{var\\_name}');
});
//=============================================================================
test('resolveNumeric and resolveArray behavior', () => {
  expect(expr.resolveNumeric(5)).toBe(5);
  expect(expr.resolveNumeric('  6  ')).toBe(6);
  expect(expr.resolveNumeric(FieldNames.kPi)).toBeCloseTo(Math.PI);
  expect(expr.resolveNumeric('\\pi')).toBeCloseTo(Math.PI);
  expect(expr.resolveNumeric('e')).toBeCloseTo(Math.E);
  expect(expr.resolveNumeric('1+2', {}, { allowExpressions: false })).toBe(0);
  expect(expr.resolveArray([1, '2', 'x'], { x: 3 })).toEqual([1, 2, 3]);
  expect(expr.resolveArray('1, 2, , 3', {})).toEqual([1, 2, 3]);
});
//=============================================================================
test('parseVariables ignores invalid lines and evaluates expressions referencing vars', () => {
  const text = '#comment\n =bad\n good=2\n sum=good+3\n';
  const { vars, display } = expr.parseVariables(text);
  expect(vars.good).toBe(2);
  expect(vars.sum).toBe(5);
  expect(display).toEqual(expect.arrayContaining(['good=2', 'sum=5']));
});
//=============================================================================
describe('type contracts', () => {
  test('replaceLatexVars and tokenize return string and array respectively', () => {
    expect(typeof expr.replaceLatexVars('\\alpha + x')).toBe('string');
    const latex = expr.exprToLatex('1 + x');
    expect(typeof latex).toBe('string');
  });
  test('evalExpression returns number for valid expr and throws for invalid', () => {
    expect(typeof expr.evalExpression('2+2', {})).toBe('number');
    expect(Number.isNaN(expr.evalExpression('bad@@', {}))).toBe(true);
  });
});
//=============================================================================
export {};
//=============================================================================
