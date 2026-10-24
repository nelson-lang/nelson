//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shared parity battery around expression evaluation:
// - the unit side checks the TypeScript evaluator;
// - the b2b side checks the real C++ engine through nflow.expr.eval.
// Extend this table whenever the ExprEngine grammar changes.
//=============================================================================
export interface ParityCase {
  expr: string;
  variables?: Record<string, number>;
  expected: number;
}
//=============================================================================
export const EXPR_PARITY_CASES: ParityCase[] = [
  { expr: '1+2*3', expected: 7 },
  { expr: '(1+2)*3', expected: 9 },
  { expr: '2^10', expected: 1024 },
  { expr: '10/4', expected: 2.5 },
  { expr: '7%4', expected: 3 },
  { expr: '2*x+1', variables: { x: 4 }, expected: 9 },
  { expr: 'a*b - c', variables: { a: 2, b: 5, c: 3 }, expected: 7 },
  { expr: 'sqrt(16)', expected: 4 },
  { expr: 'abs(-3.5)', expected: 3.5 },
  { expr: 'min(3, -2)', expected: -2 },
  { expr: 'max(3, -2)', expected: 3 },
  { expr: 'pow(2, 8)', expected: 256 },
  { expr: 'floor(2.9)', expected: 2 },
  { expr: 'ceil(2.1)', expected: 3 },
  { expr: 'round(2.5)', expected: 3 },
  { expr: 'sin(0)', expected: 0 },
  { expr: 'cos(0)', expected: 1 },
  { expr: 'atan2(0, 1)', expected: 0 },
  { expr: 'exp(0)', expected: 1 },
  { expr: 'log(1)', expected: 0 },
  { expr: 'pi', expected: Math.PI },
  { expr: '2*pi', expected: 2 * Math.PI },
  { expr: 'clamp(15, 0, 10)', expected: 10 },
];
//=============================================================================
