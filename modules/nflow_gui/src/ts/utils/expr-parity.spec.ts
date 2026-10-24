//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit side of the evaluator parity battery: keeps the TypeScript evaluator
// faithful to the golden values. The b2b suite checks the real C++ ExprEngine
// through nflow.expr.eval with the same table.
//=============================================================================
import { EXPR_PARITY_CASES } from './expr-parity-cases.js';
import { evalExpression } from './expr.js';
//=============================================================================
describe('expression evaluator parity battery (test-harness side)', () => {
  for (const c of EXPR_PARITY_CASES) {
    test(`${c.expr} = ${c.expected}`, async () => {
      const value = evalExpression(c.expr, c.variables ?? {});
      expect(value).toBeCloseTo(c.expected, 10);
    });
  }
});
//=============================================================================
