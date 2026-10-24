//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// C++ side of the evaluator parity battery: the real ExprEngine (through the
// native RPC bridge) must agree with the TS evaluator table.
//=============================================================================
import { test, expect } from '@playwright/test';
import { EXPR_PARITY_CASES } from '../../../src/ts/utils/expr-parity-cases';
//=============================================================================
test('ExprEngine matches the shared parity battery', async ({ page }) => {
  await page.goto('/nflow.html');
  await page.waitForFunction(() => typeof (window as any).webui?.call === 'function', {
    timeout: 20_000,
  });
  await page.waitForFunction(
    () => {
      const webui = (window as any).webui;
      return typeof webui?.isConnected !== 'function' || webui.isConnected();
    },
    undefined,
    { timeout: 20_000 }
  );
  const results = await page.evaluate(async (cases) => {
    const parseResult = (result: unknown) => {
      if (typeof result !== 'string') return result;
      const trimmed = result.trim();
      if (!trimmed || !'[{'.includes(trimmed[0])) return result;
      try {
        return JSON.parse(trimmed);
      } catch (_e) {
        return result;
      }
    };
    const out: Array<{ expr: string; ok: boolean; value: number | null; error?: string }> = [];
    for (const c of cases) {
      const raw = await (window as any).webui.call(
        'rpc',
        JSON.stringify({
          id: 1,
          method: 'nflow.expr.eval',
          params: { payload: JSON.stringify({ expr: c.expr, variables: c.variables ?? {} }) },
        })
      );
      const envelope = JSON.parse(String(raw));
      if (envelope.error) throw new Error(envelope.error.message || 'native RPC error');
      const parsed = parseResult(envelope.result) as { ok?: boolean; value?: number; error?: string };
      out.push({ expr: c.expr, ok: !!parsed.ok, value: parsed.value ?? null, error: parsed.error });
    }
    return out;
  }, EXPR_PARITY_CASES);

  for (let i = 0; i < EXPR_PARITY_CASES.length; i++) {
    const expected = EXPR_PARITY_CASES[i].expected;
    const got = results[i];
    expect(got.ok, `${got.expr}: ${got.error ?? ''}`).toBe(true);
    expect(got.value, got.expr).not.toBeNull();
    expect(Math.abs((got.value as number) - expected), `${got.expr} -> ${got.value}`).toBeLessThan(
      1e-9
    );
  }
});
//=============================================================================
