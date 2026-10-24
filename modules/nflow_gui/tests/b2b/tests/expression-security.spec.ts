//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Security: expression-block expressions are emitted into generated C/Rust. A crafted
// .nflow could otherwise inject arbitrary code. The real generators must reject
// anything that is not a plain math expression, while still emitting valid
// math verbatim. Exercised through the real engine over the native RPC bridge.
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
//=============================================================================
function diagramWith(expr: string): string {
  return JSON.stringify({
    name: 'sec',
    format: 'nflow-1',
    sampleTime: 0.01,
    runtime: 1,
    variables: '',
    blocks: [
      { id: 'c', type: 'constant', x: 0, y: 0, params: { value: 1 } },
      { id: 'f', type: 'expression', x: 200, y: 0, params: { Expr: expr } },
      { id: 's', type: 'scope', x: 400, y: 0, params: {} },
    ],
    connections: [
      { from: 'c', to: 'f', fromIndex: 0, toIndex: 0 },
      { from: 'f', to: 's', fromIndex: 0, toIndex: 0 },
    ],
  });
}
//=============================================================================
async function gen(page: Page, expr: string, lang: string): Promise<string> {
  const diagram = diagramWith(expr);
  const options = JSON.stringify({ sampleTime: 0.01, includeMain: false, lang });
  const raw = await page.evaluate(
    async ([d, o]) => {
      const rpcRaw = await (window as any).webui.call(
        'rpc',
        JSON.stringify({
          id: 1,
          method: 'nflow.codegen.generate',
          params: { diagramJson: d, optionsJson: o },
        })
      );
      const parsed = JSON.parse(String(rpcRaw));
      if (parsed.error) throw new Error(parsed.error.message || 'native RPC error');
      return parsed.result;
    },
    [diagram, options] as const
  );
  return String(raw);
}
//=============================================================================
const INJECTIONS = [
  '0; system(u)',
  '0, system(u)',
  'system(u)',
  '0; } int evil(){return 1;',
  'u + strlen("x")',
  'u // rest',
  'u /* x */ + 1',
];
//=============================================================================
for (const lang of ['c', 'rust']) {
  test(`expression codegen (${lang}): rejects code-injection expressions`, async ({ page }) => {
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

    for (const expr of INJECTIONS) {
      const out = await gen(page, expr, lang);
      // A rejected expression fails the generation instead of shipping a constant 0.0:
      // the code view shows the error marker naming the security gate.
      expect(out, `expr: ${expr}`).toContain('/* codegen error');
      expect(out, `expr: ${expr}`).toContain('rejected by the math-only security gate');
      // The dangerous token must not reach the output.
      expect(out, `expr: ${expr}`).not.toMatch(/system\s*\(|strlen|int evil/);
    }

    // A legitimate math expression is still emitted (not rejected).
    const okC = await gen(page, 'sin(u) + u * 2', lang);
    expect(okC).not.toContain('rejected non-math expression');
    expect(okC).toMatch(lang === 'c' ? /sin\(/ : /libm::sin\(/);

    // Correctness: "^" is power in the expression grammar but XOR in C/Rust, so
    // it must be emitted as pow(), never as a bare "^".
    const powOut = await gen(page, 'u ^ 2', lang);
    expect(powOut).not.toContain('rejected non-math expression');
    expect(powOut).toMatch(lang === 'c' ? /pow\(/ : /libm::pow\(/);
    const userLine = powOut.split('\n').find((l) => /out_\w+\s*=/.test(l) && /pow\(/.test(l));
    expect(userLine, 'expression line').toBeTruthy();
    expect(userLine!).not.toContain('^');
  });
}
//=============================================================================
