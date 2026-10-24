//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Golden code generation.
// The real C and Rust generators are run on the bundled examples and their
// output is compared byte-for-byte with versioned golden files. Any refactor
// that changes the generated code will fail here.
//
// To regenerate the goldens intentionally:
//   NFLOW_UPDATE_GOLDEN=1 npm run test:b2b
//=============================================================================
import { test, expect } from '@playwright/test';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import type { Page } from '@playwright/test';
//=============================================================================
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const examplesDir = path.resolve(__dirname, '..', '..', '..', '..', 'nflow_blocks', 'examples');
const goldenDir = path.resolve(__dirname, '..', 'golden');
const UPDATE = process.env.NFLOW_UPDATE_GOLDEN === '1';
const EXAMPLES = ['PID_Demo', 'complementary_filter', 'Logic_Gates_Demo'];
const LANGS: Array<{ lang: string; ext: string }> = [
  { lang: 'c', ext: 'c' },
  { lang: 'rust', ext: 'rs' },
];
/** Strip content that may legitimately vary between runs (none known today;
 * kept as a single normalization point). */
function normalize(source: string): string {
  return source.replace(/\r\n/g, '\n').trimEnd() + '\n';
}
//=============================================================================
async function nflowRpc(page: Page, method: string, params: Record<string, unknown> = {}) {
  return page.evaluate(
    async ([rpcMethod, rpcParams]) => {
      const raw = await (window as any).webui.call(
        'rpc',
        JSON.stringify({ id: 1, method: rpcMethod, params: rpcParams })
      );
      const parsed = JSON.parse(String(raw));
      if (parsed.error) throw new Error(parsed.error.message || 'native RPC error');
      return parsed.result;
    },
    [method, params] as const
  );
}
//=============================================================================
for (const name of EXAMPLES) {
  for (const { lang, ext } of LANGS) {
    test(`golden ${lang} codegen: ${name}`, async ({ page }) => {
      const diagramText = fs.readFileSync(path.join(examplesDir, 'causal', `${name}.nflow`), 'utf8');
      const diagram = JSON.parse(diagramText);

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

      const options = JSON.stringify({
        sampleTime: Number(diagram.sampleTime) || 0.01,
        includeMain: true,
        lang,
      });
      const raw = await nflowRpc(page, 'nflow.codegen.generate', {
        diagramJson: diagramText,
        optionsJson: options,
      });
      const generated = normalize(String(raw));
      expect(generated).not.toContain('/* codegen error');

      const goldenPath = path.join(goldenDir, `${name}.${ext}`);
      if (UPDATE) {
        fs.mkdirSync(goldenDir, { recursive: true });
        fs.writeFileSync(goldenPath, generated);
        console.log(`golden updated: ${goldenPath} (${generated.length} bytes)`);
        return;
      }
      expect(fs.existsSync(goldenPath), `missing golden ${goldenPath}`).toBe(true);
      const golden = normalize(fs.readFileSync(goldenPath, 'utf8'));
      expect(generated).toBe(golden);
    });
  }
}
//=============================================================================
