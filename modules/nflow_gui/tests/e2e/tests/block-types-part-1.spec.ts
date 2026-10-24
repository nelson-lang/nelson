//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
//=============================================================================
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fixturesRoot = path.resolve(__dirname, '..', '..'); // modules/nflow_gui/tests/
function collectNflowFiles(dir: string, out: string[] = []) {
  for (const ent of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, ent.name);
    if (ent.isDirectory()) {
      if (path.basename(p) === 'e2e') continue;
      collectNflowFiles(p, out);
    } else if (ent.isFile() && ent.name.endsWith('.nflow')) {
      out.push(p);
    }
  }
  return out;
}
//=============================================================================
const fixtures = collectNflowFiles(fixturesRoot);
const TOTAL_PARTS = 4;
const PART_INDEX = 0;
function hashString(s: string) {
  let h = 0;
  for (let i = 0; i < s.length; i++) h = ((h << 5) - h) + s.charCodeAt(i);
  return Math.abs(h);
}
//=============================================================================
for (const fixturePath of fixtures) {
  const rel = path.relative(fixturesRoot, fixturePath);
  if (hashString(rel) % TOTAL_PARTS !== PART_INDEX) continue;

  test.describe(`fixture: ${rel}`, () => {
    test.use({ openDialogPath: fixturePath });

    test('loads and renders blocks', async ({ mockPage: page, openDialogPath }) => {
      const app = new AppPage(page);
      await app.open();

      // Wait for the block-library templates: the app fires `nf-ready` early and builds the
      // libraries in the background. Loading before `__nf_lib_ready_promise` resolves
      // aborts with "missing block types".
      await page.evaluate(async () => {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const p = (window as any).__nf_lib_ready_promise as Promise<unknown> | undefined;
        if (p && typeof p.then === 'function') {
          try {
            await p;
          } catch (_e) {
            // ignore failures here; we'll let loadDiagram surface any errors
          }
        }
      });

      // Load the fixture through the hidden file input. This avoids OS file dialogs and works
      // in plain browser runs.
      const fileInput = page.locator('#fileOpenInput');
      await fileInput.setInputFiles(openDialogPath as unknown as string);

      await page.waitForSelector('#blockLayer > *', { timeout: 15000 });
      const count = await page.locator('#blockLayer > *').count();
      expect(count).toBeGreaterThan(0);
    });
  });
}
//=============================================================================
