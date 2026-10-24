//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as path from 'path';
import * as url from 'url';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { InspectorPanel } from '../pages/InspectorPanel.js';
import { ContextMenu } from '../pages/ContextMenu.js';
//=============================================================================
const __dirname = path.dirname(url.fileURLToPath(import.meta.url));
const FIXTURE = path.resolve(__dirname, '..', 'fixtures', 'three-block-chain.nflow');
async function loadThreeBlockChain(app: AppPage): Promise<void> {
  await app.open();
  await app.page.evaluate(async () => {
    const p = (window as any).__nf_lib_ready_promise as Promise<unknown> | undefined;
    if (p && typeof p.then === 'function') {
      try { await p; } catch (_e) { /* ignore */ }
    }
  });
  const fileInput = app.page.locator('#fileOpenInput');
  await fileInput.setInputFiles(FIXTURE);
  await app.page.waitForSelector('#blockLayer > *', { timeout: 15_000 });
  await expect(app.blockLayer.locator('> *')).toHaveCount(3, { timeout: 10_000 });
}
//=============================================================================
async function selectAll(app: AppPage): Promise<void> {
  await app.page.keyboard.press('Control+a');
}
//=============================================================================
test.describe('Masked subsystem', () => {
  test('inspector hides Open Subsystem action when masked', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const inspector = new InspectorPanel(page);
    await loadThreeBlockChain(app);

    await selectAll(app);
    const firstBlock = app.blockLayer.locator('> *').first();
    const ctx = new ContextMenu(page);
    await ctx.openOnBlock(firstBlock);
    await ctx.clickAction('createSubsystem');

    await expect(app.blockLayer.locator('> *')).toHaveCount(1, { timeout: 8_000 });

    const subsystemBlock = app.blockLayer.locator('> *').first();
    await subsystemBlock.click();
    await inspector.open();

    const openBtn = page.locator('#inspectorBody button[data-action="open-subsystem"]');
    await expect(openBtn).toHaveCount(1);

    await inspector.setParam('masked', true);
    await expect(openBtn).toHaveCount(0, { timeout: 3000 });
  });

  test('double-clicking masked subsystem does not open it', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const inspector = new InspectorPanel(page);
    await loadThreeBlockChain(app);

    await selectAll(app);
    const firstBlock = app.blockLayer.locator('> *').first();
    const ctx = new ContextMenu(page);
    await ctx.openOnBlock(firstBlock);
    await ctx.clickAction('createSubsystem');
    await expect(app.blockLayer.locator('> *')).toHaveCount(1, { timeout: 8_000 });

    const subsystemBlock = app.blockLayer.locator('> *').first();
    await subsystemBlock.click();
    await inspector.open();

    await inspector.setParam('masked', true);

    // Double-click the subsystem - it should NOT open when masked
    await subsystemBlock.dblclick();

    const upBtn = page.locator('#subsystemBreadcrumb');
    const hidden = await upBtn.evaluate((el) => el.getAttribute && el.getAttribute('hidden') !== null);
    expect(hidden).toBe(true);
  });
});
//=============================================================================
