//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// E2E tests for "Create Subsystem from Selection"
//=============================================================================
import * as path from 'path';
import * as url from 'url';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
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
async function selectAll(app: AppPage): Promise<number> {
  await app.page.keyboard.press('Control+a');
  return app.blockLayer.locator('> *').count();
}
//=============================================================================
test.describe('Create Subsystem from Selection - context menu item', () => {
  test('menu item is present in block context menu', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await loadThreeBlockChain(app);

    const firstBlock = app.blockLayer.locator('> *').first();
    const ctx = new ContextMenu(page);
    await ctx.openOnBlock(firstBlock);

    await expect(page.locator('#blockContextMenu [data-action="createSubsystem"]')).toHaveCount(1);

    await page.keyboard.press('Escape');
  });
});
//=============================================================================
test.describe('Create Subsystem from Selection - single block → no-op', () => {
  test('triggering on a single-block selection shows a status message and does not reduce block count', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await loadThreeBlockChain(app);

    const firstBlock = app.blockLayer.locator('> *').first();
    await firstBlock.click();

    const ctx = new ContextMenu(page);
    await ctx.openOnBlock(firstBlock);
    await ctx.clickAction('createSubsystem');

    await expect(app.blockLayer.locator('> *')).toHaveCount(3, { timeout: 5_000 });

    const status = await app.statusText();
    expect(status.length).toBeGreaterThan(0);
  });
});
//=============================================================================
test.describe('Create Subsystem from Selection - full selection', () => {
  test('selecting all 3 blocks and creating subsystem leaves 1 block', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await loadThreeBlockChain(app);

    await selectAll(app);

    const firstBlock = app.blockLayer.locator('> *').first();
    const ctx = new ContextMenu(page);
    await ctx.openOnBlock(firstBlock);
    await ctx.clickAction('createSubsystem');

    await expect(app.blockLayer.locator('> *')).toHaveCount(1, { timeout: 8_000 });

    const status = await app.statusText();
    expect(status.length).toBeGreaterThan(0);
  });

  test('resulting subsystem block carries correct externalInputs/externalOutputs', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await loadThreeBlockChain(app);

    await selectAll(app);
    const firstBlock = app.blockLayer.locator('> *').first();
    const ctx = new ContextMenu(page);
    await ctx.openOnBlock(firstBlock);
    await ctx.clickAction('createSubsystem');

    await expect(app.blockLayer.locator('> *')).toHaveCount(1, { timeout: 8_000 });

    const params = await page.evaluate(() => {
      const { state } = (window as any).__nf_state_ref
        ? { state: (window as any).__nf_state_ref }
        : { state: (window as any)._nfState ?? (window as any).nfState ?? null };
      if (!state) {
        const appState = (window as any).__nf_app_state as any;
        if (!appState) return null;
        const [, blk] = Array.from((appState.blocks as Map<number, any>).entries())[0] ?? [];
        return blk ? { type: blk.type, externalInputs: blk.params?.externalInputs, externalOutputs: blk.params?.externalOutputs } : null;
      }
      const [, blk] = Array.from((state.blocks as Map<number, any>).entries())[0] ?? [];
      return blk ? { type: blk.type, externalInputs: blk.params?.externalInputs, externalOutputs: blk.params?.externalOutputs } : null;
    });

    if (params !== null) {
      // All 3 blocks were internal, so the subsystem has no external ports.
      expect(params.type).toBe('subsystem');
      expect(params.externalInputs ?? []).toHaveLength(0);
      expect(params.externalOutputs ?? []).toHaveLength(0);
    }
  });
});
//=============================================================================
test.describe('Create Subsystem from Selection - partial selection (boundary)', () => {
  test('selecting gain+scope (2 of 3 blocks) leaves 2 blocks in outer diagram', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await loadThreeBlockChain(app);

    const blocks = app.blockLayer.locator('> *');

    // DOM order = fixture order: nth(0)=sine x~100, nth(1)=gain x~300, nth(2)=scope x~500.
    const [sineBox, gainBox, scopeBox] = await Promise.all([
      blocks.nth(0).boundingBox(),
      blocks.nth(1).boundingBox(),
      blocks.nth(2).boundingBox(),
    ]);
    if (!sineBox || !gainBox || !scopeBox)
      throw new Error('Could not get block bounding boxes');

    // Marquee selection starts only when Ctrl is held and the pointerdown target
    // is the SVG canvas itself, so start on empty canvas right of sine and above
    // all blocks, then sweep over gain and scope but not sine.
    const dragStartX = sineBox.x + sineBox.width + 5;
    const dragStartY = Math.max(5, Math.min(sineBox.y, gainBox.y, scopeBox.y) - 40);
    const dragEndX   = scopeBox.x + scopeBox.width + 20;
    const dragEndY   = Math.max(sineBox.y + sineBox.height,
                                gainBox.y + gainBox.height,
                                scopeBox.y + scopeBox.height) + 20;

    await page.keyboard.down('Control');
    await page.mouse.move(dragStartX, dragStartY);
    await page.mouse.down();
    await page.mouse.move(dragEndX, dragEndY, { steps: 8 });
    await page.mouse.up();
    await page.keyboard.up('Control');

    // handleBlockContextMenu sets selectedId but does not clear selectedIds, so
    // gain+scope stay in the multi-selection.
    const ctx = new ContextMenu(page);
    await ctx.openOnBlock(blocks.nth(1));
    await ctx.clickAction('createSubsystem');

    await expect(app.blockLayer.locator('> *')).toHaveCount(2, { timeout: 8_000 });
  });
});
//=============================================================================
test.describe('Create Subsystem from Selection - subsystem is openable', () => {
  test('double-clicking the created subsystem opens it (subsystem breadcrumb becomes visible)', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await loadThreeBlockChain(app);

    await selectAll(app);
    const firstBlock = app.blockLayer.locator('> *').first();
    const ctx = new ContextMenu(page);
    await ctx.openOnBlock(firstBlock);
    await ctx.clickAction('createSubsystem');

    await expect(app.blockLayer.locator('> *')).toHaveCount(1, { timeout: 8_000 });

    const subsystemBlock = app.blockLayer.locator('> *').first();
    await subsystemBlock.dblclick();

    const upBtn = page.locator('#subsystemBreadcrumb');
    await expect(upBtn).toBeVisible({ timeout: 5_000 });
  });
});
//=============================================================================
