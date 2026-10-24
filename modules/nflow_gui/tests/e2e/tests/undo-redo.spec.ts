//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/**
 * Undo (Ctrl+Z) and redo (Ctrl+Y) after deleting blocks, and the Edit menu shortcuts.
 */
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Undo / Redo', () => {
  async function loadDiagram(app: AppPage): Promise<number> {
    await app.open();
    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(app.page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });
    return app.page.locator('#blockLayer > *').count();
  }

  test('Ctrl+Z undoes a block deletion', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const initialCount = await loadDiagram(app);

    await page.locator('#blockLayer > *').first().click();
    await app.deleteSelection();
    await expect(page.locator('#blockLayer > *')).toHaveCount(initialCount - 1, { timeout: 3_000 });

    await app.undo();
    await expect(page.locator('#blockLayer > *')).toHaveCount(initialCount, { timeout: 3_000 });
  });

  test('Ctrl+Y redoes after Ctrl+Z', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const initialCount = await loadDiagram(app);

    await page.locator('#blockLayer > *').first().click();
    await app.deleteSelection();
    await app.undo();
    await expect(page.locator('#blockLayer > *')).toHaveCount(initialCount, { timeout: 3_000 });

    await app.redo();
    await expect(page.locator('#blockLayer > *')).toHaveCount(initialCount - 1, { timeout: 3_000 });
  });

  test('Edit menu exposes Undo and Redo items', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('edit');

    await expect(page.locator('[data-action="undo"]').first()).toBeVisible();
    await expect(page.locator('[data-action="redo"]').first()).toBeVisible();

    await page.keyboard.press('Escape');
  });

  test('Undo via Edit menu undoes a block deletion', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const initialCount = await loadDiagram(app);

    await page.locator('#blockLayer > *').first().click();
    await app.deleteSelection();
    await expect(page.locator('#blockLayer > *')).toHaveCount(initialCount - 1, { timeout: 3_000 });

    await app.openMenu('edit');
    await app.clickMenuAction('undo');

    await expect(page.locator('#blockLayer > *')).toHaveCount(initialCount, { timeout: 3_000 });
  });
});
//=============================================================================
