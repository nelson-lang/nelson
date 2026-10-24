//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Context menu tests
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { ContextMenu } from '../pages/ContextMenu.js';
//=============================================================================
test.describe('Context menus', () => {
  test('block context menu shows expected actions', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    await app.openMenu('file');
    await app.clickMenuAction('open');
    await page.waitForSelector('#blockLayer > *', { timeout: 10000 });

    const firstBlock = page.locator('#blockLayer > *').first();
    const ctx = new ContextMenu(page);

    await ctx.openOnBlock(firstBlock);
    await expect(page.locator('#blockContextMenu [data-action="delete"]').first()).toBeVisible();
    await expect(page.locator('#blockContextMenu [data-action="copy"]').first()).toBeVisible();
    await expect(page.locator('#blockContextMenu [data-action="blockHelp"]').first()).toBeVisible();
    await page.locator('#blockContextMenu [data-action="blockHelp"]').first().click();
    await expect
      .poll(() => page.evaluate(() => (window as any).__e2eLastHelpTopic))
      .toMatch(/^block:/);

    await expect(page.locator('#blockContextMenu')).not.toHaveClass(/show/);
  });

  test('canvas context menu shows canvas actions', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    const ctx = new ContextMenu(page);
    const canvasBox = await page.locator('#canvas').boundingBox();
    if (!canvasBox) throw new Error('Canvas not visible');

    await ctx.openOnCanvas(Math.floor(canvasBox.x + 10), Math.floor(canvasBox.y + 10));
    await expect(
      page.locator('#canvasContextMenu [data-action="resetView"]').first()
    ).toBeVisible();

    await page.keyboard.press('Escape');
  });

  test('library block context menu opens help without inserting the block', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    await page.getByRole('searchbox', { name: 'Search blocks' }).fill('sine');
    const libraryBlock = page.getByRole('button', { name: 'Sine Source' });
    const initialCount = await page.locator('#blockLayer > *').count();

    await libraryBlock.click({ button: 'right' });
    const help = page.locator('#libraryBlockContextMenu [data-action="libraryBlockHelp"]');
    await expect(help).toBeVisible();
    await help.click();

    await expect
      .poll(() => page.evaluate(() => (window as any).__e2eLastHelpTopic))
      .toBe('block:sine');
    await expect(page.locator('#blockLayer > *')).toHaveCount(initialCount);
  });
});
//=============================================================================
