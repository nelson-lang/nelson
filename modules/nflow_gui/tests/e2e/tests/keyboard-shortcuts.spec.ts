//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Keyboard shortcuts smoke tests
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Keyboard shortcuts', () => {
  test('Ctrl+N creates a new empty diagram', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await page.waitForSelector('#blockLayer > *', { timeout: 10000 });
    const before = await page.locator('#blockLayer > *').count();
    expect(before).toBeGreaterThan(0);

    await page.keyboard.press('Control+n');
    await page.waitForTimeout(200);
    const after = await page.locator('#blockLayer > *').count();
    expect(after).toBe(0);
  });

  test('Ctrl+O opens the file dialog (mocked) and loads the diagram', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await page.keyboard.press('Control+o');
    // The open dialog is mocked to return the fixture - wait for blocks.
    await page.waitForSelector('#blockLayer > *', { timeout: 10000 });
    const count = await page.locator('#blockLayer > *').count();
    expect(count).toBeGreaterThan(0);
  });

  test('Ctrl+Z / Ctrl+Y undo and redo', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    await app.openMenu('file');
    await app.clickMenuAction('open');
    await page.waitForSelector('#blockLayer > *', { timeout: 10000 });

    const initial = await page.locator('#blockLayer > *').count();
    expect(initial).toBeGreaterThan(0);

    await page.locator('#blockLayer > *').first().click();
    await page.keyboard.press('Delete');
    await page.waitForTimeout(200);
    await page.keyboard.press('Control+z');
    await page.waitForTimeout(200);
    const afterUndo = await page.locator('#blockLayer > *').count();
    expect(afterUndo).toBe(initial);

    await page.keyboard.press('Control+y');
    await page.waitForTimeout(200);
    const afterRedo = await page.locator('#blockLayer > *').count();
    expect(afterRedo).toBe(initial - 1);
  });
});
//=============================================================================
