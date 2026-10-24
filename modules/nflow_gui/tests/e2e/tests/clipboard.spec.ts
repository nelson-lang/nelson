//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Clipboard E2E tests (copy / cut / paste)
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Clipboard (copy / cut / paste)', () => {
  test('Copy and paste via keyboard duplicates selection', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await page.waitForSelector('#blockLayer > *', { timeout: 10_000 });

    const before = await page.locator('#blockLayer > *').count();
    expect(before).toBeGreaterThan(0);

    await page.locator('#blockLayer > *').first().click();
    await page.keyboard.press('Control+c');
    await page.keyboard.press('Control+v');

    await expect(page.locator('#blockLayer > *')).toHaveCount(before + 1, { timeout: 5_000 });
  });

  test('Cut and paste via keyboard moves selection', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await page.waitForSelector('#blockLayer > *', { timeout: 10_000 });

    const before = await page.locator('#blockLayer > *').count();
    expect(before).toBeGreaterThan(0);

    await page.locator('#blockLayer > *').first().click();
    await page.keyboard.press('Control+x');
    await expect(page.locator('#blockLayer > *')).toHaveCount(before - 1, { timeout: 5_000 });

    await page.keyboard.press('Control+v');
    await expect(page.locator('#blockLayer > *')).toHaveCount(before, { timeout: 5_000 });
  });
});
//=============================================================================
