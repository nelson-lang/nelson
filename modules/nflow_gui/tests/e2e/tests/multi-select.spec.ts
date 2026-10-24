//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Multi-select E2E tests (skeleton)
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Multi-select', () => {
  test('shift-click selects multiple blocks (basic smoke)', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');

    const blocks = page.locator('#blockLayer > *');
    await expect(blocks.first()).toBeVisible({ timeout: 10_000 });

    const count = await blocks.count();
    expect(count).toBeGreaterThan(1);

    await blocks.nth(0).click();
    await page.keyboard.down('Shift');
    await blocks.nth(1).click();
    await page.keyboard.up('Shift');

    expect(await blocks.nth(0).isVisible()).toBe(true);
    expect(await blocks.nth(1).isVisible()).toBe(true);
  });
});
//=============================================================================
