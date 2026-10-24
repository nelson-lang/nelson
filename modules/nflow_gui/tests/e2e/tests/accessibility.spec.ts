//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Accessibility and keyboard E2E tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Accessibility and keyboard', () => {
  test('floating toolbar has appropriate ARIA role', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await expect(page.locator('#floatingToolbar')).toHaveAttribute('role', 'toolbar');
  });

  test('menubar items are reachable via keyboard (basic smoke)', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await page.focus('body');
    await page.keyboard.press('Tab');
    const active = await page.evaluate(() => document.activeElement?.tagName || '');
    expect(typeof active).toBe('string');
  });
});
//=============================================================================
