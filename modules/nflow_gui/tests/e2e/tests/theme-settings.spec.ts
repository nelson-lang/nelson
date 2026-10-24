//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Theme and settings E2E tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Theme & Settings', () => {
  test('settings panel opens via activity icon', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await page.locator('.activity-icon[data-panel="settings"]').click();
    const sidebar = page.locator('.sidebar');
    const isCollapsed = await sidebar.evaluate((el) => el.classList.contains('collapsed'));
    expect(isCollapsed).toBe(false);
    await expect(page.locator('#modelicaPathStatus')).toHaveAttribute('data-state', 'ready');
    await expect(page.locator('#modelicaPathStatusLabel')).toHaveText('Detected and ready');
    await expect(page.locator('#modelicaPathStatusDetails')).toContainText('OpenModelica 1.27.0');
  });
});
//=============================================================================
