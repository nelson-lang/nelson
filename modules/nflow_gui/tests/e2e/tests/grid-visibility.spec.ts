//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// E2E: verify grid visibility is applied from ui-settings.json
import { test, expect } from '@playwright/test';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test('grid is visible when ui-settings.json enables it', async ({ page }) => {
  // Intercept the ui-settings.json fetch and return a config enabling the grid
  await page.route('**/nflow/ui-settings.json', async (route) => {
    await route.fulfill({
      status: 200,
      contentType: 'application/json',
      body: JSON.stringify({ gridVisible: true, gridSize: 12 }),
    });
  });

  const app = new AppPage(page);
  await app.open();

  const classes = (await page.locator('#gridToggleBtn').getAttribute('class')) || '';
  expect(classes.split(/\s+/).includes('active')).toBe(true);

  const bg = await page.evaluate(() => {
    const el = document.getElementById('canvas');
    if (!el) return null;
    return getComputedStyle(el).backgroundImage;
  });
  expect(bg && bg !== 'none').toBe(true);
});
//=============================================================================
export {};
//=============================================================================
