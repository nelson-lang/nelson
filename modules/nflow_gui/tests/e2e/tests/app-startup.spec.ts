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
 * Verifies that nflow loads cleanly, the loading overlay is removed, the
 * menu bar is present, and no unexpected console errors occur.
 */
import { test, expect } from '@playwright/test';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('App startup', () => {
  test('loads without critical console errors', async ({ page }) => {
    const errors: string[] = [];
    page.on('console', (msg) => {
      if (msg.type() === 'error') errors.push(msg.text());
    });

    const app = new AppPage(page);
    await app.open();

    // Filter out known benign messages (e.g. network errors for optional resources).
    const criticalErrors = errors.filter(
      (e) =>
        !e.includes('webui.js') && // webui bridge not available outside C++ host
        !e.includes('favicon') &&
        !e.includes('ERR_FILE_NOT_FOUND') &&
        // Ignore benign 404s produced by optional assets when running
        // against the static `dist/` server in CI/local runs.
        !e.includes('Failed to load resource')
    );
    expect(criticalErrors).toHaveLength(0);
  });

  test('loading overlay disappears after startup', async ({ page }) => {
    const app = new AppPage(page);
    await app.open();

    // After waitForReady the html element must not carry nf-loading.
    const hasLoadingClass = await page.evaluate(() =>
      document.documentElement.classList.contains('nf-loading')
    );
    expect(hasLoadingClass).toBe(false);

    await expect(app.loadingOverlay).not.toBeVisible();
  });

  test('document title is set', async ({ page }) => {
    const app = new AppPage(page);
    await app.open();

    const title = await app.title();
    expect(title.length).toBeGreaterThan(0);
  });

  test('menu bar shows File, Edit, View, Help', async ({ page }) => {
    const app = new AppPage(page);
    await app.open();

    for (const menu of ['file', 'edit', 'view', 'help'] as const) {
      const item = page.locator(`.menubar-item[data-menu="${menu}"]`);
      await expect(item).toBeVisible();
    }
  });

  test('SVG canvas is rendered', async ({ page }) => {
    const app = new AppPage(page);
    await app.open();

    await expect(app.svgCanvas).toBeVisible();

    await expect(app.blockLayer).toBeAttached();
  });
});
//=============================================================================
