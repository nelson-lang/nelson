//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '@playwright/test';
import { AppPage } from '../pages/AppPage';
//=============================================================================
test.describe('Bottom console panel', () => {
  let app: AppPage;

  test.beforeEach(async ({ page }) => {
    app = new AppPage(page);
    await app.goto();
  });

  test('mounts collapsed under the editor', async ({ page }) => {
    const panel = page.locator('#bottomPanel');
    await expect(panel).toHaveCount(1);
    await expect(panel).toHaveClass(/collapsed/);
    await expect(page.locator('.bottom-panel-title')).toHaveText(/console/i);
  });

  test('expands via its header and collapses back', async ({ page }) => {
    const panel = page.locator('#bottomPanel');
    await page.locator('.bottom-panel-header').click();
    await expect(panel).not.toHaveClass(/collapsed/);
    const content = page.locator('#consolePanelHost');
    await expect(content.locator('[data-testid="console-panel"]')).toBeVisible();
    await page.locator('.bottom-panel-header').click();
    await expect(panel).toHaveClass(/collapsed/);
  });

  test('toggles from the View menu', async ({ page }) => {
    const panel = page.locator('#bottomPanel');
    await page.locator('.menubar-item[data-menu="view"]').click();
    await page.locator('.menubar-item[data-action="toggleConsole"]').click();
    await expect(panel).not.toHaveClass(/collapsed/);
  });

  test('shows status-service messages', async ({ page }) => {
    await page.locator('.bottom-panel-header').click();
    const consolePanel = page.locator('[data-testid="console-panel"]');
    await expect(consolePanel).toBeVisible();
  });

  test('logs a UI action (File > New) as a console entry', async ({ page }) => {
    await page.locator('.bottom-panel-header').click();
    await page.locator('.menubar-item[data-menu="file"]').click();
    await page.locator('.menubar-item[data-action="new"]').click();
    // File > New always asks about saving first.
    await page.locator('#dontSaveConfirm').click();
    const entries = page.locator('.nf-console-entry');
    await expect(entries.last()).toContainText(/new diagram created/i);
    await expect(entries.last().locator('.nf-console-time')).not.toHaveText('');
  });
});
//=============================================================================
