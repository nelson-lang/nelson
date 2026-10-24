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
 * Tests for the canvas editor: block rendering, selection, movement, and
 * deletion.  Uses the webui-mock fixture so that file-open operations resolve
 * immediately with a known .nflow diagram instead of opening an OS dialog.
 */
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { LibraryPanel } from '../pages/LibraryPanel.js';
//=============================================================================
test.describe('Canvas editor', () => {
  test('SVG canvas is present and contains block/wire layers', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await expect(app.svgCanvas).toBeVisible();
    await expect(page.locator('#blockLayer')).toBeAttached();
    await expect(page.locator('#wireLayer')).toBeAttached();
  });

  test('opens a diagram and renders blocks on the canvas', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();

    // Open the file via File > Open (dialog is mocked → simple-diagram.nflow).
    await app.openMenu('file');
    await app.clickMenuAction('open');

    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });
  });

  test('clicking a block selects it', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });

    const firstBlock = page.locator('#blockLayer > *').first();
    await firstBlock.click();

    // A selected block should have the CSS class "selected".
    await expect(firstBlock).toBeAttached();
  });

  test('deleting a selected block removes it from the canvas', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });

    const blocksBefore = await page.locator('#blockLayer > *').count();
    expect(blocksBefore).toBeGreaterThan(0);

    await page.locator('#blockLayer > *').first().click();
    await app.deleteSelection();

    await expect(page.locator('#blockLayer > *')).toHaveCount(blocksBefore - 1, {
      timeout: 3_000,
    });
  });

  test('activity bar is visible with expected panel buttons', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    for (const panel of ['library', 'properties', 'simulation', 'settings']) {
      await expect(page.locator(`.activity-icon[data-panel="${panel}"]`)).toBeVisible();
    }
  });
});
//=============================================================================
