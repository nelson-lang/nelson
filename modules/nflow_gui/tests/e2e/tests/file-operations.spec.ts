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
 * file-operations.spec.ts
 *
 * Tests for File menu operations:
 *  - New  → clears the canvas
 *  - Open → loads a .nflow diagram (dialog mocked → simple-diagram.nflow)
 *  - Save / Save As → triggers save dialog (dialog mocked → temp path)
 *
 * Native OS dialogs are intercepted by the webui-mock fixture so that tests
 * never block on system UI.
 */
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('File operations', () => {
  test('File menu is accessible', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');

    for (const action of ['new', 'open', 'save', 'saveAs']) {
      await expect(page.locator(`[data-action="${action}"]`).first()).toBeVisible();
    }

    await page.keyboard.press('Escape');
  });

  test('Open loads a diagram onto the canvas', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');

    // simple-diagram.nflow has 2 blocks (sine + scope).
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });
    const count = await page.locator('#blockLayer > *').count();
    expect(count).toBeGreaterThanOrEqual(2);
  });

  test('New clears the canvas', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });

    await app.openMenu('file');
    await app.clickMenuAction('new');

    await expect(page.locator('#blockLayer > *')).toHaveCount(0, { timeout: 5_000 });
  });

  test('Diagram title is updated after loading a file', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    const titleBefore = await app.title();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });

    const titleAfter = await app.title();
    // simple-diagram.nflow has name "simple-diagram".
    expect(titleAfter).toContain('simple-diagram');

    void titleBefore; // may or may not differ; we just verify the after state.
  });

  test('Save can be triggered via File > Save', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });

    await app.openMenu('file');
    await app.clickMenuAction('save');

    await expect(page.locator('#blockLayer > *')).toHaveCount(2, { timeout: 5_000 });
  });

  test('Save As can be triggered via File > Save As', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });

    await app.openMenu('file');
    await app.clickMenuAction('saveAs');

    await expect(page.locator('#blockLayer > *')).toHaveCount(2, { timeout: 5_000 });
  });

  test('Ctrl+N keyboard shortcut creates a new diagram', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('open');
    await expect(page.locator('#blockLayer > *').first()).toBeVisible({ timeout: 10_000 });

    await page.keyboard.press('Control+n');

    await expect(page.locator('#blockLayer > *')).toHaveCount(0, { timeout: 5_000 });
  });
});
//=============================================================================
