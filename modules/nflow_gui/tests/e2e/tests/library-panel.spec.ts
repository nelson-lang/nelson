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
 * Tests that the library sidebar panel opens, shows block categories, and
 * allows adding blocks to the canvas via drag-and-drop.
 */
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { LibraryPanel } from '../pages/LibraryPanel.js';
//=============================================================================
test.describe('Library panel', () => {
  test('library panel opens when the activity-bar Library button is clicked', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();

    const library = new LibraryPanel(page);
    await library.open();

    await expect(library.panel).toBeVisible();
  });

  test('library panel contains block entries after libraries load', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();

    const library = new LibraryPanel(page);
    await library.open();

    await expect(library.libraryGroups.locator('[draggable="true"]').first()).toBeVisible({
      timeout: 15_000,
    });

    const count = await library.libraryGroups.locator('[draggable="true"]').count();
    expect(count).toBeGreaterThan(0);
  });

  test('library panel shows expected library category groups', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();

    const library = new LibraryPanel(page);
    await library.open();

    await expect(library.libraryGroups.locator('[draggable="true"]').first()).toBeVisible({
      timeout: 15_000,
    });

    // At least one group header (library name) should be present.
    const groups = page.locator('#blockLibraryGroups .library-group, #blockLibraryGroups .group-header, #blockLibraryGroups details');
    const groupCount = await groups.count();
    expect(groupCount).toBeGreaterThan(0);
  });

  test('dragging a block from the library adds it to the canvas', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();

    const library = new LibraryPanel(page);
    await library.open();

    await expect(library.libraryGroups.locator('[draggable="true"]').first()).toBeVisible({
      timeout: 15_000,
    });

    const blocksBefore = await page.locator('#blockLayer > *').count();

    await library.dragBlockToCanvas('', 300, 200);

    await expect(page.locator('#blockLayer > *')).toHaveCount(blocksBefore + 1, {
      timeout: 5_000,
    });
  });

  test('library panel closes when the activity-bar button is clicked again', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();

    const library = new LibraryPanel(page);
    await library.open();
    await expect(library.panel).toBeVisible();

    await library.close();

    // After closing, the sidebar should be collapsed (panel not visible or sidebar collapsed).
    const sidebar = page.locator('.sidebar');
    const isCollapsed = await sidebar.evaluate((el) => el.classList.contains('collapsed'));
    expect(isCollapsed).toBe(true);
  });
});
//=============================================================================
