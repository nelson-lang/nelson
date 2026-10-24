//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Drag & Drop edge-case scenarios (skeleton)
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { LibraryPanel } from '../pages/LibraryPanel.js';
//=============================================================================
test.describe('Drag & Drop edge cases', () => {
  test('dragging a block to the top-left corner of the canvas', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    const library = new LibraryPanel(page);
    await library.open();
    await expect(library.libraryGroups.locator('[draggable="true"]').first()).toBeVisible({ timeout: 15_000 });

    const before = await page.locator('#blockLayer > *').count();
    await library.dragBlockToCanvas('', 10, 10);
    await expect(page.locator('#blockLayer > *')).toHaveCount(before + 1, { timeout: 5_000 });
  });
});
//=============================================================================
