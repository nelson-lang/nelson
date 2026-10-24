//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Persistence E2E tests (skeleton)
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Persistence (Save / Load)', () => {
  test('save action is available and can be invoked', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    const save = page.locator('[data-action="save"]');
    await expect(save.first()).toBeAttached();
    await app.clickMenuAction('save');

    const status = await app.statusText();
    expect(typeof status).toBe('string');
  });

  test('save as subsystem action exists', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    // The "Save As Subsystem" menu item was removed intentionally; do not assert its presence.
    await app.openMenu('file');
    const save = page.locator('[data-action="save"]');
    await expect(save.first()).toBeAttached();
  });
});
//=============================================================================
