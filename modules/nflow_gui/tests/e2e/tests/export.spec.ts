//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Export E2E tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Export', () => {
  test('export action exists under File menu', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    const exportSvg = page.locator('[data-action="export-svg"]');
    await expect(exportSvg.first()).toBeAttached();
  });
});
//=============================================================================
