//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Subsystem E2E tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Subsystem navigation', () => {
  test('subsystem breadcrumb exists and is hidden by default', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    const upBtn = page.locator('#subsystemBreadcrumb');
    // By default the breadcrumb is hidden until inside a subsystem.
    const hidden = await upBtn.evaluate((el) => el.getAttribute('hidden') !== null);
    expect(hidden).toBe(true);
  });
});
//=============================================================================
