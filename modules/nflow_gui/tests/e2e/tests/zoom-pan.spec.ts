//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Zoom & Pan E2E tests (skeleton)
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Zoom & Pan', () => {
  test('canvas and viewport are present', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await expect(app.canvasContainer).toBeVisible();
    await expect(app.svgCanvas).toBeVisible();
  });

  test('view menu contains zoom actions', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('view');
    const zoomIn = page.locator('[data-action="zoomIn"]');
    const zoomOut = page.locator('[data-action="zoomOut"]');
    await expect(zoomIn.first()).toBeAttached();
    await expect(zoomOut.first()).toBeAttached();
  });
});
//=============================================================================
