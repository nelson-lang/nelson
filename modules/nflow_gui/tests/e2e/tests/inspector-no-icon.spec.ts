//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { InspectorPanel } from '../pages/InspectorPanel.js';
//=============================================================================
test('Properties header icon is cleared when no block is selected', async ({ mockPage: page }) => {
  const app = new AppPage(page);
  const inspector = new InspectorPanel(page);
  await app.open();

  await app.openMenu('file');
  await app.clickMenuAction('open');
  await page.waitForSelector('#blockLayer > *', { timeout: 10_000 });

  await page.locator('#blockLayer > *').first().click();
  await inspector.open();

  const propsIcon = page.locator('#panel-properties .sidebar-header .sidebar-properties-icon');

  const beforeType = await propsIcon.getAttribute('data-type');
  expect(beforeType).toBeTruthy();

  // Deselect by clicking the SVG canvas (app clears selection on svg click)
  await page.locator('#svgCanvas').click();

  await page.waitForFunction(() => {
    const el = document.querySelector('#panel-properties .sidebar-header .sidebar-properties-icon');
    if (!el) return true;
    return !el.getAttribute('data-type') && !el.querySelector('svg');
  }, { timeout: 3_000 });

  const afterType = await propsIcon.getAttribute('data-type');
  expect(afterType).toBeNull();
  await expect(propsIcon.locator('svg')).toHaveCount(0);
});
//=============================================================================
