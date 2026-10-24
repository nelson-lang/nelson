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
test.describe('Quick-insert (double-click on canvas)', () => {
  test.beforeEach(async ({ page }) => {
    const app = new AppPage(page);
    await app.goto();
  });

  test('double-click opens the popup, typing + Enter inserts the block', async ({ page }) => {
    await page.locator('#svgCanvas').dblclick({ position: { x: 420, y: 260 } });
    const input = page.locator('#quickInsertInput');
    await expect(input).toBeVisible();
    await input.fill('gain');
    await input.press('Enter');
    await expect(page.locator('#quickInsertPopup')).toHaveCount(0);
    await expect(page.locator('#blockLayer > *')).not.toHaveCount(0);
    await page.locator('.bottom-panel-header').click();
    await expect(page.locator('.nf-console-entry').last()).toContainText(/added gain/i);
  });

  test('Escape closes the popup without inserting', async ({ page }) => {
    await page.locator('#svgCanvas').dblclick({ position: { x: 400, y: 240 } });
    await expect(page.locator('#quickInsertInput')).toBeVisible();
    await page.keyboard.press('Escape');
    await expect(page.locator('#quickInsertPopup')).toHaveCount(0);
    await expect(page.locator('#blockLayer > *')).toHaveCount(0);
  });

  test('clicking the "Added" console entry selects the inserted block', async ({ page }) => {
    await page.locator('#svgCanvas').dblclick({ position: { x: 420, y: 260 } });
    await page.locator('#quickInsertInput').fill('gain');
    await page.locator('#quickInsertInput').press('Enter');
    await page.locator('.bottom-panel-header').click();
    const link = page.locator('.nf-console-entry.nf-console-link').last();
    await expect(link).toContainText(/added gain/i);
    await link.click();
    await expect(page.locator('#blockLayer g.svg-block.selected')).toHaveCount(1);
    const centered = await page.evaluate(() => {
      const svg = document.getElementById('svgCanvas') as unknown as SVGSVGElement;
      const vb = svg.viewBox.baseVal;
      const g = document.querySelector('#blockLayer g.svg-block.selected');
      const tf = g?.getAttribute('transform') || '';
      const m = tf.match(/translate\(([-\d.]+)[ ,]([-\d.]+)\)/);
      if (!m) return null;
      const bx = Number(m[1]);
      const cx = vb.x + vb.width / 2;
      return Math.abs(bx - cx) < vb.width * 0.25;
    });
    expect(centered).not.toBe(false);
  });
});
//=============================================================================
