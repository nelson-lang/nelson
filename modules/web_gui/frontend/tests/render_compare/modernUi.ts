//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, type Page, type TestInfo } from '@playwright/test';
//=============================================================================
export async function checkModernUi(page: Page, testInfo: TestInfo): Promise<void> {
  await page.setViewportSize({ width: 1920, height: 1000 });
  const input = page.locator('.console-input');
  await expect(input).toBeVisible({ timeout: 120_000 });
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 120_000 });
  await input.fill(
    "run([nelsonroot(), '/modules/graphics/examples/uicontrol/complete_ui_components_demo.m']);"
  );
  await input.press('Enter');
  const main = page.getByRole('tab', { name: /Figure 1|Complete UI component gallery/ }).first();
  await expect(main).toBeVisible({ timeout: 60_000 });
  await main.click();
  const figure = page.locator('.figure-panel__canvas').first();
  const field = figure.locator('input[type="number"]').first();
  await expect(field).toBeVisible();
  const checkPosition = async () => {
    await expect
      .poll(async () => {
        const canvas = await figure.boundingBox();
        const box = await field.boundingBox();
        if (!canvas || !box) return false;
        return (
          Math.abs(box.x - canvas.x - 145) < 1 &&
          Math.abs(box.width - 125) < 1 &&
          Math.abs(box.y - canvas.y - (canvas.height - 555 - 24)) < 1
        );
      })
      .toBe(true);
  };
  await checkPosition();
  const knob = figure
    .locator('svg')
    .filter({ has: page.locator('polygon[fill="rgb(196,38,46)"]') });
  await expect(knob).toBeVisible();
  await expect(knob.locator('circle').first()).toHaveAttribute('r', '29');
  await figure.screenshot({ path: testInfo.outputPath('modern-ui-initial.png') });
  const bounds = await figure.boundingBox();
  if (bounds && bounds.width >= 1000 && bounds.height >= 680) {
    await page.screenshot({
      path: testInfo.outputPath('modern-ui-qt-size.png'),
      clip: { x: bounds.x, y: bounds.y + bounds.height - 680, width: 1000, height: 680 },
    });
  }
  const slider = figure.locator('input[type="range"]').first();
  await slider.focus();
  await slider.press('End');
  await expect(figure).toContainText('frequency 6.00', { timeout: 15_000 });
  await field.fill('2');
  await field.press('Enter');
  await expect(figure).toContainText('amplitude 2.00', { timeout: 15_000 });
  await page.setViewportSize({ width: 1450, height: 900 });
  await checkPosition();
  await expect(figure.locator('svg text').filter({ hasText: /^2$/ }).first()).toBeVisible();
  const png = await figure.screenshot({ path: testInfo.outputPath('modern-ui.png') });
  await testInfo.attach('modern-ui', { body: png, contentType: 'image/png' });
}
//=============================================================================
