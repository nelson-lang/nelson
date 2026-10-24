//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test, type Page } from '@playwright/test';
//=============================================================================
async function command(page: Page, source: string) {
  const input = page.locator('.console-input');
  if (!(await input.isVisible())) {
    await page.getByText('Command Window', { exact: true }).first().click();
  }
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 60_000 });
  await input.fill(source);
  await input.press('Enter');
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 60_000 });
}
//=============================================================================
test('real engine: pushbuttons render and activate through pointer and keyboard', async ({
  page,
}, info) => {
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected: engine', {
    timeout: 60_000,
  });
  await command(
    page,
    "pb_fig = uifigure('Name', 'Pushbutton regression', 'Position', [100 100 520 260]); " +
      "pb_label = uilabel(pb_fig, 'Text', 'Activations: 0', 'Position', [30 175 300 35]); " +
      'pb_label.UserData = 0; ' +
      "pb_button = uibutton(pb_fig, 'Text', 'Calculate', 'Position', [30 100 200 40], " +
      "'ButtonPushedFcn', 'pb_label.UserData = pb_label.UserData + 1; " +
      "pb_label.Text = sprintf(''Activations: %d'', pb_label.UserData);'); " +
      "pb_disabled = uibutton(pb_fig, 'Text', 'Unavailable', " +
      "'Position', [270 100 200 40], 'Enable', 'off'); drawnow();"
  );
  const figure = page.locator('.figure-panel');
  const button = figure.getByRole('button', { name: 'Calculate', exact: true });
  const disabled = figure.getByRole('button', { name: 'Unavailable', exact: true });
  await expect(button).toBeVisible();
  await expect(disabled).toBeVisible();
  await expect(disabled).toBeDisabled();
  await expect(figure).toContainText('Activations: 0');
  await button.hover();
  await page.mouse.down();
  await expect(figure).toContainText('Activations: 0');
  await page.mouse.up();
  await expect(figure).toContainText('Activations: 1');
  await button.focus();
  await page.keyboard.press('Enter');
  await expect(figure).toContainText('Activations: 2');
  await page.keyboard.press('Space');
  await expect(figure).toContainText('Activations: 3');
  await expect(button).toBeFocused();
  await page.screenshot({ path: info.outputPath('pushbuttons-real-engine.png') });
  await command(page, 'assert_isequal(pb_label.UserData, 3); delete(pb_fig);');
});
//=============================================================================
