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
type ControlAction = { handle: number; value: number; text: string };
type ObservedWindow = Window & { __buttonActions: ControlAction[] };
const actions = (page: Page) => page.evaluate(() => (window as ObservedWindow).__buttonActions);
test.beforeEach(async ({ page }) => {
  await page.addInitScript(() => {
    const observed = window as ObservedWindow;
    observed.__buttonActions = [];
    Object.defineProperty(window, '__mockLastAction', {
      get: () => observed.__buttonActions.at(-1),
      set: (value: ControlAction) => observed.__buttonActions.push(value),
    });
  });
  await page.goto('/');
  const input = page.locator('.console-input').first();
  await expect(input).toBeVisible();
  await expect(page.locator('.xterm-rows').first()).toContainText('connected: mock');
  await input.fill('uidemo');
  await input.press('Enter');
  await expect(
    page.locator('.figure-panel').getByRole('button', { name: 'Push me' })
  ).toBeVisible();
});
//=============================================================================
// The control activates on pointer down (see FigureControls) and only once per click.
test('pushbutton activates once on pointer down, not again on release', async ({ page }) => {
  const button = page.locator('.figure-panel').getByRole('button', { name: 'Push me' });
  await button.hover();
  await page.mouse.down();
  await expect.poll(() => actions(page)).toEqual([{ handle: 9001, value: 0, text: 'Push me' }]);
  await page.mouse.up();
  expect(await actions(page)).toEqual([{ handle: 9001, value: 0, text: 'Push me' }]);
  await expect(button).toBeVisible();
});
//=============================================================================
// Native keyboard activation requires a browser: jsdom does not synthesize
// the button's default click when dispatching keyboard events.
for (const key of ['Enter', 'Space']) {
  test(`pushbutton activates once with ${key}`, async ({ page }) => {
    const button = page.locator('.figure-panel').getByRole('button', { name: 'Push me' });
    await button.focus();
    await expect(button).toBeFocused();
    await page.keyboard.press(key);
    await expect.poll(() => actions(page)).toEqual([{ handle: 9001, value: 0, text: 'Push me' }]);
    await expect(button).toBeFocused();
  });
}
//=============================================================================
