//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test } from '@playwright/test';
//=============================================================================
const platforms = [
  {
    name: 'macOS',
    navigatorPlatform: 'MacIntel',
    paletteKey: 'Meta+Shift+P',
    panelLabel: 'Command History (⌘1)',
    stopTitle: 'Stop execution (⌘C)',
    focusKey: 'Meta+F6',
    reverseFocusKey: 'Meta+Shift+F6',
  },
  {
    name: 'Windows',
    navigatorPlatform: 'Win32',
    paletteKey: 'Control+Shift+P',
    panelLabel: 'Command History (Ctrl+1)',
    stopTitle: 'Stop execution (Ctrl+C)',
    focusKey: 'Control+F6',
    reverseFocusKey: 'Control+Shift+F6',
  },
  {
    name: 'Linux',
    navigatorPlatform: 'Linux x86_64',
    paletteKey: 'Control+Shift+P',
    panelLabel: 'Command History (Ctrl+1)',
    stopTitle: 'Stop execution (Ctrl+C)',
    focusKey: 'Control+F6',
    reverseFocusKey: 'Control+Shift+F6',
  },
] as const;
//=============================================================================
for (const platform of platforms) {
  test(`uses ${platform.name} shortcut conventions`, async ({ page }) => {
    await page.addInitScript(({ navigatorPlatform }) => {
      Object.defineProperty(navigator, 'userAgentData', {
        configurable: true,
        value: { platform: navigatorPlatform },
      });
      Object.defineProperty(navigator, 'platform', {
        configurable: true,
        value: navigatorPlatform,
      });
    }, platform);
    await page.goto('/');
    await expect(page.getByRole('status')).toHaveText('Ready');

    await expect(page.getByRole('button', { name: platform.panelLabel })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Stop the current execution' })).toHaveAttribute(
      'title',
      platform.stopTitle
    );

    await page.keyboard.press(platform.paletteKey);
    const palette = page.getByRole('dialog', { name: 'Command palette' });
    await expect(palette).toBeVisible();
    await expect(palette.getByRole('textbox')).toBeFocused();
    await page.keyboard.press('Escape');
    await expect(palette).toHaveCount(0);

    const fileMenu = page.getByRole('menuitem', { name: 'File' });
    await fileMenu.focus();
    await page.keyboard.press(platform.focusKey);
    await expect(
      page.getByRole('toolbar', { name: 'Main toolbar' }).getByRole('button').first()
    ).toBeFocused();
    await page.keyboard.press(platform.reverseFocusKey);
    await expect(fileMenu).toBeFocused();
  });
}
//=============================================================================
