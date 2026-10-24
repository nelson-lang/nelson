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
import { LibraryPanel } from '../pages/LibraryPanel.js';
//=============================================================================
async function reopenManager(page: import('@playwright/test').Page): Promise<void> {
  await page.reload();
  const app = new AppPage(page);
  await app.waitForReady();
  const library = new LibraryPanel(page);
  await library.open();
  await page.getByRole('button', { name: 'Manage libraries' }).click();
}
//=============================================================================
test.describe('Library Manager', () => {
  test.beforeEach(async ({ mockPage: page }) => {
    await page.setViewportSize({ width: 1440, height: 900 });
    const app = new AppPage(page);
    await app.open();
    const library = new LibraryPanel(page);
    await library.open();
    await page.getByRole('button', { name: 'Manage libraries' }).click();
  });

  test('opens as a dedicated accessible view and returns to blocks', async ({ mockPage: page }) => {
    const manager = page.getByRole('region', { name: 'Library Manager' });
    await expect(manager).toBeVisible();
    await expect(page.getByRole('searchbox', { name: 'Search libraries' })).toBeEnabled();
    await expect(manager.locator('summary').filter({ hasText: 'Library locations' })).toBeVisible();
    await expect(
      manager.locator('summary').filter({ hasText: 'Built-in libraries' })
    ).toBeVisible();
    await expect(
      manager.locator('summary').filter({ hasText: 'External libraries' })
    ).toBeVisible();

    await page.getByRole('button', { name: 'Back to blocks' }).click();
    await expect(page.locator('#blockLibraryView')).toBeVisible();
    await expect(page.locator('#blockLibrarySearch')).toBeFocused();
  });

  test('filters libraries without case or accent sensitivity', async ({ mockPage: page }) => {
    await page.locator('.lib-manager-section').nth(1).locator('summary').click();
    await page.getByRole('searchbox', { name: 'Search libraries' }).fill('S\u00d3URCE');

    await expect(page.locator('.lib-manager-library-row')).toHaveCount(1);
    await expect(page.locator('.lib-manager-library-row')).toContainText('builtin.source');
    await expect(page.locator('.lib-manager-library-row [role="switch"]')).toHaveCount(0);
  });

  test('adds and removes an ordered library location', async ({ mockPage: page }) => {
    await page.evaluate(() => {
      (window as typeof window & { __e2eFilePickerPath?: string }).__e2eFilePickerPath =
        'C:\\nflow-libraries';
    });
    await page.getByRole('button', { name: 'Add folder' }).click();

    const location = page.locator('.lib-manager-location-row');
    await expect(location).toHaveCount(1);
    await expect(location).toContainText('C:\\nflow-libraries');
    await expect(location).toContainText('Available');

    await location.getByRole('button', { name: 'Remove' }).click();
    await expect(location).toHaveCount(0);
  });

  test('enables, disables and restores an external library after reload', async ({
    mockPage: page,
  }) => {
    await page.evaluate(() => {
      (window as typeof window & { __e2eFilePickerPath?: string }).__e2eFilePickerPath =
        'C:\\nflow-libraries';
    });
    await page.getByRole('button', { name: 'Add folder' }).click();

    let external = page.locator('.lib-manager-library-row').filter({
      hasText: 'custom.example',
    });
    await expect(external).toContainText('Example Library');
    await expect(external.getByRole('switch')).toBeEnabled();
    await external.getByRole('switch').click();
    await expect(external.getByRole('switch')).toBeChecked();

    await reopenManager(page);
    external = page.locator('.lib-manager-library-row').filter({ hasText: 'custom.example' });
    await expect(external.getByRole('switch')).toBeChecked();
    await expect(external.getByRole('switch')).toBeEnabled();
    await external.getByRole('switch').click();
    await expect(external.getByRole('switch')).not.toBeChecked();
    await expect(external.getByRole('switch')).toBeEnabled();
    await expect
      .poll(() =>
        page.evaluate(() => {
          const config = JSON.parse(localStorage.getItem('nflow-e2e-libraries-config') || '{}') as {
            enabledLibraries?: string[];
          };
          return config.enabledLibraries || [];
        })
      )
      .toEqual([]);

    await reopenManager(page);
    expect(
      await page.evaluate(() => {
        const config = JSON.parse(localStorage.getItem('nflow-e2e-libraries-config') || '{}') as {
          enabledLibraries?: string[];
        };
        return config.enabledLibraries || [];
      })
    ).toEqual([]);
    external = page.locator('.lib-manager-library-row').filter({ hasText: 'custom.example' });
    await expect(external.getByRole('switch')).not.toBeChecked();
  });

  test('reorders locations and applies first-path duplicate priority', async ({
    mockPage: page,
  }) => {
    await page.evaluate(() => {
      (window as typeof window & { __e2eFilePickerPath?: string }).__e2eFilePickerPath =
        'C:\\priority-b';
    });
    await page.getByRole('button', { name: 'Add folder' }).click();
    await page.evaluate(() => {
      (window as typeof window & { __e2eFilePickerPath?: string }).__e2eFilePickerPath =
        'C:\\priority-a';
    });
    await page.getByRole('button', { name: 'Add folder' }).click();

    const external = page.locator('.lib-manager-library-row').filter({
      hasText: 'custom.priority',
    });
    await expect(external).toContainText('Priority B');

    const locations = page.locator('.lib-manager-location-row');
    await locations.nth(1).getByRole('button', { name: 'Move up' }).click();
    await expect(locations.nth(0)).toContainText('C:\\priority-a');
    await expect(external).toContainText('Priority A');
  });

  test('keeps an unavailable location until locate or remove and reports a diagnostic', async ({
    mockPage: page,
  }) => {
    await page.evaluate(() => {
      (window as typeof window & { __e2eFilePickerPath?: string }).__e2eFilePickerPath =
        'C:\\missing';
    });
    await page.getByRole('button', { name: 'Add folder' }).click();

    const location = page.locator('.lib-manager-location-row');
    await expect(location).toContainText('Unavailable');
    await expect(page.locator('#diagnosticWarningCount')).not.toHaveText('0');

    await page.evaluate(() => {
      (window as typeof window & { __e2eFilePickerPath?: string }).__e2eFilePickerPath =
        'C:\\nflow-libraries';
    });
    await location.getByRole('button', { name: 'Locate' }).click();
    await expect(location).toContainText('Available');
    await expect(location).toContainText('C:\\nflow-libraries');

    await location.getByRole('button', { name: 'Remove' }).click();
    await expect(location).toHaveCount(0);
  });

  const viewports = [
    { width: 1440, height: 900 },
    { width: 1280, height: 800 },
    { width: 1024, height: 768 },
  ];
  for (const viewport of viewports) {
    test(`fits inside the ${viewport.width}x${viewport.height} viewport`, async ({
      mockPage: page,
    }) => {
      await page.setViewportSize(viewport);
      await reopenManager(page);

      const manager = page.getByRole('region', { name: 'Library Manager' });
      await expect(manager).toBeVisible();
      const bounds = await manager.boundingBox();
      expect(bounds).not.toBeNull();
      expect((bounds?.x || 0) + (bounds?.width || 0)).toBeLessThanOrEqual(viewport.width);
      expect((bounds?.y || 0) + (bounds?.height || 0)).toBeLessThanOrEqual(viewport.height);
    });
  }

  test('swaps the background theme variable between light and dark', async ({ mockPage: page }) => {
    await page.setViewportSize({ width: 1440, height: 900 });
    await reopenManager(page);
    await expect(page.getByRole('region', { name: 'Library Manager' })).toBeVisible();

    // #rrgggg -> perceived luminance; the dark theme must be darker than light.
    const luminance = (hex: string): number => {
      const value = hex.replace('#', '').trim();
      const r = parseInt(value.slice(0, 2), 16);
      const g = parseInt(value.slice(2, 4), 16);
      const b = parseInt(value.slice(4, 6), 16);
      return 0.2126 * r + 0.7152 * g + 0.0722 * b;
    };
    const backgroundVar = async (): Promise<string> =>
      page.evaluate(() =>
        getComputedStyle(document.documentElement).getPropertyValue('--nflow-background').trim()
      );

    await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'light'));
    const light = await backgroundVar();
    await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'dark'));
    const dark = await backgroundVar();

    expect(light).not.toBe('');
    expect(dark).not.toBe(light);
    expect(luminance(dark)).toBeLessThan(luminance(light));
  });
});
//=============================================================================
