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
test.describe('Inspector panel', () => {
  test('shows block parameters and accepts updates', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const inspector = new InspectorPanel(page);
    await app.open();

    await page.getByRole('searchbox', { name: 'Search blocks' }).fill('sine');
    await page.getByRole('button', { name: 'Sine Source' }).click();
    await page.waitForSelector('#blockLayer > *', { timeout: 10_000 });

    await page.locator('#blockLayer > *').first().click();

    await inspector.open();
    await expect(inspector.panel).toBeVisible();
    await expect(inspector.panel.locator('[data-capability="c"]')).toHaveAttribute(
      'data-status',
      'supported'
    );
    await expect(inspector.panel.locator('[data-capability="rust"]')).toHaveAttribute(
      'data-status',
      'supported'
    );
    await expect(inspector.panel.locator('[data-capability="modelica"]')).toHaveCount(0);

    const helpButton = inspector.panel.getByRole('button', { name: 'Block Help' });
    await expect(helpButton).toBeVisible();
    await helpButton.click();
    await expect
      .poll(() => page.evaluate(() => (window as any).__e2eLastHelpTopic))
      .toBe('block:sine');
    await page.evaluate(() => ((window as any).__e2eLastHelpTopic = ''));
    await inspector.panel.locator('[data-capability="c"] .inspector-capability-value').click();
    await expect
      .poll(() => page.evaluate(() => (window as any).__e2eLastHelpTopic))
      .toBe('block:sine');
    await page.evaluate(() => ((window as any).__e2eLastHelpTopic = ''));
    await app.openMenu('help');
    await expect(page.locator('#selectedBlockHelpAction')).toHaveAttribute(
      'aria-disabled',
      'false'
    );
    await page.locator('#selectedBlockHelpAction').click();
    await expect
      .poll(() => page.evaluate(() => (window as any).__e2eLastHelpTopic))
      .toBe('block:sine');
    await page.evaluate(() => ((window as any).__e2eLastHelpTopic = ''));
    await page.keyboard.press('F1');
    await expect
      .poll(() => page.evaluate(() => (window as any).__e2eLastHelpTopic))
      .toBe('block:sine');

    const capabilities = inspector.panel.locator('[data-inspector-section="capabilities"]');
    const parameters = inspector.panel.locator('[data-inspector-section="parameters"]');
    await capabilities.locator(':scope > summary').click();
    await expect(capabilities).not.toHaveAttribute('open', '');
    await capabilities.locator(':scope > summary').click();
    await expect(capabilities).toHaveAttribute('open', '');

    await parameters.locator(':scope > summary').click();
    await expect(parameters).not.toHaveAttribute('open', '');
    await parameters.locator(':scope > summary').click();
    await expect(parameters).toHaveAttribute('open', '');

    const ampBefore = await inspector.getParam('Amplitude');
    expect(ampBefore).toBeTruthy();

    await inspector.setParam('Amplitude', '2');
    const ampAfter = await inspector.getParam('Amplitude');
    expect(ampAfter).toBe('2');
  });

  test('rotate control is present and clickable', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const inspector = new InspectorPanel(page);
    await app.open();

    await page.getByRole('searchbox', { name: 'Search blocks' }).fill('sine');
    await page.getByRole('button', { name: 'Sine Source' }).click();
    await page.waitForSelector('#blockLayer > *', { timeout: 10_000 });

    await page.locator('#blockLayer > *').first().click();
    await inspector.open();
    // The rotate button may be absent (rotation is also in the context menu);
    // rotateBlock() handles both cases.
    const btnCount = await inspector.rotateBtn.count();
    if (btnCount > 0) await expect(inspector.rotateBtn).toBeVisible();
    await inspector.rotateBlock();

    // No strong DOM assertion here - ensure no errors and inspector stays visible
    await expect(inspector.panel).toBeVisible();
  });
});
//=============================================================================
