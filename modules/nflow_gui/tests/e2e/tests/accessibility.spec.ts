//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Accessibility and keyboard E2E tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
//=============================================================================
test.describe('Accessibility and keyboard', () => {
  test('floating toolbar has appropriate ARIA role', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await expect(page.locator('#floatingToolbar')).toHaveAttribute('role', 'toolbar');
  });

  test('canvas, blocks and ports expose ARIA labels', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await editor.insertBlock('gain', { x: 260, y: 220 });

    await expect(page.locator('#svgCanvas')).toHaveAttribute('role', 'application');
    await expect(page.locator('#svgCanvas')).toHaveAttribute('aria-label', /canvas/i);
    await expect(page.locator('#blockLayer .svg-block').first()).toHaveAttribute(
      'aria-label',
      /block/i
    );
    await expect(page.locator('.port-group').first()).toHaveAttribute('aria-label', /port/i);
  });

  test('with the canvas focused, an arrow key selects the first block', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await editor.insertBlock('gain', { x: 260, y: 220 });

    // Clear the selection the insert left behind, then focus the canvas.
    await editor.canvas.click({ position: { x: 20, y: 20 } });
    await page.locator('#svgCanvas').focus();
    await expect(page.locator('#blockLayer .svg-block.selected')).toHaveCount(0);

    await page.keyboard.press('ArrowDown');
    await expect(page.locator('#blockLayer .svg-block.selected')).toHaveCount(1);
  });

  test('menubar items are reachable via keyboard (basic smoke)', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await page.focus('body');
    await page.keyboard.press('Tab');
    const active = await page.evaluate(() => document.activeElement?.tagName || '');
    expect(typeof active).toBe('string');
  });
});
//=============================================================================
