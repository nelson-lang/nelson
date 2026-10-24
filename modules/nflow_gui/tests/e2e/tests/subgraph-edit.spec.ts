//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Multi-block editing usability: copying a wired selection must paste the wire
// with it (a user copying two connected blocks expects the connection to come
// along), and the align commands must line selected blocks up. clipboard.spec
// only covers single-block copy; alignment is unit-tested (alignment-integration)
// but not through the context-menu UI a user drives.
//=============================================================================
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { type Page } from '@playwright/test';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
import { ContextMenu } from '../pages/ContextMenu.js';
//=============================================================================
const currentDir = path.dirname(fileURLToPath(import.meta.url));
const SIMPLE_DIAGRAM = path.resolve(currentDir, '..', 'fixtures', 'simple-diagram.nflow');
async function loadSimple(page: Page, editor: CanvasEditor): Promise<void> {
  await page.locator('#fileOpenInput').setInputFiles(SIMPLE_DIAGRAM);
  await expect(page.locator('#blockLayer > *')).toHaveCount(2, { timeout: 15_000 });
  await expect.poll(() => editor.wireCount(), { timeout: 5000 }).toBe(1);
}
//=============================================================================
const mod = process.platform === 'darwin' ? 'Meta' : 'Control';
test.describe('subgraph editing', () => {
  test('copying a wired selection pastes the wire too', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadSimple(page, editor);

    await page.locator('#svgCanvas').click({ position: { x: 8, y: 8 } });
    await page.keyboard.press(`${mod}+a`);
    await page.keyboard.press(`${mod}+c`);
    await page.keyboard.press(`${mod}+v`);

    // The pasted copy is a second sine -> scope pair, so blocks and wires double.
    await expect(page.locator('#blockLayer > *')).toHaveCount(4, { timeout: 5000 });
    await expect.poll(() => editor.wireCount(), { timeout: 5000 }).toBe(2);
  });

  test('align-left lines up the left edges of the selection', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    const menu = new ContextMenu(page);
    await loadSimple(page, editor);

    // b1 and b2 start at different x; select both and align their origins.
    const before = await editor.blockOrigins();
    expect(Math.abs(before['b1'].x - before['b2'].x)).toBeGreaterThan(20);

    await page.keyboard.press(`${mod}+a`);
    await menu.openOnBlock(page.locator('#blockLayer > [data-block-id="b2"]'));
    await menu.clickAction('alignLeft');

    await expect
      .poll(
        async () => {
          const origins = await editor.blockOrigins();
          return Math.abs(origins['b1'].x - origins['b2'].x);
        },
        { timeout: 3000 }
      )
      .toBeLessThan(2);
  });
});
//=============================================================================
