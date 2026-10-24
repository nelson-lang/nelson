//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// N1b - Wire removal / re-editing. Creating and refusing wires is covered by
// wiring-invalid; this covers the other half of the everyday gesture: removing a
// wire (select it, press Delete), undoing that, and the fact that deleting a
// block takes its wires with it. Selection marks the wire path .selected
// (renderer-selection), Delete removes it, undo restores it. The undo case loads
// the bundled simple diagram so it inherits a clean initial history snapshot.
//=============================================================================
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { type Page } from '@playwright/test';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
//=============================================================================
const currentDir = path.dirname(fileURLToPath(import.meta.url));
// tests/e2e/tests -> tests/e2e/fixtures/simple-diagram.nflow (b1 sine -> b2 scope, 1 wire).
const SIMPLE_DIAGRAM = path.resolve(currentDir, '..', 'fixtures', 'simple-diagram.nflow');
async function loadSimpleWithWire(page: Page, editor: CanvasEditor): Promise<void> {
  await page.locator('#fileOpenInput').setInputFiles(SIMPLE_DIAGRAM);
  await expect(page.locator('#blockLayer > *')).toHaveCount(2, { timeout: 15_000 });
  await expect.poll(() => editor.wireCount(), { timeout: 5000 }).toBe(1);
}
//=============================================================================
test.describe('N1b - wire editing', () => {
  test('a wire can be selected and deleted, and undo restores it', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadSimpleWithWire(page, editor);

    await page.locator('#wireLayer path.wire-hit').first().click({ force: true });
    await expect(page.locator('#wireLayer path.wire.selected')).toHaveCount(1);
    await app.deleteSelection();
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(0);

    await app.undo();
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(1);
  });

  test('deleting a connected block removes its wire', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadSimpleWithWire(page, editor);

    // Select the scope block (top-level group, not its ports) and delete it.
    await page.locator('#blockLayer > [data-block-id="b2"]').click();
    await app.deleteSelection();

    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(0);
    await expect(page.locator('#blockLayer > [data-block-id="b2"]')).toHaveCount(0);
  });

  test('after deleting a wire the input port is free to reconnect', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await loadSimpleWithWire(page, editor);

    await page.locator('#wireLayer path.wire-hit').first().click({ force: true });
    await app.deleteSelection();
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(0);

    await editor.connect('b1', 0, 'b2', 0);
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(1);
  });
});
//=============================================================================
