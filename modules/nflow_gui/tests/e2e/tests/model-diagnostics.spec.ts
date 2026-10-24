//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Diagnostics surfacing (host-less half). An error must reach the user through
// the Diagnostics panel: status-bar counter, tab badge, a readable row, severity
// filters and a clear action. A real error is driven through the invalid-wiring
// path (an error-level status that status-service turns into a diagnostic); the
// intentionally-broken fixtures must also load and render. The engine half
// (algebraic loop, unconnected input) lives in tests/b2b/model-diagnostics.
//=============================================================================
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { type Page } from '@playwright/test';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
import { DiagnosticsPanel } from '../pages/DiagnosticsPanel.js';
//=============================================================================
const currentDir = path.dirname(fileURLToPath(import.meta.url));
const invalidDir = path.resolve(currentDir, '..', '..', 'invalid');
async function raiseWiringError(page: Page): Promise<void> {
  const editor = new CanvasEditor(page);
  const a = await editor.insertBlock('sine', { x: 200, y: 200 });
  const b = await editor.insertBlock('constant', { x: 200, y: 340 });
  await editor.clickPort(a, 'out', 0);
  await editor.clickPort(b, 'out', 0); // output -> output: rejected, raises a diagnostic
}
//=============================================================================
test.describe('N2 - diagnostics surfacing (host-less)', () => {
  test('an error raises the status-bar counter and the tab badge', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const diagnostics = new DiagnosticsPanel(page);

    expect(await diagnostics.errorCount()).toBe(0);
    await raiseWiringError(page);

    await diagnostics.waitForErrorCount(1);
    expect(await diagnostics.tabCount()).toBeGreaterThanOrEqual(1);
  });

  test('the diagnostics panel lists the error as a readable row', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const diagnostics = new DiagnosticsPanel(page);

    await raiseWiringError(page);
    await diagnostics.waitForErrorCount(1);
    await diagnostics.open();

    const items = await diagnostics.items();
    expect(items.length).toBeGreaterThanOrEqual(1);
    const errorRow = items.find((i) => i.severity === 'error');
    expect(errorRow, 'an error row is shown').toBeTruthy();
    expect(errorRow?.message.toLowerCase()).toContain('input port');
    expect(errorRow?.source.length).toBeGreaterThan(0);
  });

  test('severity filters hide and reveal rows', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const diagnostics = new DiagnosticsPanel(page);

    await raiseWiringError(page);
    await diagnostics.waitForErrorCount(1);
    await diagnostics.open();
    await expect(diagnostics.rows).toHaveCount(1);

    await page.locator('.nf-diagnostics-filter.nf-diagnostic-error').click();
    await expect(diagnostics.rows).toHaveCount(0);
    await expect(page.locator('.nf-diagnostics-empty')).toBeVisible();

    await page.locator('.nf-diagnostics-filter.nf-diagnostic-error').click();
    await expect(diagnostics.rows).toHaveCount(1);
  });

  test('clear empties the panel and resets the counter', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const diagnostics = new DiagnosticsPanel(page);

    await raiseWiringError(page);
    await diagnostics.waitForErrorCount(1);
    await diagnostics.open();

    await page.locator('.nf-diagnostics-clear').click();
    await expect(diagnostics.rows).toHaveCount(0);
    await expect(page.locator('.nf-diagnostics-empty')).toBeVisible();
    expect(await diagnostics.errorCount()).toBe(0);
  });

  // The clipboard write itself is unit-tested with a mocked navigator.
  test('right-click on a row opens a Copy / Copy all menu', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const diagnostics = new DiagnosticsPanel(page);

    await raiseWiringError(page);
    await diagnostics.waitForErrorCount(1);
    await diagnostics.open();

    await diagnostics.rows.first().click({ button: 'right' });
    const menu = page.locator('.nf-panel-context-menu[role="menu"]');
    await expect(menu).toBeVisible();
    await expect(menu.locator('[role="menuitem"]')).toHaveText(['Copy', 'Copy all']);

    await page.keyboard.press('Escape');
    await expect(menu).toHaveCount(0);
  });

  // The button is disabled until there is something to copy.
  test('the toolbar Copy all button enables once a diagnostic is present', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const diagnostics = new DiagnosticsPanel(page);
    await diagnostics.open();

    const copyAll = page.locator('.nf-diagnostics-copy');
    await expect(copyAll).toBeDisabled();

    await raiseWiringError(page);
    await diagnostics.waitForErrorCount(1);
    await expect(copyAll).toBeEnabled();
  });

  // Opening another file must not carry the previous model's errors over: their
  // blockIds would point at nothing, or at a homonymous block of the new file.
  test('opening another file clears the previous document diagnostics', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const diagnostics = new DiagnosticsPanel(page);

    await raiseWiringError(page);
    await diagnostics.waitForErrorCount(1);
    await diagnostics.open();
    await expect(diagnostics.rows).toHaveCount(1);

    await page
      .locator('#fileOpenInput')
      .setInputFiles(path.join(invalidDir, 'unconnected_input.nflow'));
    await expect(page.locator('#blockLayer > *')).toHaveCount(2, { timeout: 15_000 });

    await expect(diagnostics.rows).toHaveCount(0);
    await expect(page.locator('.nf-diagnostics-empty')).toBeVisible();
    expect(await diagnostics.errorCount()).toBe(0);
  });

  // The engine reaction to their defect is asserted in the b2b suite.
  for (const { file, blocks } of [
    { file: 'algebraic_loop.nflow', blocks: 4 },
    { file: 'unconnected_input.nflow', blocks: 2 },
  ]) {
    test(`invalid fixture ${file} loads and renders`, async ({ mockPage: page }) => {
      const app = new AppPage(page);
      await app.open();
      await page.locator('#fileOpenInput').setInputFiles(path.join(invalidDir, file));
      await expect(page.locator('#blockLayer > *')).toHaveCount(blocks, { timeout: 15_000 });
    });
  }

  // A diagnostic with a blockId selects the offending block through
  // nf-select-block; the engine-driven click-through is covered in the b2b suite.
  test('the nf-select-block wiring selects and is what the panel row triggers', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    const target = await editor.insertBlock('gain', { x: 300, y: 240 });

    await page.locator('#svgCanvas').click({ position: { x: 8, y: 8 } });
    await expect(page.locator(`#blockLayer > [data-block-id="${target}"].selected`)).toHaveCount(0);
    await page.evaluate((id) => {
      document.dispatchEvent(new CustomEvent('nf-select-block', { detail: { blockId: id } }));
    }, target);

    await expect(page.locator(`#blockLayer > [data-block-id="${target}"].selected`)).toHaveCount(1);
  });

  test('a diagnostic without a blockId is not clickable', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const diagnostics = new DiagnosticsPanel(page);

    await raiseWiringError(page); // wiring errors carry no blockId
    await diagnostics.waitForErrorCount(1);
    await diagnostics.open();

    await expect(diagnostics.rows.first().locator('.nf-diagnostic-main')).toBeDisabled();
  });
});
//=============================================================================
