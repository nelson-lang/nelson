//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Export image tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
//=============================================================================
test.describe('Export image', () => {
  test('export to SVG action can be invoked', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await app.openMenu('file');
    await app.clickMenuAction('export-svg');

    const status = await app.statusText();
    expect(typeof status).toBe('string');
  });

  test('export to PNG downloads a .png image', async ({ mockPage: page }) => {
    // Force the anchor-download path so Playwright captures a download event
    // (the File System Access picker is blocked in headless runs).
    await page.addInitScript(() => {
      try {
        (window as unknown as { showSaveFilePicker?: unknown }).showSaveFilePicker = undefined;
      } catch (_e) {
        /* ignore */
      }
    });
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    await editor.insertBlock('gain', { x: 260, y: 220 }); // give the diagram content

    const downloadPromise = page.waitForEvent('download', { timeout: 15_000 });
    await app.openMenu('file');
    await app.clickMenuAction('export-png');

    const download = await downloadPromise;
    expect(download.suggestedFilename()).toMatch(/\.png$/i);
  });
});
//=============================================================================
