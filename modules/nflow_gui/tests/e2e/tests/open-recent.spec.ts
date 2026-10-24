//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Playwright E2E tests for Open Recent menu
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Open Recent', () => {
  test('opens recent file via nflow.file.readText', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    // Seed recent list and stub host RPC readText to return a minimal diagram.
    await page.evaluate(() => {
      localStorage.setItem('nf-recent-files-v1', JSON.stringify(['/tmp/test1.nflow']));
      (window as any).__e2eReadTextBackend = async () => {
        const diagram = {
          version: '1.0.0',
          format: 'nflow-2',
          name: 'test-diagram',
          blocks: [],
          connections: [],
        };
        return JSON.stringify({ success: true, content: JSON.stringify(diagram) });
      };
    });

    await app.openMenu('file');
    const action = `openRecent:${encodeURIComponent('/tmp/test1.nflow')}`;
    await app.clickMenuAction(action);

    await page.waitForFunction(() => document.title.includes('test-diagram'), undefined, { timeout: 5000 });
    expect(await app.title()).toContain('test-diagram');
  });

  test('clear recent removes items from the menu', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    await page.evaluate(() => {
      localStorage.setItem('nf-recent-files-v1', JSON.stringify(['/a', '/b']));
    });

    await app.openMenu('file');
    await app.clickMenuAction('clearRecent');

    await app.openMenu('file');
    const content = await page.locator('#menu-open-recent-list').textContent();
    expect(content).toContain('No recently opened files.');
  });
});
//=============================================================================
