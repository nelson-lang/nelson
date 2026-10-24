//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as path from 'path';
import * as url from 'url';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { InspectorPanel } from '../pages/InspectorPanel.js';
//=============================================================================
const __dirname = path.dirname(url.fileURLToPath(import.meta.url));
const FIXTURE = path.resolve(__dirname, '..', 'fixtures', 'subsystem-empty.nflow');
test.describe('Inspector Subsystem - description/author and save', () => {
  test('inspector shows description and author and Save Subsystem triggers native save with fields', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    const fileInput = page.locator('#fileOpenInput');
    await fileInput.setInputFiles(FIXTURE);
    await page.waitForSelector('#blockLayer > *', { timeout: 15_000 });

    const subsystem = page.locator('#blockLayer > *').first();
    await subsystem.click();

    const inspector = new InspectorPanel(page);
    await inspector.open();

    // The inspector may render param inputs hidden initially (visibility bound),
    // wait for the elements to be attached then set their values via evaluate.
    await page.waitForSelector('textarea[data-edit="description"]', { state: 'attached', timeout: 5_000 });
    await page.waitForSelector('input[data-edit="author"]', { state: 'attached', timeout: 5_000 });

    await page.evaluate(() => {
      const d = document.querySelector('textarea[data-edit="description"]') as HTMLTextAreaElement | null;
      if (d) {
        d.value = 'E2E description';
        d.dispatchEvent(new Event('input', { bubbles: true }));
      }
      const a = document.querySelector('input[data-edit="author"]') as HTMLInputElement | null;
      if (a) {
        a.value = 'E2E Author';
        a.dispatchEvent(new Event('input', { bubbles: true }));
      }
    });

    // Saving opens the shared in-window file picker (auto-resolved by the e2e
    // fixture) and writes through nflow.file.writeText; capture that content.
    await page.evaluate(() => {
      (window as any).__lastSavedNblock = null;
      (window as any).__e2eFilePickerPath = '/tmp/e2e.nblock';
      (window as any).__e2eWriteTextBackend = async (content: string) => {
        try {
          (window as any).__lastSavedNblock = content;
        } catch (e) {
          // ignore
        }
        return JSON.stringify({ success: true });
      };
    });

    await page.click('button[data-action="save-subsystem"]');

    await page.waitForFunction(() => !!(window as any).__lastSavedNblock, undefined, { timeout: 5_000 });

    const savedStr = await page.evaluate(() => (window as any).__lastSavedNblock as string);
    const saved = JSON.parse(savedStr);

    expect(saved.description).toBe('E2E description');
    expect(saved.metadata).toBeDefined();
    expect(saved.metadata.author).toBe('E2E Author');
  });
});
//=============================================================================
