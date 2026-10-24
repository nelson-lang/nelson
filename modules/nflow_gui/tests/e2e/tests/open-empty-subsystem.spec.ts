//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
test.describe('Open empty subsystem via Inspector', () => {
  test('loading a Subsystem block fixture then using inspector Open Subsystem opens an editable empty subsystem', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const inspector = new InspectorPanel(page);

    await app.open();

    // Load a fixture containing a single Subsystem block (subsystem: null)
    const fileInput = page.locator('#fileOpenInput');
    await fileInput.setInputFiles(FIXTURE);
    await page.waitForSelector('#blockLayer > *', { timeout: 10_000 });

    const subsystemBlock = page.locator('#blockLayer > *').first();
    await subsystemBlock.click();

    await inspector.open();
    const openBtn = inspector.inspectorBody.locator("button[data-action='open-subsystem']");
    await expect(openBtn).toBeVisible({ timeout: 3000 });
    await openBtn.click();

    const upBtn = page.locator('#subsystemBreadcrumb');
    await expect(upBtn).toBeVisible({ timeout: 5_000 });
  });
});
//=============================================================================
