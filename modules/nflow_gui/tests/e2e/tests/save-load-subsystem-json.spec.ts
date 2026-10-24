//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const fixture = path.resolve(__dirname, '..', 'fixtures', 'external-ports.nblock');
test('loads a saved subsystem block through the file input', async ({ mockPage: page }) => {
  const app = new AppPage(page);
  await app.open();

  await page.locator('#loadSubsystemInput').setInputFiles(fixture);

  await expect(page.locator('#statusBarInfo')).toContainText('Loaded subsystem: e2e-subsystem', {
    timeout: 10_000,
  });
  const reusableBlock = page.locator(
    '#blockLibraryGroups [data-type="subsystem"][data-subsystem-key="e2e-subsystem"]'
  );
  await expect(reusableBlock).toHaveCount(1);
  await expect(reusableBlock).toContainText('e2e-subsystem');
});
//=============================================================================
