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
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

test('WGS-84 function blocks display their current operation', async ({ mockPage: page }) => {
  await new AppPage(page).open();
  const root = path.resolve(
    path.dirname(fileURLToPath(import.meta.url)),
    '..',
    '..',
    '..',
    '..',
    'nflow_blocks'
  );
  const example = fs.readFileSync(
    path.join(root, 'examples', 'causal', 'WGS84_Gravity_Annotated.nflow')
  );
  await page
    .locator('#fileOpenInput')
    .setInputFiles({ name: 'wgs84.nflow', mimeType: 'application/json', buffer: example });
  const block = (id: string) => page.locator('#blockLayer > [data-block-id="' + id + '"]');
  await expect(block('sine').locator('.nf-function-symbol')).toHaveText('sin');
  await expect(block('square').locator('.nf-function-symbol')).toHaveText('u²');
  await expect(block('ratiosq').locator('.nf-function-symbol')).toHaveText('u²');
  await block('square').click();
  const field = page.locator("select[data-edit='Function']");
  await field.selectOption('sqrt');
  await expect(block('square').locator('.nf-function-symbol')).toHaveText('√u');
});
