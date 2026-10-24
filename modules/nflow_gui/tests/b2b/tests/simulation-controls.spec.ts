//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '@playwright/test';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
//=============================================================================
const currentDirectory = path.dirname(fileURLToPath(import.meta.url));
const example = path.resolve(
  currentDirectory,
  '..',
  '..',
  '..',
  '..',
  'nflow_blocks',
  'examples',
  'causal',
  'PID_Demo.nflow'
);
//=============================================================================
test('the Run control starts and completes a native simulation', async ({ page }) => {
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles(example);
  await page.waitForSelector('#blockLayer > *');

  const runButton = page.locator('#playPauseBtn');
  await expect(runButton).toHaveAttribute('data-action', 'run');
  await runButton.click();
  await expect(runButton).toHaveAttribute('data-action', 'pause');
  await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: 30_000 });
  await expect(runButton).toHaveAttribute('data-action', 'run');
});
//=============================================================================
