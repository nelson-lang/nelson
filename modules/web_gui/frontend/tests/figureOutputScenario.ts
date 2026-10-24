//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, type Page } from '@playwright/test';
//=============================================================================
export async function figureOutputScenario(
  page: Page,
  run: (command: string) => Promise<void>,
  saveLabel: string
) {
  // Observe print invocation without opening the operating system dialog.
  await page.context().addInitScript(() => {
    window.print = () => {
      document.documentElement.dataset.printRequested = 'yes';
    };
  });
  await page.getByRole('button', { name: 'More figure actions', exact: true }).click();
  const opening = page.waitForEvent('popup');
  await page.getByRole('button', { name: 'Print figure', exact: true }).click();
  const printPreview = await opening;
  await expect(printPreview.getByRole('button', { name: 'Print', exact: true })).toBeEnabled();
  await expect(printPreview.getByRole('status')).toHaveText('Figure ready to print');
  await expect(printPreview.locator('html')).toHaveAttribute('data-print-requested', 'yes');
  expect(
    await printPreview
      .locator('img')
      .evaluate((element) => (element as HTMLImageElement).naturalWidth)
  ).toBeGreaterThan(0);
  await printPreview.close();
  await page.getByRole('button', { name: 'More figure actions', exact: true }).click();
  const detaching = page.waitForEvent('popup');
  await page.getByRole('button', { name: 'Undock (separate window)', exact: true }).click();
  const detached = await detaching;
  await expect(detached.locator('.nelson-shell--standalone')).toBeVisible({ timeout: 60000 });
  await detached.getByRole('button', { name: 'More figure actions', exact: true }).click();
  await detached.getByRole('button', { name: saveLabel, exact: true }).click();
  const saveDialog = detached.getByRole('dialog');
  await expect(saveDialog).toBeVisible();
  const savedName = `subplot-undocked-${Date.now()}.svg`;
  await saveDialog.getByRole('textbox').fill(savedName);
  await saveDialog.getByRole('button', { name: 'OK', exact: true }).click();
  await expect(saveDialog).toBeHidden();
  // the export is written by the engine after the dialog closes: give it a bounded wait
  await run(
    `saveStart=tic(); while ~isfile('${savedName}') && toc(saveStart) < 30, pause(0.2); end; asserts.istrue(isfile('${savedName}')); asserts.istrue(contains(fileread('${savedName}'),'<svg')); close(f)`
  );
  await detached.close();
  await page.bringToFront();
}
//=============================================================================
