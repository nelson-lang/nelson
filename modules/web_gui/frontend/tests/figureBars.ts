//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, type Locator, type Page } from '@playwright/test';
//=============================================================================
export async function showFigureBars(figure: Page | Locator) {
  for (const name of ['Navigation toolbar', 'Plot details bar']) {
    await figure.getByRole('button', { name: 'More figure actions', exact: true }).click();
    const toggle = figure.getByRole('button', { name, exact: true });
    if ((await toggle.getAttribute('aria-pressed')) === 'false') await toggle.click();
    else await figure.getByRole('button', { name: 'More figure actions', exact: true }).click();
  }
  await waitForFigureLayout(figure);
}
//=============================================================================
export async function waitForFigureLayout(figure: Page | Locator) {
  // Showing a bar resizes the drawing area; wait for both engine dimensions.
  await expect
    .poll(
      () =>
        figure
          .locator('.figure-panel__canvas > div')
          .first()
          .evaluate((element) => {
            const style = (element as HTMLElement).style;
            return Math.max(
              Math.abs(parseFloat(style.width) - element.parentElement!.clientWidth),
              Math.abs(parseFloat(style.height) - element.parentElement!.clientHeight)
            );
          }),
      { timeout: 15000 }
    )
    .toBeLessThan(1);
}
//=============================================================================
