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
// Heavy demos (tens of thousands of mesh vertices) take well over a minute in
// the WebAssembly engine on a loaded machine, and the prompt returns only once
// the last frame is serialized. Both waits use the same budget instead of the
// 5 s expect default.
const COMMAND_TIMEOUT_MS = 120000;
export function figureCommands(page: Page, prefix: string) {
  let sequence = 0;
  return async (command: string) => {
    for (const panel of await page.locator('.figure-panel:visible').all()) {
      await expect(panel).not.toHaveAttribute('aria-busy', 'true');
    }
    await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
    const marker = `${prefix}-${++sequence}`;
    const input = page.getByRole('textbox', { name: 'Command input', exact: true });
    await input.fill(`${command}; disp('${marker}');`);
    await input.press('Enter');
    await expect
      .poll(
        async () => {
          await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
          if (
            await page
              .locator('.console-line')
              .filter({ hasText: /^(Error[: ]|Nelson:[A-Za-z])/ })
              .count()
          ) {
            throw new Error(
              (await page.locator('.console-line').allTextContents()).slice(-10).join('\n')
            );
          }
          return page
            .locator('.console-line')
            .filter({ hasText: new RegExp(`^${marker}$`) })
            .count();
        },
        { timeout: COMMAND_TIMEOUT_MS }
      )
      .toBe(1);
    await expect(page.locator('.statusbar__state')).toHaveText('Ready', {
      timeout: COMMAND_TIMEOUT_MS,
    });
  };
}
//=============================================================================
