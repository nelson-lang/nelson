//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '@playwright/test';
import { hoverWord, runLine, terminalText } from './helpers';
//=============================================================================
test('real engine: data tips at the prompt and while paused', async ({ page }) => {
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect.poll(() => terminalText(page), { timeout: 60_000 }).toContain('connected: engine');

  await runLine(page, "b2b_tip_file = fullfile(pwd, 'b2b_tip.m');");
  await runLine(
    page,
    "filewrite(b2b_tip_file, sprintf('function r = b2b_tip(x)\\ns.a = x * 2;\\nr = s.a + 1;\\nend\\n'));"
  );
  await runLine(page, 'rehash');
  await runLine(page, 'b2b_tip_value = 42;');
  await runLine(page, 'editor(b2b_tip_file)');
  const epanel = page.locator('.editor-panel').first();
  await expect(epanel.locator('.view-lines')).toContainText('b2b_tip', { timeout: 30_000 });

  // at the >> prompt: a name of the base workspace answers, a function name does not block
  await runLine(page, 'x = 7;');
  const tipAtPrompt = await hoverWord(page, 'b2b_tip.m', 'x');
  expect(tipAtPrompt).toContain('x = 7');
  await hoverWord(page, 'b2b_tip.m', 'r').catch(() => undefined);
  await runLine(page, 'disp(1 + 1)');
  await expect.poll(() => terminalText(page)).toContain('2');

  // paused at line 3: the local struct and its field answer
  await runLine(page, 'dbstop in b2b_tip at 3');
  await runLine(page, 'y = b2b_tip(5)', false);
  const bar = page.locator('.editor-debugbar');
  await expect(bar).toContainText('Paused', { timeout: 30_000 });
  // the stop reuses the tab opened with another spelling of the path, it does not add one
  await expect(page.locator('.dv-tab', { hasText: 'b2b_tip.m' })).toHaveCount(1);
  const tipPaused = await hoverWord(page, 'b2b_tip.m', 'x');
  expect(tipPaused).toContain('x = 5');
  const tipField = await hoverWord(page, 'b2b_tip.m', 'a');
  expect(tipField).toContain('s.a = 10');
  await bar.getByRole('button', { name: 'Continue', exact: true }).click();
  await expect(bar).toBeHidden({ timeout: 30_000 });
  await runLine(page, 'dbclear all');
  await runLine(page, 'disp(y)');
  await expect.poll(() => terminalText(page)).toContain('11');
});
//=============================================================================
