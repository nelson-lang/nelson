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
import { cursorLineText, runLine, showTab, terminalText } from './helpers';
//=============================================================================
test('real engine: editor breakpoints, conditions, stepping and error stops', async ({ page }) => {
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect.poll(() => terminalText(page), { timeout: 60_000 }).toContain('connected: engine');

  // fixture written by the engine in its working folder
  await runLine(page, "b2b_dbg_file = fullfile(pwd, 'b2b_dbg.m');");
  await runLine(
    page,
    "filewrite(b2b_dbg_file, sprintf('function r = b2b_dbg(x)\\nr = x + 1;\\nr = r * 2;\\nif r > 100\\n  r = undefined_b2b + r;\\nend\\nend\\n'));"
  );
  await runLine(page, 'rehash');
  await runLine(page, 'editor(b2b_dbg_file)');
  const epanel = page.locator('.editor-panel').first();
  await expect(epanel).toBeVisible({ timeout: 30_000 });
  await expect(epanel.locator('.view-lines')).toContainText('b2b_dbg', { timeout: 30_000 });

  // F12 on line 3 sets a breakpoint the evaluator lists
  await epanel.locator('.monaco-editor .view-lines').click();
  await page.keyboard.press('Control+Home');
  await page.keyboard.press('ArrowDown');
  await page.keyboard.press('ArrowDown');
  await page.keyboard.press('F12');
  await expect(epanel.locator('.bp-glyph')).toHaveCount(1);
  await runLine(page, "b2b_status = dbstatus(); disp(['bp line ', num2str(b2b_status.line)])");
  await expect.poll(() => terminalText(page)).toContain('bp line 3');

  // gutter menu: a bad condition is refused, a valid one turns the marker orange
  await showTab(page, 'b2b_dbg.m');
  await epanel
    .locator('.margin-view-overlays .line-numbers', { hasText: /^3$/ })
    .first()
    .click({ button: 'right' });
  const popover = page.getByTestId('breakpoint-popover');
  await expect(popover).toBeVisible();
  await popover.getByRole('button', { name: 'Set/Modify Condition...' }).click();
  await page.getByTestId('breakpoint-condition-input').fill('x >>> 2');
  await popover.getByRole('button', { name: 'Apply' }).click();
  await expect(popover).toContainText("The condition 'x >>> 2' contains a syntax error.");
  await page.getByTestId('breakpoint-condition-input').fill('x > 1');
  await popover.getByRole('button', { name: 'Apply' }).click();
  await expect(popover).toBeHidden();
  await expect(epanel.locator('.bp-glyph--conditional')).toHaveCount(1);

  // false condition: the call completes without a pause
  await runLine(page, 'b2b_y1 = b2b_dbg(1)');
  await expect.poll(() => terminalText(page)).toContain('b2b_y1 =');
  await expect(page.locator('.editor-debugbar')).toBeHidden();

  // true condition: paused on line 3 with the K>> prompt and the green line
  await runLine(page, 'b2b_y2 = b2b_dbg(2)', false);
  const bar = page.locator('.editor-debugbar');
  await expect(bar).toContainText('Paused', { timeout: 30_000 });
  await expect(bar).toContainText('b2b_dbg:3');
  await showTab(page, 'b2b_dbg.m');
  await expect(epanel.locator('.debug-current-line')).toHaveCount(1);
  // the stop opened this editor: the cursor sits on the paused line, not on line 1
  await expect.poll(() => cursorLineText(page)).toBe('r = r * 2;');
  await showTab(page, 'Command Window');
  await expect.poll(() => terminalText(page)).toContain('K>>');
  // a stop reached from a typed command echoes the line in the console
  await expect.poll(() => terminalText(page)).toContain('3   r = r * 2;');
  await showTab(page, 'b2b_dbg.m');
  await bar.getByRole('button', { name: 'Step', exact: true }).click();
  await expect(bar).toContainText('b2b_dbg:4');
  // a step driven from the editor moves the arrow without echoing the line
  await showTab(page, 'Command Window');
  expect(await terminalText(page)).not.toContain('4   if r > 100');
  await showTab(page, 'b2b_dbg.m');
  await bar.getByRole('button', { name: 'Continue', exact: true }).click();
  await expect(bar).toBeHidden({ timeout: 30_000 });
  await showTab(page, 'Command Window');
  await expect.poll(() => terminalText(page)).toContain('b2b_y2 =');

  // dbclear in the console clears the gutter; dbstop if error pauses with stepping disabled
  await runLine(page, 'dbclear all');
  await showTab(page, 'b2b_dbg.m');
  await expect(epanel.locator('.bp-glyph')).toHaveCount(0);
  await runLine(page, 'dbstop if error');
  await runLine(page, 'b2b_y3 = b2b_dbg(100)', false);
  await expect(bar).toContainText('Paused on error', { timeout: 30_000 });
  await expect(bar).toContainText('undefined_b2b');
  await expect(bar.getByRole('button', { name: 'Step', exact: true })).toBeDisabled();
  await showTab(page, 'b2b_dbg.m');
  await expect(epanel.locator('.debug-current-line--error')).toHaveCount(1);
  await bar.getByRole('button', { name: 'Stop', exact: true }).click();
  await expect(bar).toBeHidden({ timeout: 30_000 });
  await runLine(page, 'dbclear all');
});
//=============================================================================
