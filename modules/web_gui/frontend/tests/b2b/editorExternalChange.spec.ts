//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
import { runLine, showTab, terminalText } from './helpers';
//=============================================================================
const TAB = 'b2b_ext.m';
//=============================================================================
async function typeAtEnd(page: Page, text: string) {
  await showTab(page, TAB);
  const lines = page.locator('.editor-panel:visible .monaco-editor .view-lines').first();
  await lines.click();
  await page.keyboard.press('Control+End');
  await page.keyboard.type(text);
}
//=============================================================================
test('real engine: editor follows files changed on disk and reports save failures', async ({
  page,
}) => {
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-panel')).toBeVisible();
  await expect.poll(() => terminalText(page), { timeout: 60_000 }).toContain('connected: engine');

  await runLine(page, "b2b_ext_dir = fullfile(tempdir(), 'b2b_ext_dir'); mkdir(b2b_ext_dir);");
  await runLine(page, "b2b_ext_file = fullfile(b2b_ext_dir, 'b2b_ext.m');");
  await runLine(page, "filewrite(b2b_ext_file, sprintf('b2b_first=1;\\n'));");
  await runLine(page, 'editor(b2b_ext_file)');
  const epanel = page.locator('.editor-panel').first();
  const text = epanel.locator('.view-lines');
  const banner = epanel.locator('.editor-notice--disk');
  await expect(epanel).toBeVisible({ timeout: 30_000 });
  await expect(text).toContainText('b2b_first=1;', { timeout: 30_000 });

  // no unsaved edits: a write from the prompt reloads the buffer directly
  await runLine(page, "filewrite(b2b_ext_file, sprintf('b2b_second=2;\\n'));");
  await showTab(page, TAB);
  await expect(text).toContainText('b2b_second=2;', { timeout: 30_000 });
  await expect(epanel.locator('.editor-notice', { hasText: 'reloaded from disk' })).toBeVisible();
  await expect(banner).toBeHidden();

  // unsaved edits: the editor asks; Keep leaves the buffer as it is
  await typeAtEnd(page, 'b2b_mine=3;');
  await runLine(page, "filewrite(b2b_ext_file, sprintf('b2b_third=3;\\n'));");
  await showTab(page, TAB);
  await expect(banner).toBeVisible({ timeout: 30_000 });
  await expect(banner).toContainText('b2b_ext.m');
  await banner.getByRole('button', { name: 'Keep', exact: true }).click();
  await expect(banner).toBeHidden();
  await expect(text).toContainText('b2b_mine=3;');
  await expect(text).not.toContainText('b2b_third=3;');
  // kept: the same version on disk does not ask again
  await runLine(page, 'b2b_noop = 1;');
  await showTab(page, TAB);
  await expect(banner).toBeHidden();

  // a newer write asks again; Reload takes the disk text
  await runLine(page, "filewrite(b2b_ext_file, sprintf('b2b_fourth=4;\\n'));");
  await showTab(page, TAB);
  await expect(banner).toBeVisible({ timeout: 30_000 });
  await banner.getByRole('button', { name: 'Reload', exact: true }).click();
  await expect(banner).toBeHidden();
  await expect(text).toContainText('b2b_fourth=4;');
  await expect(text).not.toContainText('b2b_mine=3;');

  // the editor's own save is not an external change
  await typeAtEnd(page, 'b2b_saved=5;');
  await page.keyboard.press('Control+s');
  await page.waitForTimeout(500);
  await runLine(
    page,
    "disp(['saved_ok_', num2str(contains(fileread(b2b_ext_file), 'b2b_saved=5;'))])"
  );
  await expect.poll(() => terminalText(page)).toContain('saved_ok_1');
  await showTab(page, TAB);
  await expect(banner).toBeHidden();
  await expect(epanel.locator('.editor-notice', { hasText: 'reloaded from disk' })).toHaveCount(0);

  // deleted on disk: the text stays, without any question
  await runLine(page, 'delete(b2b_ext_file); rmdir(b2b_ext_dir);');
  await showTab(page, TAB);
  await page.waitForTimeout(500);
  await expect(banner).toBeHidden();
  await expect(text).toContainText('b2b_saved=5;');

  // the folder is gone: saving reports why and keeps the buffer modified
  await typeAtEnd(page, 'b2b_lost=6;');
  await page.keyboard.press('Control+s');
  const failure = epanel.locator('.editor-notice', { hasText: 'Cannot write file' });
  await expect(failure).toBeVisible({ timeout: 30_000 });
  await expect(failure).toContainText('b2b_ext.m');
  await expect(failure).toContainText('The folder does not exist.');
  await expect(text).toContainText('b2b_lost=6;');

  // the folder is back: the same buffer saves
  await runLine(page, 'mkdir(b2b_ext_dir);');
  await showTab(page, TAB);
  await epanel.locator('.monaco-editor .view-lines').click();
  await page.keyboard.press('Control+s');
  await page.waitForTimeout(500);
  await runLine(
    page,
    "disp(['resaved_ok_', num2str(contains(fileread(b2b_ext_file), 'b2b_lost=6;'))])"
  );
  await expect.poll(() => terminalText(page)).toContain('resaved_ok_1');
  await runLine(page, 'delete(b2b_ext_file); rmdir(b2b_ext_dir);');
});
//=============================================================================
