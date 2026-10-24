//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Operations and navigation: remove, progress events, cancel, failures, deep
// link, keyboard, version picker, dependency tree, verify, cache, sources.
import { test, expect, type Page } from '@playwright/test';
import { openWindow, calls, push } from './fixtures';
//=============================================================================
const row = (page: Page, name: string) => page.locator(`#rows .row[data-name="${name}"]`);
const select = async (page: Page, name: string) => {
  await row(page, name).click();
  await expect(page.locator('.head__name')).toHaveText(name);
};
//=============================================================================
test('remove asks for confirmation, then follows the progress until done', async ({ page }) => {
  await openWindow(page);
  await select(page, 'morse');
  await page.locator('[data-a="remove"]').click();
  await page.locator('.modal [data-r="0"]').click();
  expect(await calls(page, 'remove')).toHaveLength(0);
  await page.locator('[data-a="remove"]').click();
  await page.locator('.modal .btn--danger').click();
  expect(await calls(page, 'remove')).toEqual([expect.objectContaining({ name: 'morse', version: '0.8.0' })]);
  await push(page, 'progress', { action: 'remove', name: 'morse', version: '0.8.0', step: 'remove', index: 1, total: 2 });
  await expect(page.locator('.progress')).toContainText('Step 1 of 2');
  await expect(page.locator('#status-state')).toHaveClass(/is-busy/);
  const loads = (await calls(page, 'snapshot')).length;
  await push(page, 'progress', { action: 'remove', name: 'morse', version: '0.8.0', step: 'done', index: 2, total: 2 });
  await expect(page.locator('.progress')).toHaveCount(0);
  await expect.poll(async () => (await calls(page, 'snapshot')).length).toBeGreaterThan(loads);
  await expect(page.locator('.head__name')).toHaveText('morse');
});
//=============================================================================
test('a dependency shows as a sub-step, cancel interrupts, a failure is reported', async ({ page }) => {
  await openWindow(page);
  await select(page, 'com_engine');
  await push(page, 'progress', { action: 'install', name: 'com_engine', version: '1.0.0', step: 'fetch', index: 1, total: 4 });
  await push(page, 'progress', { action: 'install', name: 'edf', version: '0.3.0', step: 'build', index: 3, total: 5 });
  await expect(page.locator('.progress__sub')).toContainText('edf');
  await page.locator('[data-a="cancel"]').click();
  expect(await calls(page, 'interrupt')).toHaveLength(1);
  await expect(page.locator('[data-a="cancel"]')).toBeDisabled();
  await push(page, 'failed', 'interrupted');
  await expect(page.locator('.progress')).toHaveCount(0);
  await expect(page.locator('.error')).toHaveCount(0);
  await push(page, 'progress', { action: 'install', name: 'com_engine', version: '1.0.0', step: 'fetch', index: 1, total: 4 });
  await push(page, 'failed', 'Registry cannot be fetched');
  await expect(page.locator('.error')).toHaveText('Registry cannot be fetched');
});
//=============================================================================
test('a deep link selects the package and starts its install', async ({ page }) => {
  await openWindow(page, { deepLink: { action: 'install', name: 'com_engine', version: '1.0.0' } });
  await expect(page.locator('.modal--preview .modal__title')).toHaveText('Install com_engine 1.0.0?');
  await expect(page.locator('.head__name')).toHaveText('com_engine');
  await page.locator('.modal--preview [data-r="1"]').click();
  expect(await calls(page, 'install')).toEqual([expect.objectContaining({ name: 'com_engine', version: '1.0.0' })]);
});
//=============================================================================
test('keyboard: slash focuses the search, arrows move, Enter installs', async ({ page }) => {
  await openWindow(page);
  await expect(row(page, 'com_engine')).toBeVisible();
  await page.locator('body').press('/');
  await expect(page.locator('#q')).toBeFocused();
  await page.locator('#rows').focus();
  await page.keyboard.press('ArrowDown');
  await expect(page.locator('.head__name')).toHaveText('morse');
  await page.keyboard.press('ArrowUp');
  await expect(page.locator('.head__name')).toHaveText('com_engine');
  await page.keyboard.press('Enter');
  await expect(page.locator('.modal--preview .modal__title')).toHaveText('Install com_engine 1.0.0?');
  await page.keyboard.press('Escape');
  await expect(page.locator('.modal--preview')).toHaveCount(0);
});
//=============================================================================
test('version picker, dependency tree and verify', async ({ page }) => {
  await openWindow(page);
  await page.locator('#q').fill('cmeng');
  await select(page, 'com_engine');
  await page.locator('#q').fill('');
  await select(page, 'morse');
  await page.locator('[data-a="tree"]').click();
  await expect(page.locator('.tree-row')).toHaveCount(2);
  await expect(page.locator('.tree-row').nth(1)).toContainText('edf');
  await page.locator('.tree-row .deplink', { hasText: 'edf' }).click();
  await select(page, 'morse');
  await page.locator('[data-a="verify"]').click();
  await expect(page.locator('.verify--err')).toContainText('module.json is missing.');
});
//=============================================================================
test('cache window lists the archives and removes one', async ({ page }) => {
  await openWindow(page);
  await page.locator('#open-cache').click();
  const dialog = page.locator('.modal--cache');
  await expect(dialog).toContainText('1 archive');
  await dialog.locator('[data-c="remove"]').click();
  expect(await calls(page, 'cacheRemove')).toEqual([{ name: 'morse', version: '1.0.0' }]);
  await dialog.locator('[data-c="close"]').click();
  await expect(dialog).toHaveCount(0);
});
//=============================================================================
test('install from a file or URL and pick a registry', async ({ page }) => {
  await openWindow(page, { noRegistry: true });
  // the panel shows when no package is selected (here: nothing matches)
  await page.locator('#q').fill('zzz');
  await expect(page.locator('#detail h2')).toHaveText('No package registry configured');
  await page.locator('#reg-url').fill('https://example.org/registry.json');
  await page.locator('#use-registry').click();
  expect(await calls(page, 'setRegistry')).toEqual([{ url: 'https://example.org/registry.json' }]);
  await page.locator('#add-source').click();
  await page.locator('.modal__input').fill('C:/pkgs/demo.nmz');
  await page.locator('.modal [data-r="1"]').click();
  expect(await calls(page, 'installSource')).toEqual([{ source: 'C:/pkgs/demo.nmz' }]);
});
//=============================================================================
test('updates filter, update all, refresh and the desktop badge count', async ({ page }) => {
  await openWindow(page);
  await expect.poll(async () => (await calls(page, 'updates')).map(c => c.count)).toContain(1);
  await expect(page.locator('#count-updates')).toHaveText('1');
  await page.locator('.seg__btn[data-filter="updates"]').click();
  await expect(page.locator('#rows .row')).toHaveCount(1);
  await page.locator('#update-all').click();
  await expect.poll(async () => await calls(page, 'update')).toEqual([expect.objectContaining({ name: 'morse', version: '1.0.0' })]);
  await page.locator('#refresh').click();
  await expect.poll(async () => (await calls(page, 'snapshot')).some(c => c.fresh === true)).toBe(true);
});
//=============================================================================
