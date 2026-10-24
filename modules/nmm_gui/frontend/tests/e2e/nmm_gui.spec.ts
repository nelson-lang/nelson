//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect, type Page } from '@playwright/test';
import { readFileSync } from 'node:fs';
import { openWindow, calls } from './fixtures';
//=============================================================================
const row = (page: Page, name: string) => page.locator(`#rows .row[data-name="${name}"]`);
const select = async (page: Page, name: string) => {
  await row(page, name).click();
  await expect(page.locator('.head__name')).toHaveText(name);
};
//=============================================================================
test('shows loading skeletons until the packages arrive', async ({ page }) => {
  await openWindow(page, { snapshotDelayMs: 1500 });
  await expect(page.locator('#rows .skeleton-row')).toHaveCount(6);
  await expect(page.locator('#detail .skeleton')).toHaveCount(3);
  await expect(page.locator('#filters')).toBeHidden();
  await expect(row(page, 'morse')).toBeVisible();
  await expect(page.locator('#rows .skeleton-row')).toHaveCount(0);
});
//=============================================================================
test('approximate search finds typos and highlights the matches', async ({ page }) => {
  await openWindow(page);
  const search = page.locator('#q');
  await search.fill('mrse');
  await expect(page.locator('#rows .row')).toHaveCount(1);
  await expect(row(page, 'morse')).toBeVisible();
  await search.fill('telegrph');
  await expect(row(page, 'morse')).toBeVisible();
  await search.fill('code');
  await expect(row(page, 'morse').locator('.row__desc mark').first()).toHaveText('code');
  await search.fill('farnsworth');
  await expect(page.locator('#rows .row')).toHaveCount(1);
});
//=============================================================================
test('filters the list by category and by license', async ({ page }) => {
  await openWindow(page);
  await expect(page.locator('#category option')).toHaveText(['All categories', 'Education', 'Interoperability', 'Signal processing']);
  await page.locator('#category').selectOption('Interoperability');
  await expect(page.locator('#rows .row')).toHaveCount(1);
  await expect(row(page, 'com_engine')).toBeVisible();
  await page.locator('#category').selectOption('');
  await page.locator('#license').selectOption('LGPL-3.0-or-later');
  await expect(page.locator('#rows .row')).toHaveCount(1);
  await expect(row(page, 'morse')).toBeVisible();
});
//=============================================================================
test('detail shows the signed registry, license warning, checksum, source, size and date', async ({ page }) => {
  await openWindow(page);
  await select(page, 'morse');
  await expect(page.locator('.head .badge', { hasText: 'Signed registry' })).toBeVisible();
  await expect(page.locator('.meta__item', { hasText: 'Size on disk' })).toContainText('1.2 MB');
  await expect(page.locator('.meta__item', { hasText: 'Installed on' })).toContainText('2026-09-20 14:05');
  await expect(page.locator('.meta__item', { hasText: 'Categories' })).toContainText('Signal processing, Education');
  await expect(page.locator('.warn-note')).toHaveCount(0);
  await select(page, 'com_engine');
  await expect(page.locator('.warn-note')).toContainText('License Proprietary is not known to be compatible');
  await expect(page.locator('.meta__item', { hasText: 'Source (win64)' }).locator('a')).toHaveAttribute('href', /com_engine-win64-1\.0\.0\.nmz$/);
  const sha = page.locator('.meta__item', { hasText: 'SHA-256' });
  await expect(sha).toContainText('5548d7ec0caf77d7...27324ef1');
  await expect(sha.locator('[data-a="copyhash"]')).toHaveAttribute('data-h', '5548d7ec0caf77d77d3007a71c632f9cda28d6d8f94ccb82e5719c1d27324ef1');
});
//=============================================================================
test('opens package links through the native browser bridge', async ({ page }) => {
  await openWindow(page);
  await select(page, 'morse');
  await page.locator('.linksrow a', { hasText: 'Repository' }).click();
  await expect.poll(async () => await calls(page, 'openExternal')).toEqual([
    { url: 'https://github.com/NelsonModules/morse' },
  ]);
});
//=============================================================================
test('install preview lists what the install does, cancel sends nothing', async ({ page }) => {
  await openWindow(page);
  await select(page, 'com_engine');
  await page.locator('[data-a="install"]').click();
  const dialog = page.locator('.modal--preview');
  await expect(dialog.locator('.modal__title')).toHaveText('Install com_engine 1.0.0?');
  await expect(dialog).toContainText('native code');
  await expect(dialog).toContainText('Download size: 4.5 MB');
  await expect(dialog).toContainText('No other package to install.');
  await dialog.locator('[data-r="0"]').click();
  await expect(dialog).toHaveCount(0);
  expect(await calls(page, 'install')).toHaveLength(0);
  await page.locator('[data-a="install"]').click();
  await page.locator('.modal--preview [data-r="1"]').click();
  expect(await calls(page, 'install')).toEqual([expect.objectContaining({ name: 'com_engine', version: '1.0.0' })]);
});
//=============================================================================
test('update preview warns about a major version and shows what is new', async ({ page }) => {
  await openWindow(page);
  await select(page, 'morse');
  await page.locator('[data-a="update"]').click();
  const dialog = page.locator('.modal--preview');
  await expect(dialog.locator('.modal__title')).toHaveText('Update morse from 0.8.0 to 1.0.0?');
  await expect(dialog).toContainText('Major version change');
  await expect(dialog.locator('.preview__deps li')).toHaveText(['edf 0.3.0']);
  await expect(dialog.locator('.preview__changes')).toContainText('First public release');
  await expect(dialog.locator('.preview__changes')).toContainText('Farnsworth timing');
  await expect(dialog.locator('.preview__changes')).not.toContainText('Initial import');
});
//=============================================================================
test('readme of a registry package comes from its repository', async ({ page }) => {
  await openWindow(page);
  await select(page, 'com_engine');
  await page.locator('[data-a="doc"][data-k="readme"]').click();
  await expect(page.locator('.doc')).toContainText('Fetched from the repository.');
  expect(await calls(page, 'doc')).toContainEqual(expect.objectContaining({
    kind: 'readme', path: '', repository: 'https://github.com/NelsonModules/com_engine.git' }));
});
//=============================================================================
test('status bar flags an offline registry copy', async ({ page }) => {
  await openWindow(page, { offline: true });
  const status = page.locator('#status-registry');
  await expect(status).toContainText('offline, cached copy');
  await expect(status).toHaveClass(/is-warn/);
  await expect(status).toHaveAttribute('title', /C:\/cache\/registry\.json/);
});
//=============================================================================
test('environment list is saved to and opened from a file', async ({ page }, testInfo) => {
  await openWindow(page);
  await page.locator('#open-env').click();
  const download = page.waitForEvent('download');
  await page.locator('.modal--env [data-e="save"]').click();
  const file = await download;
  expect(file.suggestedFilename()).toBe('nelson-packages.txt');
  const saved = testInfo.outputPath('saved.txt');
  await file.saveAs(saved);
  expect(readFileSync(saved, 'utf8').trim()).toBe('morse 0.8.0');
  await page.locator('#env-file').setInputFiles({ name: 'list.txt', mimeType: 'text/plain', buffer: Buffer.from('ngen 1.2.0\nmorse\n') });
  await expect(page.locator('#env-import')).toHaveValue('ngen 1.2.0\nmorse\n');
});
//=============================================================================
test('filter, sort and selection are restored and saved', async ({ page }) => {
  await openWindow(page, { prefs: { filter: 'installed', sort: 'updates', selected: 'morse' } });
  await expect(page.locator('.seg__btn.is-active')).toHaveAttribute('data-filter', 'installed');
  await expect(page.locator('#sort')).toHaveValue('updates');
  await expect(page.locator('.head__name')).toHaveText('morse');
  await page.locator('.seg__btn[data-filter="all"]').click();
  await expect.poll(async () => (await calls(page, 'savePrefs')).map(c => (c.prefs as Record<string, unknown>).filter)).toContain('all');
});
//=============================================================================
test('the new strings follow the Nelson language', async ({ page }) => {
  await openWindow(page, { locale: 'fr_FR' });
  await expect(page.locator('#category option').first()).toHaveText('Toutes les catégories');
  await select(page, 'morse');
  await page.locator('[data-a="update"]').click();
  await expect(page.locator('.modal--preview .modal__title')).toHaveText('Mettre à jour morse de 0.8.0 vers 1.0.0 ?');
});
//=============================================================================
