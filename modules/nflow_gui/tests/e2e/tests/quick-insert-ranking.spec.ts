//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Quick-insert discoverability. Typing a block name must rank the obvious
// block first (so pressing Enter inserts it) and must tolerate case and
// sub-strings. Exercises quick-insert.tsx end to end: popup -> ranked #quickInsertList
// -> Enter inserts #blockLayer child. The unit test covers scoreMatch(); this
// covers the real popup a user sees.
//=============================================================================
import { type Page } from '@playwright/test';
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
//=============================================================================
async function openQuickInsert(page: Page, query: string): Promise<void> {
  await page.locator('#svgCanvas').dblclick({ position: { x: 300, y: 240 } });
  const input = page.locator('#quickInsertInput').first();
  await expect(input).toBeVisible({ timeout: 5000 });
  await input.fill(query);
}
//=============================================================================
async function rankedTypes(page: Page): Promise<string[]> {
  const items = page.locator('#quickInsertList li');
  await expect(items.first()).toBeVisible({ timeout: 5000 });
  return items.allTextContents().then((list) => list.map((s) => s.trim()));
}
//=============================================================================
test.describe('N5 - quick-insert ranking', () => {
  const exactFirst: Array<{ query: string; type: string }> = [
    { query: 'gain', type: 'gain' },
    { query: 'scope', type: 'scope' },
    { query: 'sum', type: 'sum' },
  ];

  for (const { query, type } of exactFirst) {
    test(`"${query}" ranks "${type}" first`, async ({ mockPage: page }) => {
      const app = new AppPage(page);
      await app.open();
      await openQuickInsert(page, query);
      const ranked = await rankedTypes(page);
      expect(ranked.length).toBeGreaterThan(0);
      expect(ranked[0]).toBe(type);
    });
  }

  test('ranking is case-insensitive', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    await openQuickInsert(page, 'GAIN');
    const ranked = await rankedTypes(page);
    expect(ranked[0]).toBe('gain');
  });

  test('a sub-string query still finds the block', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    await openQuickInsert(page, 'sin');
    const ranked = await rankedTypes(page);
    expect(ranked).toContain('sine');
  });

  test('Enter inserts the top-ranked block at the cursor', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    const before = await editor.blockCount();

    await openQuickInsert(page, 'gain');
    await page.locator('#quickInsertInput').press('Enter');

    await expect(page.locator('#blockLayer > *')).toHaveCount(before + 1, { timeout: 5000 });
  });

  test('an unknown query yields an empty list without crashing', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    await openQuickInsert(page, 'zzz-not-a-block');
    await expect(page.locator('#quickInsertList li')).toHaveCount(0);
    await page.keyboard.press('Escape');
    await expect(page.locator('#quickInsertPopup')).toHaveCount(0);
  });

  test('ArrowDown moves the selection and Enter inserts the highlighted block', async ({
    mockPage: page,
  }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    const before = await editor.blockCount();

    await openQuickInsert(page, 's');
    const ranked = await rankedTypes(page);
    expect(ranked.length).toBeGreaterThan(1);
    await expect(page.locator('#quickInsertList li.selected')).toHaveText(ranked[0]);

    await page.locator('#quickInsertInput').press('ArrowDown');
    await expect(page.locator('#quickInsertList li.selected')).toHaveText(ranked[1]);
    await page.locator('#quickInsertInput').press('Enter');

    await expect(page.locator('#blockLayer > *')).toHaveCount(before + 1, { timeout: 5000 });
  });
});
//=============================================================================
