//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import path from 'path';
import { fileURLToPath } from 'url';
import { expect, test } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { LibraryPanel } from '../pages/LibraryPanel.js';
//=============================================================================
const here = path.dirname(fileURLToPath(import.meta.url));
const gallery = path.resolve(here, '..', '..', 'dashboard', 'dashboard-gallery.nflow');
async function loadGallery(page: import('@playwright/test').Page): Promise<void> {
  const app = new AppPage(page);
  await app.open();
  await page.locator('#fileOpenInput').setInputFiles(gallery);
  await expect(page.locator('#blockLayer .dashboard-block')).toHaveCount(20);
}
//=============================================================================
test('hydrates the 20 dashboard SVG icons in the block library', async ({ mockPage: page }) => {
  await page.setViewportSize({ width: 1440, height: 2200 });
  const app = new AppPage(page);
  await app.open();
  const library = new LibraryPanel(page);
  await library.open();
  await page.locator('#blockLibrarySearch').fill('dashboard');

  const tools = page.locator('#blockLibraryGroups .tool[data-type^="dashboard"]');
  await expect(tools).toHaveCount(20);
  await expect
    .poll(() =>
      tools.evaluateAll((items) =>
        items.every((item) => {
          const svg = item.querySelector('svg[data-library-icon]');
          return svg !== null && svg.childElementCount > 0 && svg.hasAttribute('viewBox');
        })
      )
    )
    .toBe(true);

  // Each dashboard block ships its own icon: assert the 20 rendered SVGs are
  // all distinct so a regression collapsing them to a shared/blank glyph fails.
  const icons = await tools.evaluateAll((items) =>
    items.map((item) => item.querySelector('svg[data-library-icon]')?.innerHTML ?? '')
  );
  expect(icons).toHaveLength(20);
  expect(icons.every((markup) => markup.length > 0)).toBe(true);
  expect(new Set(icons).size).toBe(20);
});
//=============================================================================
test('combo box text has full height and a canvas click releases its focus', async ({
  mockPage: page,
}) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await loadGallery(page);
  const combo = page.locator('[data-block-id="d20"]');
  const select = combo.locator('.dashboard-combo-select');
  const foreign = combo.locator('.dashboard-combo-foreign');

  await expect(foreign).toHaveAttribute('y', '0');
  await expect(foreign).toHaveAttribute('height', '25');
  const dimensions = await select.evaluate((element) => {
    const selectElement = element as HTMLSelectElement;
    const bounds = selectElement.getBoundingClientRect();
    return {
      height: bounds.height,
      clientHeight: selectElement.clientHeight,
      scrollHeight: selectElement.scrollHeight,
    };
  });
  expect(dimensions.height).toBeGreaterThan(0);
  expect(dimensions.scrollHeight).toBeLessThanOrEqual(dimensions.clientHeight);

  await select.focus();
  await expect(select).toBeFocused();
  const canvas = await page.locator('#svgCanvas').boundingBox();
  expect(canvas).not.toBeNull();
  await page.mouse.click((canvas?.x ?? 0) + 10, (canvas?.y ?? 0) + 10);
  await expect(select).not.toBeFocused();
});
//=============================================================================
test('dashboard scope clips values outside its configured limits', async ({ mockPage: page }) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await loadGallery(page);
  const scope = page.locator('[data-block-id="d01"]');
  await page.evaluate(() => {
    document.dispatchEvent(
      new CustomEvent('nf-dashboard-update', {
        detail: {
          id: 'd01',
          connected: true,
          valid: true,
          value: 0,
          series: [[10, 0, -10]],
          times: [0, 1, 2],
        },
      })
    );
  });

  const result = await scope.evaluate((element) => {
    const path = element.querySelector('.dashboard-scope-path');
    const coordinates = Array.from(
      path?.getAttribute('d')?.matchAll(/[ML] [\d.]+ ([\d.]+)/g) ?? []
    );
    return {
      clipPath: path?.getAttribute('clip-path'),
      yValues: coordinates.map((match) => Number(match[1])),
    };
  });
  expect(result.clipPath).toMatch(/^url\(#nflow-dashboard-scope-clip-/);
  expect(result.yValues).toEqual([18, 80.5, 143]);
});
//=============================================================================
for (const viewport of [
  { name: 'desktop', width: 1440, height: 900 },
  { name: 'mobile', width: 390, height: 844 },
]) {
  test(`renders all dashboard blocks without geometry overlap on ${viewport.name}`, async ({
    mockPage: page,
  }) => {
    await page.setViewportSize(viewport);
    await loadGallery(page);
    const boxes = await page.locator('#blockLayer .dashboard-block').evaluateAll((blocks) =>
      blocks.map((block) => {
        const rect = block.getBoundingClientRect();
        return { left: rect.left, top: rect.top, right: rect.right, bottom: rect.bottom };
      })
    );
    const toolbar = await page.locator('#floatingToolbar').boundingBox();
    expect(toolbar).not.toBeNull();
    for (let i = 0; i < boxes.length; i += 1) {
      expect(boxes[i].right).toBeGreaterThan(boxes[i].left);
      expect(boxes[i].bottom).toBeGreaterThan(boxes[i].top);
      const toolbarOverlaps =
        toolbar !== null &&
        boxes[i].left < toolbar.x + toolbar.width &&
        boxes[i].right > toolbar.x &&
        boxes[i].top < toolbar.y + toolbar.height &&
        boxes[i].bottom > toolbar.y;
      expect(toolbarOverlaps, `dashboard block ${i + 1} overlaps the simulation toolbar`).toBe(
        false
      );
      for (let j = i + 1; j < boxes.length; j += 1) {
        const overlaps =
          boxes[i].left < boxes[j].right &&
          boxes[i].right > boxes[j].left &&
          boxes[i].top < boxes[j].bottom &&
          boxes[i].bottom > boxes[j].top;
        expect(overlaps, `dashboard blocks ${i + 1} and ${j + 1} overlap`).toBe(false);
      }
    }
  });
}
//=============================================================================
