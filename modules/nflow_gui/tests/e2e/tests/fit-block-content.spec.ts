//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';

test('fit a constant to its expression and retain attached wires on reload', async ({
  mockPage: page,
}) => {
  await new AppPage(page).open();
  const diagram = {
    format: 'nflow-2',
    name: 'Fit content',
    autoRoute: false,
    blocks: [
      {
        id: 'source',
        type: 'constant',
        x: 100,
        y: 100,
        params: { Value: 'Aircraft.Flight_Dynamics.Aerodynamics.Lift.CL_alpha' },
      },
      { id: 'scope', type: 'scope', x: 800, y: 100, params: {} },
    ],
    connections: [{ from: 'source', to: 'scope', fromIndex: 0, toIndex: 0 }],
  };
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'fit.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify(diagram)),
  });
  const block = page.locator('#blockLayer > [data-block-id="source"]');
  const width = () => block.locator('rect.nf-block').getAttribute('width').then(Number);
  const before = await width();
  await block.click({ button: 'right' });
  await page.locator('#blockContextMenu [data-action="fitBlockContent"]').click();
  await expect.poll(width).toBeGreaterThan(before);
  const fittedWidth = await width();
  const compressed = block.locator('.nf-constant-formula .svg-math-text[textLength]');
  await expect(compressed).toHaveCount(0);
  await block.click({ button: 'right' });
  await page.locator('#blockContextMenu [data-action="fitBlockContent"]').click();
  await expect.poll(width).toBe(fittedWidth);
  const portX = await block
    .locator('.port-group[data-port-type="out"] .port-hit')
    .getAttribute('cx');
  expect(Number(portX)).toBe(fittedWidth);
  await page.waitForTimeout(500);
  await page.keyboard.press('Control+z');
  await expect.poll(width).toBe(before);
  await page.keyboard.press('Control+Shift+z');
  await expect.poll(width).toBe(fittedWidth);
  const saved = await page.evaluate(() => (window as any).__nf_serializeDiagram());
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'saved.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(saved),
  });
  await expect.poll(width).toBe(fittedWidth);
  expect(
    Number(await block.locator('.port-group[data-port-type="out"] .port-hit').getAttribute('cx'))
  ).toBe(fittedWidth);
});

test('context menu labels do not become selected when dragged', async ({ mockPage: page }) => {
  await new AppPage(page).open();
  const diagram = {
    format: 'nflow-2',
    name: 'Menu',
    blocks: [{ id: 'sine', type: 'sine', x: 100, y: 100, params: {} }],
    connections: [],
  };
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'menu.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify(diagram)),
  });
  await page.locator('#blockLayer > [data-block-id="sine"]').click({ button: 'right' });
  const label = page.locator('#blockContextMenu [data-action="mirrorHorizontal"] span');
  await expect(label).toHaveCSS('user-select', 'none');
  const box = await label.boundingBox();
  if (!box) throw new Error('Menu label is not visible');
  await page.mouse.move(box.x + 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width - 2, box.y + box.height / 2, { steps: 4 });
  await page.mouse.up();
  expect(await page.evaluate(() => window.getSelection()?.toString())).toBe('');
});

test('fit gain and multiline annotation without clipping their contents', async ({
  mockPage: page,
}) => {
  await new AppPage(page).open();
  const diagram = {
    format: 'nflow-2',
    name: 'Fit labels',
    blocks: [
      {
        id: 'gain',
        type: 'gain',
        x: 100,
        y: 100,
        params: { Gain: 'Aircraft.Flight_Dynamics.Aerodynamics.Lift.CL_alpha' },
      },
      {
        id: 'note',
        type: 'comment',
        x: 100,
        y: 300,
        params: { width: 80, height: 40, CommentText: 'First line of the annotation\nSecond line' },
      },
      { id: 'scope', type: 'scope', x: 800, y: 100, params: {} },
    ],
    connections: [{ from: 'gain', to: 'scope' }],
  };
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'fit-labels.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify(diagram)),
  });
  for (const id of ['gain', 'note']) {
    const block = page.locator(`#blockLayer > [data-block-id="${id}"]`);
    const before = Number(await block.locator('rect.nf-block').getAttribute('width'));
    await block.click({ button: 'right' });
    await page.locator('#blockContextMenu [data-action="fitBlockContent"]').click();
    await expect
      .poll(async () => Number(await block.locator('rect.nf-block').getAttribute('width')))
      .toBeGreaterThan(before);
    if (id === 'gain') {
      const width = Number(await block.locator('rect.nf-block').getAttribute('width'));
      expect(
        Number(
          await block.locator('.port-group[data-port-type="out"] .port-hit').getAttribute('cx')
        )
      ).toBe(width);
    } else {
      const size = await block.locator('.comment-text').evaluate((el) => ({
        width: el.scrollWidth,
        clientWidth: el.clientWidth,
        height: el.scrollHeight,
        clientHeight: el.clientHeight,
      }));
      expect(size.width).toBeLessThanOrEqual(size.clientWidth);
      expect(size.height).toBeLessThanOrEqual(size.clientHeight);
    }
  }
});
