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

test('matrix Product accepts one inverse input and keeps its mode after reload', async ({
  mockPage: page,
}) => {
  await new AppPage(page).open();
  const model = {
    format: 'nflow-2',
    name: 'Matrix Product',
    autoRoute: false,
    blocks: [{ id: 'product', type: 'mult', x: 200, y: 150, params: { Inputs: 2 } }],
    connections: [],
  };
  await page
    .locator('#fileOpenInput')
    .setInputFiles({
      name: 'product.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(JSON.stringify(model)),
    });
  await page.locator('#blockLayer > [data-block-id="product"]').click();
  await page.locator('select[data-edit="Multiplication"]').selectOption('Matrix(*)');
  const input = page.locator('input[data-edit="Inputs"]');
  await input.fill('/');
  await input.dispatchEvent('change');
  const block = page.locator('#blockLayer > [data-block-id="product"]');
  await expect(block.locator('.port-group[data-port-type="in"]')).toHaveCount(1);
  const saved = await page.evaluate(() =>
    (window as unknown as { __nf_serializeDiagram: () => string }).__nf_serializeDiagram()
  );
  const savedBlock = JSON.parse(saved).blocks.find((b: { id: string }) => b.id === 'product');
  expect(savedBlock.params).toMatchObject({ Inputs: '/', Multiplication: 'Matrix(*)' });
  await page
    .locator('#fileOpenInput')
    .setInputFiles({
      name: 'saved-product.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(saved),
    });
  await expect(block.locator('.port-group[data-port-type="in"]')).toHaveCount(1);
  await block.click();
  await expect(page.locator('select[data-edit="Multiplication"]')).toHaveValue('Matrix(*)');
});
