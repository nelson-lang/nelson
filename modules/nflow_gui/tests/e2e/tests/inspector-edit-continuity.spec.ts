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

test('sum input accepts consecutive keystrokes across port rebuilds', async ({
  mockPage: page,
}) => {
  await new AppPage(page).open();
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'sum-edit.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(
      JSON.stringify({
        format: 'nflow-2',
        autoRoute: false,
        blocks: [{ id: 'sum', type: 'sum', x: 150, y: 150, params: { Inputs: '++' } }],
        connections: [],
      })
    ),
  });
  await page.locator('#blockLayer > [data-block-id="sum"]').click();
  const input = page.locator('#inspectorBody input[data-edit="Inputs"]');
  await expect(input).toHaveValue('++');
  await input.focus();
  await input.press('End');
  await page.keyboard.type('+++');
  await expect(input).toHaveValue('+++++');
  await expect(input).toBeFocused();
  await expect(page.locator('#blockLayer .port-group[data-port-type="in"]')).toHaveCount(5);
});

test('duplicate shortcut preserves saved signal names and offsets', async ({ mockPage: page }) => {
  await new AppPage(page).open();
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'duplicate-labels.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(
      JSON.stringify({
        format: 'nflow-2',
        autoRoute: false,
        blocks: [
          { id: 'source', type: 'constant', x: 100, y: 100, params: { Value: 4 } },
          { id: 'scope', type: 'scope', x: 400, y: 200, params: {} },
        ],
        connections: [
          { from: 'source', to: 'scope', label: 'altitude', labelOffset: { x: 10, y: -5 } },
        ],
      })
    ),
  });
  await expect(page.locator('#blockLayer > [data-block-id]')).toHaveCount(2);
  await page.locator('#svgCanvas').click({ position: { x: 8, y: 8 } });
  await page.keyboard.press('Control+a');
  await page.keyboard.press('Control+d');
  await expect(page.locator('#blockLayer > [data-block-id]')).toHaveCount(4);
  const saved = JSON.parse(
    await page.evaluate(() =>
      (window as unknown as { __nf_serializeDiagram: () => string }).__nf_serializeDiagram()
    )
  );
  expect(saved.connections).toHaveLength(2);
  for (const connection of saved.connections) {
    expect(connection.label).toBe('altitude');
    expect(connection.labelOffset).toEqual({ x: 10, y: -5 });
  }
  await expect(page.locator('#overlayLayer .wire-label')).toHaveCount(2);
});
