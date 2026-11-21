//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '@playwright/test';

test('matrix Product simulates through the real editor and engine', async ({ page }) => {
  const model = {
    format: 'nflow-2',
    name: 'Matrix Product',
    autoRoute: false,
    sampleTime: 0.01,
    runtime: 0.1,
    blocks: [
      { id: 'a', type: 'constant', x: 100, y: 100, params: { Value: [1, 3, 2, 4] } },
      { id: 'ashape', type: 'reshape', x: 260, y: 100, params: { OutputDimensions: [2, 2] } },
      { id: 'b', type: 'constant', x: 100, y: 300, params: { Value: [5, 7, 6, 8] } },
      { id: 'bshape', type: 'reshape', x: 260, y: 300, params: { OutputDimensions: [2, 2] } },
      {
        id: 'p',
        type: 'mult',
        x: 450,
        y: 190,
        params: { Inputs: '*/', Multiplication: 'Matrix(*)' },
      },
      { id: 'scope', type: 'scope', x: 600, y: 190, params: {} },
    ],
    connections: [
      { from: 'a', to: 'ashape' },
      { from: 'b', to: 'bshape' },
      { from: 'ashape', to: 'p', toIndex: 0 },
      { from: 'bshape', to: 'p', toIndex: 1 },
      { from: 'p', to: 'scope' },
    ],
  };
  await page.goto('/nflow.html');
  await page.waitForSelector('#svgCanvas');
  await page.locator('#fileOpenInput').setInputFiles({
    name: 'matrix.nflow',
    mimeType: 'application/json',
    buffer: Buffer.from(JSON.stringify(model)),
  });
  await page.locator('#blockLayer > [data-block-id="p"]').click();
  await expect(page.locator('select[data-edit="Multiplication"]')).toHaveValue('Matrix(*)');
  await page.locator('#playPauseBtn').click();
  await expect(page.locator('#statusBarInfo')).toContainText('Done', { timeout: 30_000 });
  const result = await page.evaluate(async () => {
    const host = window as unknown as {
      __nf_serializeDiagram: () => string;
      webui: { call: (...args: string[]) => Promise<string> };
    };
    const raw = await host.webui.call(
      'rpc',
      JSON.stringify({
        id: 1,
        method: 'nflow.sim.run',
        params: { diagramJson: host.__nf_serializeDiagram() },
      })
    );
    const envelope = JSON.parse(String(raw));
    if (envelope.error) throw new Error(envelope.error.message);
    return typeof envelope.result === 'string' ? JSON.parse(envelope.result) : envelope.result;
  });
  expect(result.status).toBe('done');
  expect(result.signals.p[0]).toEqual(
    expect.arrayContaining([
      expect.closeTo(3),
      expect.closeTo(2),
      expect.closeTo(-2),
      expect.closeTo(-1),
    ])
  );
});
