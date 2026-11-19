//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Performance smoke tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Performance smoke', () => {
  test('app opens within a reasonable time', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const start = Date.now();
    await app.open();
    const elapsed = Date.now() - start;

    // Loose threshold for slow CI machines.
    expect(elapsed).toBeLessThan(30_000);
  });

  test('a large diagram loads, renders and stays responsive', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();

    // A 40-block chain: constant -> gain x38 -> scope, 39 wires.
    const N = 40;
    const blocks: Array<Record<string, unknown>> = [
      { id: 'c0', type: 'constant', x: 40, y: 80, rotation: 0, params: { Value: 1 } },
    ];
    for (let i = 1; i < N - 1; i += 1) {
      blocks.push({ id: `g${i}`, type: 'gain', x: 40 + i * 120, y: 80, rotation: 0, params: { Gain: 1 } });
    }
    blocks.push({ id: 'sN', type: 'scope', x: 40 + (N - 1) * 120, y: 80, rotation: 0, params: {} });
    const ids = blocks.map((b) => b.id as string);
    const connections = ids
      .slice(0, -1)
      .map((from, i) => ({ from, to: ids[i + 1], fromIndex: 0, toIndex: 0 }));
    const diagram = {
      version: '1.0.0',
      format: 'nflow-2',
      name: 'large',
      blocks,
      connections,
      autoRoute: true,
      sampleTime: 0.01,
      runtime: 10,
    };

    const start = Date.now();
    await page.locator('#fileOpenInput').setInputFiles({
      name: 'large.nflow',
      mimeType: 'application/json',
      buffer: Buffer.from(JSON.stringify(diagram)),
    });
    await expect(page.locator('#blockLayer > *')).toHaveCount(N, { timeout: 20_000 });
    await expect
      .poll(() => page.locator('#wireLayer path.wire').count(), { timeout: 10_000 })
      .toBe(N - 1);
    const elapsed = Date.now() - start;
    expect(elapsed).toBeLessThan(20_000);

    // A whole-diagram operation stays responsive: select all.
    await page.keyboard.press('Control+a');
    await expect(page.locator('#blockLayer .svg-block.selected')).toHaveCount(N);
  });
});
//=============================================================================
