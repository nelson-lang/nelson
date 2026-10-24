//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test } from '@playwright/test';
//=============================================================================
test('WebGL preserves translucent face coverage and source-over overlap', async ({ page }) => {
  await page.goto('/tests/render_compare/browser.html?canvas2d=main');
  await page.waitForFunction(() => !!window.renderComparisonFrame);
  await page.evaluate(() => {
    const identity = [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
    const square = (id: number, left: number, right: number, color: number[]) => ({
      id,
      revision: '1',
      positions: [left, -1, 0, right, -1, 0, right, 1, 0, left, 1, 0],
      vertexColors: [...color, ...color, ...color, ...color],
      triangleIndices: [0, 1, 2, 0, 2, 3],
      lineIndices: [],
      model: identity,
      projection: identity,
      viewport: [0, 0, 96, 64] as [number, number, number, number],
      lineColor: [0, 0, 0, 1],
      lineWidth: 1,
      drawFaces: true,
    });
    window.renderComparisonFrame({
      handle: 1,
      width: 96,
      height: 64,
      primitives: [{ kind: 'rect', x: 0, y: 0, w: 96, h: 64, fill: '#fff' }],
      webglMeshes: [square(0, -1, 0.5, [1, 0, 0, 0.4]), square(1, -0.5, 1, [0, 0, 1, 0.4])],
    });
  });
  await page.waitForFunction(() => window.renderComparisonReady());
  const png = await page.locator('#root').screenshot();
  const samples = await page.evaluate(async (base64) => {
    const image = new Image();
    image.src = `data:image/png;base64,${base64}`;
    await image.decode();
    const canvas = document.createElement('canvas');
    canvas.width = image.width;
    canvas.height = image.height;
    const context = canvas.getContext('2d')!;
    context.drawImage(image, 0, 0);
    return [12, 48, 84].map((x) => [...context.getImageData(x, 32, 1, 1).data].slice(0, 3));
  }, png.toString('base64'));
  // Red over white; blue over red over white; blue over white.
  const expected = [
    [255, 153, 153],
    [153, 92, 194],
    [153, 153, 255],
  ];
  for (let sample = 0; sample < expected.length; sample++) {
    for (let channel = 0; channel < 3; channel++) {
      expect(Math.abs(samples[sample][channel] - expected[sample][channel])).toBeLessThanOrEqual(2);
    }
  }
});
//=============================================================================
