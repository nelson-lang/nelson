//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '@playwright/test';
//=============================================================================
test('keeps raycaster pixels while stationary and renders surface axes', async ({
  page,
}, testInfo) => {
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected: engine', {
    timeout: 60_000,
  });
  const input = page.locator('.console-input');
  await input.fill(
    'raycasterMaxFrames=800; raycasterBatch=false; raycasterStartFullscreen=false; ' +
      "raycasterFigureVisible='on'; run([nelsonroot(),'/modules/graphics/examples/retro_raycaster.m']);"
  );
  await input.press('Enter');
  await page
    .getByRole('tab', { name: /raycaster|Figure/ })
    .first()
    .click();
  const figure = page.locator('.figure-panel__canvas').first();
  await expect(figure.locator('canvas').first()).toBeVisible();
  // The unchanged scene is sent as image references while the loop stays busy.
  // A capture after evaluation returns would use a full snapshot and mask loss.
  for (let sample = 0; sample < 2; sample++) {
    await page.waitForTimeout(2000);
    const png = await figure.screenshot({ path: testInfo.outputPath(`stationary-${sample}.png`) });
    await testInfo.attach(`stationary-${sample}`, { body: png, contentType: 'image/png' });
    const coverage = await page.evaluate(async (base64) => {
      const image = new Image();
      image.src = `data:image/png;base64,${base64}`;
      await image.decode();
      const canvas = document.createElement('canvas');
      canvas.width = image.width;
      canvas.height = image.height;
      const ctx = canvas.getContext('2d')!;
      ctx.drawImage(image, 0, 0);
      const pixels = ctx.getImageData(0, 0, canvas.width, canvas.height).data;
      let bright = 0;
      for (let i = 0; i < pixels.length; i += 4) {
        if (Math.max(pixels[i], pixels[i + 1], pixels[i + 2]) > 32) bright++;
      }
      return bright / (canvas.width * canvas.height);
    }, png.toString('base64'));
    expect(coverage).toBeGreaterThan(0.1);
  }
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 60_000 });
  await page.getByText('Command Window', { exact: true }).first().click();
  await input.fill("close('all'); figure(); surf(peaks);");
  await input.press('Enter');
  await page
    .getByRole('tab', { name: /Figure/ })
    .first()
    .click();
  await expect(page.locator('canvas[data-figure-renderer="webgl2"]')).toBeVisible();
  await testInfo.attach('surface-with-axes', {
    body: await page.locator('.figure-panel__canvas').first().screenshot(),
    contentType: 'image/png',
  });
});
//=============================================================================
