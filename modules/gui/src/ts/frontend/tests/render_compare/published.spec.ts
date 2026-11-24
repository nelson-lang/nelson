//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test, type Page, type TestInfo } from '@playwright/test';
import { readdir } from 'node:fs/promises';
import path from 'node:path';
import { checkModernUi } from './modernUi';
//=============================================================================
test.beforeEach(async ({ page }) => {
  // Exercise a rebuilt frontend against the published runtime without deploying it.
  const frontend = process.env.NELSON_RENDER_FRONTEND_DIR;
  if (!frontend) return;
  // The published service worker normally adds isolation headers, but also
  // serves cached scripts before Playwright can substitute the local bundle.
  const base = new URL(process.env.NELSON_RENDER_TEST_URL!);
  const isolationHeaders = {
    'cross-origin-opener-policy': 'same-origin',
    'cross-origin-embedder-policy': 'require-corp',
  };
  await page.route(
    (url) =>
      url.origin === base.origin &&
      (url.pathname === base.pathname || /\.(?:js|mjs)$/.test(url.pathname)),
    async (route) => {
      const response = await route.fetch();
      await route.fulfill({
        response,
        headers: {
          ...response.headers(),
          ...isolationHeaders,
        },
      });
    }
  );
  await page.route('**/app.js*', (route) =>
    route.fulfill({
      path: path.join(frontend, 'app.js'),
      contentType: 'application/javascript',
      headers: isolationHeaders,
    })
  );
  const assets = path.join(frontend, 'assets');
  const worker = (await readdir(assets)).find((name) => /^figureCanvasWorker-.*\.js$/.test(name));
  if (!worker) throw new Error('Rebuilt frontend is missing the figure canvas worker.');
  await page.route('**/assets/figureCanvasWorker-*.js', (route) =>
    route.fulfill({
      path: path.join(assets, worker),
      contentType: 'application/javascript',
      headers: isolationHeaders,
    })
  );
});
//=============================================================================
async function expectAxes(page: Page, testInfo: TestInfo, label: string): Promise<void> {
  const image = await page
    .locator('.figure-panel__canvas')
    .first()
    .screenshot({
      path: testInfo.outputPath(`${label}.png`),
    });
  await testInfo.attach(label, { body: image, contentType: 'image/png' });
  const darkTicks = await page.evaluate(async (base64) => {
    const image = new Image();
    image.src = `data:image/png;base64,${base64}`;
    await image.decode();
    const canvas = document.createElement('canvas');
    canvas.width = image.width;
    canvas.height = image.height;
    const context = canvas.getContext('2d')!;
    context.drawImage(image, 0, 0);
    // Tick labels at the left of the default axes lie outside the mesh. A
    // visible WebGL surface alone must not satisfy this pixel assertion.
    const pixels = context.getImageData(
      0,
      Math.round(image.height * 0.1),
      Math.round(image.width * 0.12),
      Math.round(image.height * 0.8)
    ).data;
    let dark = 0;
    for (let i = 0; i < pixels.length; i += 4) {
      if (Math.max(pixels[i], pixels[i + 1], pixels[i + 2]) < 100) dark++;
    }
    return dark;
  }, image.toString('base64'));
  expect(darkTicks).toBeGreaterThan(50);
}
//=============================================================================
test('WASM keeps surf(peaks) axes at rest, after tab changes and resize', async ({
  page,
}, testInfo) => {
  await page.goto('./?figureProfile=1');
  const input = page.locator('.console-input');
  await expect(input).toBeVisible({ timeout: 120_000 });
  await expect(page.locator('.statusbar')).toHaveText(/Ready$/, { timeout: 120_000 });
  await input.fill('surf(peaks);');
  await input.press('Enter');
  const figureTab = page.getByRole('tab', { name: 'Figure 1', exact: true });
  await expect(figureTab).toBeVisible({ timeout: 60_000 });
  await figureTab.click();
  await expect(page.locator('canvas[data-figure-renderer="webgl2"]')).toBeVisible();
  await page.waitForTimeout(1500);
  await expectAxes(page, testInfo, 'initial-axes');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await figureTab.click();
  await page.waitForTimeout(1500);
  await expectAxes(page, testInfo, 'axes-after-tab');
  await page.setViewportSize({ width: 1400, height: 1000 });
  await page.waitForTimeout(1500);
  await expectAxes(page, testInfo, 'axes-after-resize');
});
//=============================================================================
test('WASM modern UI retains Qt layout, dials and callbacks after resize', async ({
  page,
}, info) => {
  await page.goto('./?canvas2d=main');
  await checkModernUi(page, info);
});
//=============================================================================
