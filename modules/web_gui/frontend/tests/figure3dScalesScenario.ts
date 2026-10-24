//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test, type Page } from '@playwright/test';
import { figureCommands } from './figureCommands';
import { showFigureBars, waitForFigureLayout } from './figureBars';
//=============================================================================
// Read the composed image: surfaces can live in a separate WebGL canvas.
async function coloredPixels(page: Page, fraction: number) {
  const png = await page
    .locator('.figure-panel__canvas')
    .screenshot({ style: '.figure-axes-indicator { visibility: hidden; }' });
  return page.evaluate(
    async ({ png, fraction }) => {
      const image = new Image();
      image.src = 'data:image/png;base64,' + png;
      await image.decode();
      const copy = document.createElement('canvas');
      copy.width = image.width;
      copy.height = image.height;
      const context = copy.getContext('2d')!;
      context.drawImage(image, 0, 0);
      const pixels = context.getImageData(
        fraction < 0.5 ? 0 : copy.width / 2,
        0,
        copy.width / 2,
        copy.height
      ).data;
      let colored = 0;
      for (let k = 0; k < pixels.length; k += 4)
        if (
          pixels[k + 3] &&
          Math.max(pixels[k], pixels[k + 1], pixels[k + 2]) -
            Math.min(pixels[k], pixels[k + 1], pixels[k + 2]) >
            40
        )
          colored++;
      return colored;
    },
    { png: png.toString('base64'), fraction }
  );
}
//=============================================================================
export async function figure3dScalesScenario(page: Page) {
  const run = figureCommands(page, '3d-scales');
  const show = async () => {
    await page.getByRole('tab', { name: /3D scales/ }).click();
    await waitForFigureLayout(page);
  };
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','3D scales','WindowStyle','docked'); [x,y,z]=peaks(18); a=subplot(1,2,1); surf(10.^((x+3)/3),10.^((y+3)/3),10.^((z+7)/7)); title('Log surface'); set(a,'XScale','log','YScale','log','ZScale','log','XDir','reverse','ZDir','reverse'); b=subplot(1,2,2); plot3([1 10 100],[1 100 10],[10 1 100]); title('Log curve'); set(b,'XScale','log','YScale','log','ZScale','log','YDir','reverse'); slots=[a.Position;b.Position]; drawnow()"
  );
  await show();
  await showFigureBars(page);
  await page.getByRole('button', { name: 'Properties', exact: true }).click();
  for (const [label, variable, fraction] of [
    ['Log surface', 'a', 0.25],
    ['Log curve', 'b', 0.75],
  ] as const) {
    await page.getByRole('combobox', { name: 'Axes', exact: true }).selectOption({ label });
    for (const projection of ['orthographic', 'perspective']) {
      await page
        .getByRole('combobox', { name: 'Projection', exact: true })
        .selectOption(projection);
      await run(
        `set(${variable},'CameraPosition',[8 -9 7],'CameraTarget',[1 1 1],'CameraViewAngle',18); drawnow(); original=[${variable}.View ${variable}.CameraPosition ${variable}.CameraTarget ${variable}.CameraViewAngle ${variable}.XLim ${variable}.YLim ${variable}.ZLim]`
      );
      await show();
      await page.getByRole('button', { name: 'More figure actions', exact: true }).click();
      await page
        .getByRole('button', { name: 'Use current view as reference', exact: true })
        .click();
      await expect.poll(() => coloredPixels(page, fraction)).toBeGreaterThan(20);
      const snapshot = await page.locator('.figure-panel__canvas').screenshot();
      await page.getByRole('button', { name: 'Pan', exact: true }).click();
      const box = await page.locator('.figure-panel__canvas').boundingBox();
      if (!box) throw new Error('Figure drawing area is missing');
      const px = box.x + box.width * fraction,
        py = box.y + box.height * 0.5;
      await page.mouse.move(px, py);
      await page.mouse.down();
      await page.mouse.move(px + 25, py + 10, { steps: 6 });
      await page.mouse.up();
      await expect(page.locator('.figure-panel')).not.toHaveAttribute('aria-busy', 'true');
      const moved = await page.locator('.figure-panel__canvas').screenshot();
      expect(moved.equals(snapshot)).toBe(false);
      await page.getByRole('button', { name: 'Zoom in', exact: true }).click();
      await run(
        `asserts.istrue(norm([${variable}.CameraPosition ${variable}.CameraTarget]-original(3:8))>1e-6); asserts.istrue(all(isfinite([${variable}.XLim ${variable}.YLim ${variable}.ZLim]))); asserts.istrue(all([${variable}.XLim ${variable}.YLim ${variable}.ZLim]>0)); asserts.isequal([a.Position;b.Position],slots)`
      );
      await show();
      await page.getByRole('button', { name: 'Fit', exact: true }).click();
      await page.getByRole('button', { name: 'Restore view', exact: true }).click();
      await run(
        `asserts.isapprox([${variable}.View ${variable}.CameraPosition ${variable}.CameraTarget ${variable}.CameraViewAngle ${variable}.XLim ${variable}.YLim ${variable}.ZLim],original,1e-8); asserts.isequal([a.Position;b.Position],slots)`
      );
      await show();
    }
  }
  await page
    .locator('.figure-panel__canvas')
    .screenshot({ path: test.info().outputPath('logarithmic-reversed-3d.png') });
  await run('close(f)');
}
//=============================================================================
