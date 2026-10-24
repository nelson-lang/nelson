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
import { showFigureBars, waitForFigureLayout } from './figureBars';
import { figureCommands } from './figureCommands';
//=============================================================================
export async function figureFitScenario(page: Page) {
  const run = figureCommands(page, 'fit-check');
  const show = async () => {
    await page.getByRole('tab', { name: /Fit subplots/ }).click();
    await waitForFigureLayout(page);
  };
  const fit = async () => {
    await page.getByRole('button', { name: 'Fit', exact: true }).click();
    await expect(page.locator('.figure-workspace-status')).not.toContainText('Pending');
  };
  const drag = async (tool: 'Pan' | 'Rotate', fx: number, dx: number, dy: number) => {
    await page.getByRole('button', { name: tool, exact: true }).click();
    const overlay = page.locator(tool === 'Pan' ? '.figure-panel__pan' : '.figure-panel__rotate');
    const box = await overlay.boundingBox();
    if (!box) throw new Error('Figure interaction surface is missing');
    const x = box.x + box.width * fx,
      y = box.y + box.height * (fx > 0.5 ? 0.3 : 0.7);
    await page.mouse.move(x, y);
    await page.mouse.down();
    await page.mouse.move(x + dx, y + dy, { steps: 12 });
    await page.mouse.up();
  };
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Fit subplots','WindowStyle','docked'); a1=subplot(2,2,1); plot(1:5); title('Signal'); a2=subplot(2,2,2); t=linspace(0,4*pi,200); plot3(cos(t),sin(t),t); title('Helix'); a3=subplot(2,2,3); surf(peaks(25)); title('Surface'); a4=subplot(2,2,4); plot(5:-1:1); title('Other'); drawnow()"
  );
  await show();
  await showFigureBars(page);
  await page.getByRole('button', { name: 'Properties', exact: true }).click();
  await waitForFigureLayout(page);
  for (const [label, ax, fx] of [
    ['Surface', 'a3', 0.3],
    ['Helix', 'a2', 0.75],
  ] as const) {
    await page.getByRole('combobox', { name: 'Axes', exact: true }).selectOption({ label });
    for (const projection of ['orthographic', 'perspective']) {
      await page
        .getByRole('combobox', { name: 'Projection', exact: true })
        .selectOption(projection);
      await page.getByRole('combobox', { name: 'View preset', exact: true }).selectOption('3d');
      await fit();
      await run(
        `target=${ax}; baseline=[target.Position target.CameraPosition target.CameraTarget target.CameraUpVector target.CameraViewAngle target.XLim target.YLim target.ZLim]; slots=[a1.Position;a2.Position;a3.Position;a4.Position]; otherLimits=[a1.XLim a1.YLim;a4.XLim a4.YLim]`
      );
      await show();
      const before = await page
        .locator('.figure-panel__canvas')
        .screenshot({ path: test.info().outputPath(`${label}-${projection}-reference.png`) });
      const box = await page.locator('.figure-panel__canvas').boundingBox();
      if (!box) throw new Error('Figure drawing area is missing');
      const selected = label === 'Surface' ? 2 : 1;
      const neighbors = [0, 1, 2, 3]
        .filter((index) => index !== selected)
        .map((index) => ({
          x: box.x + ((index % 2) * box.width) / 2,
          y: box.y + (Math.floor(index / 2) * box.height) / 2,
          width: box.width / 2,
          height: box.height / 2,
        }));
      const neighborImages = await Promise.all(neighbors.map((clip) => page.screenshot({ clip })));
      for (const [dx, dy] of [
        [40, -25],
        [-180, 130],
      ]) {
        await drag('Pan', fx, dx, dy);
        await expect(page.locator('.figure-workspace-status')).not.toContainText('Pending');
        for (let index = 0; index < neighbors.length; index++) {
          await expect
            .poll(async () =>
              (await page.screenshot({ clip: neighbors[index] })).equals(neighborImages[index])
            )
            .toBe(true);
        }
        await page.getByRole('button', { name: 'Zoom in', exact: true }).click();
        await fit();
        await run(
          "asserts.isapprox([target.Position target.CameraPosition target.CameraTarget target.CameraUpVector target.CameraViewAngle target.XLim target.YLim target.ZLim],baseline,1e-8); asserts.isequal([a1.Position;a2.Position;a3.Position;a4.Position],slots); asserts.isequal([a1.XLim a1.YLim;a4.XLim a4.YLim],otherLimits); asserts.isequal(target.CameraTargetMode,'auto')"
        );
        await show();
        await page
          .locator('.figure-panel__canvas')
          .screenshot({ path: test.info().outputPath(`${label}-${projection}-after-fit.png`) });
        await expect
          .poll(async () =>
            (await page.locator('.figure-panel__canvas').screenshot()).equals(before)
          )
          .toBe(true);
      }
      // Fit clicked immediately after rotation must keep the final orientation.
      await drag('Rotate', fx, 40, -20);
      await fit();
      await run(
        "asserts.isapprox(target.View,[-57.5 20],1e-8); asserts.isequal(target.CameraPositionMode,'auto'); asserts.isequal(target.CameraTargetMode,'auto'); asserts.isequal([a1.Position;a2.Position;a3.Position;a4.Position],slots); asserts.isequal([a1.XLim a1.YLim;a4.XLim a4.YLim],otherLimits)"
      );
      await show();
      const fitted = await page.locator('.figure-panel__canvas').screenshot();
      await fit();
      await run('asserts.isequal([a1.Position;a2.Position;a3.Position;a4.Position],slots)');
      await show();
      expect((await page.locator('.figure-panel__canvas').screenshot()).equals(fitted)).toBe(true);
    }
  }
  await run('close(f)');
}
//=============================================================================
