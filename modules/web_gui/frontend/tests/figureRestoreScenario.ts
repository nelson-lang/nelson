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
import { figureMenuAction, selectInspectorObject } from './figureObjects';
import { figureCommands } from './figureCommands';
import { showFigureBars, waitForFigureLayout } from './figureBars';
//=============================================================================
export async function figureRestoreScenario(page: Page) {
  const run = figureCommands(page, 'restore-view');
  const show = async () => {
    await page.getByRole('tab', { name: /Restore test/ }).click();
    await waitForFigureLayout(page);
  };
  const drag = async (tool: 'Pan' | 'Rotate', fx: number, fy: number) => {
    await page.getByRole('button', { name: tool, exact: true }).click();
    const box = await page.locator('.figure-panel__canvas').boundingBox();
    if (!box) throw new Error('Figure drawing area is missing');
    const x = box.x + box.width * fx,
      y = box.y + box.height * fy;
    await page.mouse.move(x, y);
    await page.mouse.down();
    await page.mouse.move(x + 25, y + 12, { steps: 5 });
    await page.mouse.up();
    await expect(page.locator('.figure-panel')).not.toHaveAttribute('aria-busy', 'true');
  };
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Restore test','WindowStyle','docked'); tiledlayout(2,2); a1=nexttile; plot([1 10 100],[1 10 100]); title('Log reversed'); set(a1,'XScale','log','YScale','log','XDir','reverse','YDir','reverse','XLim',[1 100],'YLim',[1 100]); a2=nexttile; plot(1:5); title('Other'); a3=nexttile([1 2]); surf(peaks(45)); title('Dense surface'); set(a3,'Projection','perspective','CameraPosition',[70 -80 50],'CameraTarget',[20 20 0],'CameraViewAngle',18); drawnow()"
  );
  await show();
  await showFigureBars(page);
  await page.getByRole('button', { name: 'Properties', exact: true }).click();
  await selectInspectorObject(page, 'axes (Dense surface)');
  await waitForFigureLayout(page);
  await run(
    'saved3=[a3.View a3.CameraPosition a3.CameraTarget a3.CameraUpVector a3.CameraViewAngle a3.XLim a3.YLim a3.ZLim]; saved2=[a2.XLim a2.YLim]; slots=[a1.Position;a2.Position;a3.Position]'
  );
  await show();
  await page.getByRole('button', { name: 'More figure actions', exact: true }).click();
  await page.getByRole('button', { name: 'Use current view as reference', exact: true }).click();
  await drag('Pan', 0.5, 0.7);
  await drag('Rotate', 0.5, 0.7);
  await figureMenuAction(page, 'Zoom in');
  await run(
    "asserts.istrue(norm([a3.View a3.CameraPosition a3.CameraTarget a3.CameraUpVector a3.CameraViewAngle a3.XLim a3.YLim a3.ZLim]-saved3)>1e-6); a3.XGrid='on'"
  );
  await show();
  await page.getByRole('button', { name: 'Restore view', exact: true }).click();
  const check3 =
    "asserts.isapprox([a3.View a3.CameraPosition a3.CameraTarget a3.CameraUpVector a3.CameraViewAngle a3.XLim a3.YLim a3.ZLim],saved3,1e-8); asserts.isequal(a3.CameraPositionMode,'manual'); asserts.isequal(a3.CameraViewAngleMode,'manual'); asserts.isequal(char(a3.XGrid),'on'); asserts.isequal([a2.XLim a2.YLim],saved2)";
  await run(check3 + '; asserts.isapprox([a1.Position;a2.Position;a3.Position],slots,1e-12)');
  await show();
  await expect
    .poll(() =>
      page
        .locator('.figure-panel__canvas canvas')
        .first()
        .evaluate((element) => {
          const source = element as HTMLCanvasElement;
          const canvas = document.createElement('canvas');
          canvas.width = source.width;
          canvas.height = source.height;
          const context = canvas.getContext('2d')!;
          context.drawImage(source, 0, 0);
          const data = context.getImageData(0, 0, canvas.width / 2, canvas.height / 2).data;
          let colored = 0;
          for (let k = 0; k < data.length; k += 4)
            if (data[k + 2] > data[k] + 40 && data[k + 3] > 0) colored++;
          return colored;
        })
    )
    .toBeGreaterThan(40);
  await page
    .locator('.figure-panel__canvas')
    .screenshot({ path: test.info().outputPath('restored-tiled-surface.png') });
  await selectInspectorObject(page, 'axes (Log reversed)');
  await drag('Pan', 0.3, 0.3);
  await run(
    'asserts.istrue(a1.XLim(1)>1); asserts.istrue(a1.YLim(1)<1); logSpan=diff(log10(a1.XLim))'
  );
  await show();
  await figureMenuAction(page, 'Zoom in');
  await run('asserts.isapprox(diff(log10(a1.XLim)),logSpan/1.25,1e-8)');
  await show();
  await page.getByRole('button', { name: 'Restore view', exact: true }).click();
  await run('asserts.isapprox(a1.XLim,[1 100],1e-8); asserts.isapprox(a1.YLim,[1 100],1e-8)');
  await show();
  await selectInspectorObject(page, 'axes (Dense surface)');
  await figureMenuAction(page, 'Zoom in');
  await expect(page.locator('.figure-panel')).not.toHaveAttribute('aria-busy', 'true');
  await page.getByRole('button', { name: 'More figure actions', exact: true }).click();
  const opening = page.waitForEvent('popup');
  await page.getByRole('button', { name: 'Undock (separate window)', exact: true }).click();
  const detached = await opening;
  await expect(detached.locator('.nelson-shell--standalone')).toBeVisible({ timeout: 60000 });
  const properties = detached.getByRole('button', { name: 'Properties', exact: true });
  if ((await properties.getAttribute('aria-expanded')) !== 'true') await properties.click();
  await selectInspectorObject(detached, 'axes (Dense surface)');
  // Restore view is a navigation tool of the toolbar, not a menu entry
  await detached.getByRole('button', { name: 'Restore view', exact: true }).click();
  await expect(detached.locator('.figure-panel')).not.toHaveAttribute('aria-busy', 'true');
  await run(check3 + '; close(f)');
  await detached.close();
}
//=============================================================================
