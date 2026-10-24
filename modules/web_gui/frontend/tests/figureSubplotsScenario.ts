//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, type Page } from '@playwright/test';
import { showFigureBars, waitForFigureLayout } from './figureBars';
import { figureOutputScenario } from './figureOutputScenario';
//=============================================================================
export async function figureSubplotsScenario(page: Page, saveLabel = 'Save as…') {
  let sequence = 0;
  const run = async (command: string) => {
    await page.getByRole('tab', { name: 'Command Window', exact: true }).first().click();
    const marker = `subplot-check-${++sequence}`;
    await page.locator('.console-input').fill(`${command}; disp('${marker}');`);
    await page.locator('.console-input').press('Enter');
    await expect
      .poll(
        async () => {
          await page.getByRole('tab', { name: 'Command Window', exact: true }).first().click();
          return page
            .locator('.console-line')
            .filter({ hasText: new RegExp(`^${marker}$`) })
            .count();
        },
        { timeout: 60000 }
      )
      .toBe(1);
    await expect(page.locator('.statusbar__state')).toHaveText('Ready');
  };
  const show = async () => {
    await page.getByRole('tab', { name: /Subplots test/ }).click();
  };
  const drag = async (selector: string, dx: number, dy: number, fy = 0.7) => {
    const box = await page.locator(selector).boundingBox();
    if (!box) throw new Error('Figure interaction surface is missing');
    // Surface axes are the lower-left subplot; CurrentAxes is lower-right.
    const x = box.x + box.width * 0.3;
    const y = box.y + box.height * fy;
    await page.mouse.move(x, y);
    await page.mouse.down();
    await page.mouse.move(x + dx, y + dy, { steps: 5 });
    await page.mouse.up();
    await expect
      .poll(() => page.locator('.figure-workspace-status').innerText())
      .not.toContain('Pending');
  };
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Subplots test','WindowStyle','docked'); a1=subplot(2,2,1); plot(1:5); a2=subplot(2,2,2); plot3([0 1 2],[0 1 0],[0 1 2]); a3=subplot(2,2,3); surf(peaks(25)); title('Surface peaks'); a4=subplot(2,2,4); plot(5:-1:1); camva(a3,15); drawnow(); p0=a3.CameraPosition; t0=a3.CameraTarget; others=[a1.CameraPosition;a2.CameraPosition;a4.CameraPosition]"
  );
  await show();
  await expect(page.getByRole('group', { name: 'Figure navigation' })).toHaveCount(0);
  await expect(page.locator('.figure-workspace-legend')).toHaveCount(0);
  await expect(page.locator('.figure-workspace-status')).toHaveCount(0);
  await showFigureBars(page);
  await run(
    'limits1=[a1.XLim a1.YLim]; limitsOthers=[a2.XLim a2.YLim;a3.XLim a3.YLim;a4.XLim a4.YLim]'
  );
  await show();
  await page.getByRole('button', { name: 'Pan', exact: true }).click();
  await drag('.figure-panel__pan', 30, 15, 0.3);
  await run(
    'asserts.istrue(norm([a1.XLim a1.YLim]-limits1)>1e-6); asserts.isequal([a2.XLim a2.YLim;a3.XLim a3.YLim;a4.XLim a4.YLim],limitsOthers); p0=a3.CameraPosition; t0=a3.CameraTarget; others=[a1.CameraPosition;a2.CameraPosition;a4.CameraPosition]'
  );
  await show();
  await page.getByRole('button', { name: 'Pan', exact: true }).click();
  await drag('.figure-panel__pan', 30, 15);
  await run(
    'asserts.istrue(norm(a3.CameraPosition-p0)>1e-6); asserts.isapprox(a3.CameraPosition-p0,a3.CameraTarget-t0,1e-8); asserts.isequal([a1.CameraPosition;a2.CameraPosition;a4.CameraPosition],others); panPosition=a3.CameraPosition; panTarget=a3.CameraTarget; panAngle=a3.CameraViewAngle; panView=a3.View'
  );
  await show();
  await page.getByRole('button', { name: 'Rotate', exact: true }).click();
  await drag('.figure-panel__rotate', 50, -20);
  await run(
    'asserts.istrue(norm(a3.CameraPosition-panPosition)>1e-6); asserts.isequal(a3.CameraTarget,panTarget); asserts.isapprox(a3.CameraViewAngle,panAngle,1e-8); asserts.isapprox(a3.View,panView+[-25 -10],1e-8); asserts.isequal([a1.CameraPosition;a2.CameraPosition;a4.CameraPosition],others)'
  );

  await run(
    'view(a1,3.5,87); drawnow(); firstPosition=a1.CameraPosition; firstTarget=a1.CameraTarget'
  );
  await show();
  await page.getByRole('button', { name: 'Pan', exact: true }).click();
  const beforePan = await page.locator('.figure-panel canvas').first().screenshot();
  await drag('.figure-panel__pan', 30, 15, 0.3);
  await expect
    .poll(async () =>
      (await page.locator('.figure-panel canvas').first().screenshot()).equals(beforePan)
    )
    .toBe(false);
  await run(
    'asserts.istrue(norm(a1.CameraPosition-firstPosition)>1e-6); asserts.isapprox(a1.CameraPosition-firstPosition,a1.CameraTarget-firstTarget,1e-8)'
  );

  // Filled-to-wireframe must keep labels from all four subplots visible.
  await show();
  await page.getByRole('button', { name: 'Properties', exact: true }).click();
  await page
    .getByRole('combobox', { name: 'Axes', exact: true })
    .selectOption({ label: 'Surface peaks' });
  await waitForFigureLayout(page);
  const labels = page.locator('canvas[data-figure-layer="text-overlay"]');
  for (const faceColor of ['none', 'flat']) {
    const color = page.getByRole('textbox', { name: 'FaceColor', exact: true });
    await color.fill(faceColor);
    await color.press('Enter');
    await expect(color).toHaveValue(faceColor);
    await expect(page.locator('.figure-workspace-status')).not.toContainText('Pending');
    await run(`asserts.isequal(get(findobj(a3,'Type','surface'),'FaceColor'),'${faceColor}')`);
    await show();
    await waitForFigureLayout(page);
    await expect(labels).toBeVisible();
    await expect
      .poll(() =>
        labels.evaluate((element) => {
          const canvas = element as HTMLCanvasElement;
          const ctx = canvas.getContext('2d')!;
          const data = ctx.getImageData(0, 0, canvas.width, canvas.height).data;
          const counts = [0, 0, 0, 0];
          for (let y = 0; y < canvas.height; y++)
            for (let x = 0; x < canvas.width; x++) {
              if (data[(y * canvas.width + x) * 4 + 3] > 0)
                counts[(y < canvas.height / 2 ? 0 : 2) + (x < canvas.width / 2 ? 0 : 1)]++;
            }
          return Math.min(...counts);
        })
      )
      .toBeGreaterThan(100);
  }
  await figureOutputScenario(page, run, saveLabel);
}
//=============================================================================
