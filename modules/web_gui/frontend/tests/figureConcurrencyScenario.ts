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
import { selectInspectorObject } from './figureObjects';
import { figureCommands } from './figureCommands';
import { showFigureBars, waitForFigureLayout } from './figureBars';
//=============================================================================
export async function figureConcurrencyScenario(page: Page) {
  const run = figureCommands(page, 'held-rotation');
  const errors: string[] = [];
  page.on('pageerror', (error) => errors.push(error.message));
  const show = async () => {
    await page.getByRole('tab', { name: /Concurrent navigation/ }).click();
    await waitForFigureLayout(page);
  };
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Concurrent navigation','WindowStyle','docked'); a=subplot(1,2,1); surf(peaks(25)); title('Surface'); b=subplot(1,2,2); plot(1:5); a.XLim=[2 20]; saved=[a.View a.CameraPosition a.CameraTarget a.XLim a.YLim a.ZLim]; slots=[a.Position;b.Position]; drawnow()"
  );
  await show();
  await showFigureBars(page);
  await page.getByRole('button', { name: 'Properties', exact: true }).click();
  await selectInspectorObject(page, 'axes (Surface)');
  await page.getByRole('button', { name: 'Rotate', exact: true }).click();
  const azimuth = page.getByRole('slider', { name: 'Azimuth', exact: true });
  const initial = await azimuth.inputValue();
  const hold = async () => {
    const box = await page.locator('.figure-panel__canvas').boundingBox();
    if (!box) throw new Error('Figure drawing area is missing');
    const x = box.x + box.width * 0.25,
      y = box.y + box.height * 0.5;
    await page.mouse.move(x, y);
    await page.mouse.down();
    await page.mouse.move(x + 30, y + 15, { steps: 6 });
    return { x, y };
  };
  // Fit via keyboard while the primary button remains held.
  let point = await hold();
  await page.keyboard.press('f');
  await expect(page.getByRole('combobox', { name: 'XLimMode', exact: true })).toHaveValue('auto');
  const fitted = await azimuth.inputValue();
  expect(fitted).not.toBe(initial);
  await page.mouse.move(point.x + 70, point.y + 40);
  await page.mouse.up();
  await expect(azimuth).toHaveValue(fitted);
  await run(
    "asserts.isequal(a.XLimMode,'auto'); asserts.isapprox([a.Position;b.Position],slots,1e-12)"
  );
  await show();
  // Restore via keyboard activation of the button, without releasing the drag.
  point = await hold();
  const restore = page.getByRole('button', { name: 'Restore view', exact: true });
  await restore.focus();
  await page.keyboard.press('Enter');
  await expect(azimuth).toHaveValue(initial);
  await page.mouse.move(point.x + 70, point.y + 40);
  await page.mouse.up();
  await expect(azimuth).toHaveValue(initial);
  await run(
    'asserts.isapprox([a.View a.CameraPosition a.CameraTarget a.XLim a.YLim a.ZLim],saved,1e-8); asserts.isapprox([a.Position;b.Position],slots,1e-12)'
  );
  await show();
  point = await hold();
  await page.getByRole('tab', { name: /Concurrent navigation/ }).click({ button: 'right' });
  await page.getByText('Close panel', { exact: true }).click();
  await page.mouse.up();
  await expect(page.getByRole('tab', { name: /Concurrent navigation/ })).toHaveCount(0);
  await run(
    "asserts.isequal(isgraphics(f),false); f=figure('Name','Replacement figure','WindowStyle','docked'); surf(peaks(10)); drawnow()"
  );
  await page.getByRole('tab', { name: /Replacement figure/ }).click();
  await waitForFigureLayout(page);
  await expect(page.locator('.figure-panel__canvas canvas').first()).toBeVisible();
  await run('asserts.istrue(isgraphics(f)); close(f)');
  expect(errors).toEqual([]);
}
//=============================================================================
