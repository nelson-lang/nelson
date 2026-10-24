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
import { waitForFigureLayout, showFigureBars } from './figureBars';
import { checkFigureWheel } from './figureWheelScenario';
//=============================================================================
export async function figureQuickActionsScenario(page: Page, saveLabel = 'Save as…') {
  const run = figureCommands(page, 'quick-actions');
  const show = async () => {
    await page.getByRole('tab', { name: /Quick actions|Key r/ }).click();
    await waitForFigureLayout(page);
  };
  const ready = () => expect(page.locator('.figure-workspace-status')).not.toContainText('Pending');
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Quick actions','WindowStyle','docked'); a1=subplot(2,2,1); plot(1:5); title('Signal'); a2=subplot(2,2,2); plot3([0 1 2],[0 1 0],[0 1 2]); title('Helix'); a3=subplot(2,2,3); surf(peaks(25)); title('Surface'); view(-52,20); a4=subplot(2,2,4); plot(5:-1:1); title('Other'); a1.XLim=[10 20]; slots=[a1.Position;a2.Position;a3.Position;a4.Position]; drawnow()"
  );
  await show();
  await expect(page.getByRole('group', { name: 'Figure navigation' })).toHaveCount(0);
  const panel = page.locator('.figure-panel');
  await panel.press('p');
  await expect(page.locator('.figure-panel__pan')).toBeVisible();
  const box = await page.locator('.figure-panel__canvas').boundingBox();
  if (!box) throw new Error('Figure drawing area is missing');
  const x = box.x + box.width * 0.3,
    y = box.y + box.height * 0.7;
  await page.mouse.move(x, y);
  const indicator = page.locator('.figure-axes-indicator');
  await expect(indicator).toHaveText('Surface');
  const target = await indicator.getAttribute('data-axes-handle');
  await page.mouse.down();
  try {
    await page.mouse.move(box.x + box.width * 0.75, y, { steps: 15 });
    await expect(indicator).toHaveAttribute('data-axes-handle', target!);
    await expect(indicator).toHaveText('Surface');
    await page
      .locator('.figure-panel__canvas')
      .screenshot({ path: test.info().outputPath('active-subplot.png') });
  } finally {
    await page.mouse.up();
  }
  await page.mouse.dblclick(x, y);
  await run(
    "asserts.isequal(a3.XLimMode,'auto'); asserts.isequal(a3.CameraTargetMode,'auto'); asserts.isequal(a3.View,[-52 20]); asserts.isequal(a1.XLim,[10 20]); asserts.isequal([a1.Position;a2.Position;a3.Position;a4.Position],slots)"
  );
  await show();
  await checkFigureWheel(page, run, show);
  await panel.press('p');
  await page.mouse.click(box.x + box.width * 0.3, box.y + box.height * 0.3);
  await panel.press('r');
  await expect(page.locator('.figure-panel__rotate')).toBeVisible();
  await panel.press('p');
  await expect(page.locator('.figure-panel__pan')).toBeVisible();
  await panel.press('f');
  // The confirmed state read below is queued after the shortcut's key RPC and action.
  await page.getByRole('button', { name: 'Properties', exact: true }).click();
  await expect(page.getByRole('combobox', { name: 'X limit mode', exact: true })).toHaveValue(
    'auto'
  );
  await run("asserts.isequal(a1.XLimMode,'auto'); a1.XLim=[10 20]");
  await show();
  const title = page.getByRole('textbox', { name: 'Plot title', exact: true });
  await title.fill('');
  for (const key of ['p', 'r', 'f']) await title.press(key);
  await expect(title).toHaveValue('prf');
  await expect(page.locator('.figure-panel__pan')).toBeVisible();
  await title.press('Enter');
  await run(
    "asserts.isequal(a1.XLim,[10 20]); asserts.isequal(get(a1.Title,'String'),'prf'); f.WindowKeyPressFcn=@(src,evt) set(src,'Name',['Key ' src.CurrentCharacter])"
  );
  await show();
  await panel.press('r');
  await expect(page.getByRole('tab', { name: /Key r/ })).toBeVisible();
  await expect(page.locator('.figure-panel__pan')).toBeVisible();
  await run(
    "asserts.isequal(f.CurrentCharacter,'r'); asserts.isequal(f.Name,'Key r'); f.WindowKeyPressFcn=[]"
  );
  await show();
  await showFigureBars(page);
  await ready();
  await expect(page.getByRole('button', { name: 'Pan', exact: true })).toHaveAttribute(
    'title',
    /move the view/
  );
  await expect(page.getByRole('button', { name: 'Fit', exact: true })).toHaveAttribute(
    'title',
    /keep the current orientation/
  );
  await expect(page.getByRole('button', { name: 'Export', exact: true })).toHaveAttribute(
    'title',
    /download/
  );
  await page.getByRole('button', { name: 'More figure actions', exact: true }).click();
  await expect(page.getByRole('button', { name: saveLabel, exact: true })).toHaveAttribute(
    'title',
    saveLabel === 'Save as…' ? /local file/ : /virtual filesystem/
  );
  await run('close(f)');
}
//=============================================================================
