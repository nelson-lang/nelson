//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, type Page } from '@playwright/test';
import { figureCommands } from './figureCommands';
import { waitForFigureLayout } from './figureBars';
//=============================================================================
function examplePath(file: string, portable: boolean): string {
  return portable
    ? `'/modules/graphics/examples/${file}'`
    : `[modulepath('graphics','root') '/examples/${file}']`;
}
//=============================================================================
export async function figureUiLayoutScenario(page: Page, portable = false) {
  const run = figureCommands(page, 'ui-layout');
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    `close('all'); run(${examplePath(
      'uicontrol/complete_uicontrol_demo.m',
      portable
    )}); figureHandle.WindowStyle='docked'`
  );
  await page.getByRole('tab', { name: /Complete UI controls/ }).click();
  const properties = page.getByRole('button', { name: 'Properties', exact: true });
  for (let cycle = 0; cycle < 2; cycle++) {
    await properties.click();
    await waitForFigureLayout(page);
    const drawing = page.locator('.figure-panel__canvas');
    const width = await drawing.evaluate((element) => element.clientWidth);
    // flush the figure's queued SizeChangedFcn first so its async output cannot interleave with the prompt and split the marker line
    await run(
      `drawnow(); asserts.istrue(state.axes.Position(1)*${width}>=329); asserts.istrue(state.statusText.Position(1)+state.statusText.Position(3)<=${width}+1)`
    );
    await page.getByRole('tab', { name: /Complete UI controls/ }).click();
  }
  await run("close('all')");
}
//=============================================================================
export async function figureMeshRotationScenario(page: Page, portable = false) {
  const run = figureCommands(page, 'mesh-rotation');
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  for (const demo of ['stanford-bunny/stanford_bunny.m', 'utah-teapot/utah_teapot.m']) {
    await run(
      `close('all'); run(${examplePath(
        demo,
        portable
      )}); f=gcf(); f.Name='Mesh rotation'; f.WindowStyle='docked'; ax=gca(); initialView=ax.View`
    );
    await page.getByRole('tab', { name: /Mesh rotation/ }).click();
    // The panel reports "2D plot" until its display list is in; with a 55k
    // vertex mesh that list can take a while on a loaded machine.
    await expect(page.locator('.figure-workspace-title')).toContainText('3D plot', {
      timeout: 60000,
    });
    await page.locator('.figure-panel').focus();
    await page.keyboard.press('r');
    const overlay = page.locator('.figure-panel__rotate');
    await expect(overlay).toBeVisible({ timeout: 60000 });
    const box = await overlay.boundingBox();
    if (!box) throw new Error('Mesh rotation surface is missing');
    await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
    await page.mouse.down();
    await page.mouse.move(box.x + box.width / 2 + 30, box.y + box.height / 2 - 10, { steps: 12 });
    await page.mouse.up();
    await run(
      "drawnow(); pause(0.05); asserts.istrue(norm(ax.View-initialView)>1); asserts.isequal(char(ax.Visible),'off'); close(f)"
    );
  }
}
//=============================================================================
