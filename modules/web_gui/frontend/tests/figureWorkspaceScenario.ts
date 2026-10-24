//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { readFileSync } from 'node:fs';
import { expect, type Page } from '@playwright/test';
import { showFigureBars } from './figureBars';
//=============================================================================
export async function figureWorkspaceScenario(page: Page) {
  page.setDefaultTimeout(15000);
  let sequence = 0;
  const transcript = page.locator('[aria-label="Command Window transcript"]');
  const figure = page.locator('.figure-panel').first();
  const showFigure = async () => {
    await page
      .getByRole('tab', { name: /Hélice|Figure \d/ })
      .first()
      .click();
    await expect(figure).toBeVisible();
  };
  const run = async (command: string) => {
    await page.getByRole('tab', { name: 'Command Window', exact: true }).first().click();
    const marker = `figure-workspace-ok-${++sequence}`;
    await page.locator('.console-input').fill(`${command}; disp('${marker}');`);
    await page.locator('.console-input').press('Enter');
    await expect
      .poll(
        async () => {
          await page.getByRole('tab', { name: 'Command Window', exact: true }).first().click();
          return transcript
            .locator('.console-line')
            .filter({ hasText: new RegExp(`^${marker}$`) })
            .count();
        },
        { timeout: 60000 }
      )
      .toBe(1);
    await expect(page.locator('.statusbar__state')).toHaveText('Ready', { timeout: 60000 });
  };
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Hélice','WindowStyle','docked'); ax=axes('Parent',f); t=linspace(0,4*pi,151); h=plot3(ax,cos(t),sin(t),t); h.DisplayName='Helix'; grid(ax,'on')"
  );
  await showFigure();
  const maximize = figure.getByRole('button', { name: 'Full window', exact: true });
  if (await maximize.count()) await maximize.click();
  await showFigureBars(figure);
  await figure.getByRole('button', { name: 'Properties', exact: true }).click();
  const inspector = figure.getByRole('complementary', { name: 'Figure properties' });
  const browser = inspector.getByRole('tree', { name: 'Object browser' });
  await expect(browser.getByRole('button', { name: 'line (Helix)', exact: true })).toBeVisible();
  expect(await inspector.evaluate((element) => element.scrollWidth <= element.clientWidth)).toBe(
    true
  );
  // the title text object is listed under its string once it has one
  await run("title(ax,'Titre')");
  await showFigure();
  await browser.getByRole('button', { name: 'text (Titre)', exact: true }).click();
  const titleString = inspector.getByLabel('String', { exact: true });
  await titleString.fill('Titre édité');
  await titleString.press('Enter');
  await expect(titleString).toHaveValue('Titre édité');
  await browser.getByRole('button', { name: 'line (Helix)', exact: true }).click();
  // the text object has a LineWidth too: wait for a line only property before editing
  await expect(inspector.getByLabel('Marker', { exact: true })).toBeVisible();
  const lineWidth = inspector.getByLabel('LineWidth', { exact: true });
  await lineWidth.fill('3');
  await lineWidth.press('Enter');
  await expect(lineWidth).toHaveValue('3');
  await run(
    "asserts.isequal(ax.Title.String,'Titre édité'); asserts.isequal(h.LineWidth,3); view(ax,12,40); h.LineWidth=2.5; drawnow()"
  );
  await showFigure();
  await expect(lineWidth).toHaveValue('2.5');
  await expect(inspector.getByRole('slider', { name: /Azimuth/ })).toHaveValue('12');
  await expect(inspector.getByRole('slider', { name: /Elevation/ })).toHaveValue('40');
  await figure.getByRole('combobox', { name: 'View preset' }).selectOption('xy');
  await expect(inspector.getByRole('slider', { name: /Elevation/ })).toHaveValue('90');
  await run('asserts.isequal(ax.View,[0 90])');
  await showFigure();
  await inspector.getByRole('button', { name: 'Reset camera', exact: true }).click();
  await expect(inspector.getByRole('slider', { name: /Elevation/ })).toHaveValue('30');
  await inspector.getByLabel('Projection onto floor').click();
  await expect(inspector.getByLabel('Projection onto floor')).toBeChecked();
  await figure.getByRole('button', { name: 'Pan', exact: true }).click();
  const box = await figure.locator('.figure-panel__pan').boundingBox();
  if (!box) throw new Error('Pan surface is missing');
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 40, box.y + box.height / 2 + 20, { steps: 5 });
  await page.mouse.up();
  await expect(figure.locator('.figure-workspace-status')).not.toContainText('Pending');
  await run(
    "asserts.isequal(ax.CameraPositionMode,'manual'); asserts.isequal(ax.CameraTargetMode,'manual'); asserts.isequal(numel(findall(ax,'Tag','__web_floor_projection__')),1)"
  );
  await showFigure();
  await figure.getByRole('button', { name: 'Fit', exact: true }).click();
  await expect(figure.locator('.figure-workspace-status')).not.toContainText('Pending');
  await figure.getByRole('button', { name: 'Helix', exact: true }).click();
  await expect(figure.getByRole('button', { name: 'Helix', exact: true })).toHaveAttribute(
    'aria-pressed',
    'false'
  );
  await run(
    "asserts.isequal(char(h.Visible),'off'); h.Visible='on'; f.Name='Synchro'; f.NumberTitle='off'; drawnow()"
  );
  await page.getByRole('tab', { name: 'Synchro', exact: true }).click();
  await expect(figure.locator('.figure-workspace-title')).toContainText('Synchro');
  await expect(figure.getByRole('button', { name: 'Helix', exact: true })).toHaveAttribute(
    'aria-pressed',
    'true'
  );
  for (const format of ['png', 'svg', 'pdf']) {
    await figure.getByRole('button', { name: 'Export', exact: true }).click();
    const menu = figure.getByRole('group', { name: 'Export figure', exact: true });
    await menu.getByLabel('Format', { exact: true }).selectOption(format);
    if (format === 'png') await menu.getByLabel('Resolution', { exact: true }).selectOption('2');
    const downloading = page.waitForEvent('download', { timeout: 60000 });
    await menu
      .getByRole('button', { name: `Download ${format.toUpperCase()}`, exact: true })
      .click();
    const download = await downloading;
    const path = await download.path();
    const bytes = readFileSync(path!);
    if (format === 'png') expect(bytes.subarray(0, 8).toString('hex')).toBe('89504e470d0a1a0a');
    if (format === 'svg') expect(bytes.toString('utf8')).toContain('<svg');
    if (format === 'pdf') expect(bytes.subarray(0, 4).toString()).toBe('%PDF');
  }
  await run('close(f)');
  await expect(page.getByRole('tab', { name: 'Synchro', exact: true })).toHaveCount(0);
  await run(
    "pickFigure=figure('Name','Point inspection'); pickAxes=axes('Parent',pickFigure,'Position',[0.1 0.1 0.8 0.8]); line('Parent',pickAxes,'XData',[0 5 10],'YData',[0 5 10]); set(pickAxes,'XLim',[0 10],'YLim',[0 10]); drawnow()"
  );
  await page
    .getByRole('tab', { name: /Point inspection/ })
    .first()
    .click();
  await showFigureBars(figure);
  await expect(figure.getByRole('button', { name: 'Inspect', exact: true })).toHaveAttribute(
    'aria-pressed',
    'true'
  );
  const drawing = figure.locator('.figure-panel__canvas');
  await expect(drawing.locator('canvas').first()).toBeVisible();
  await expect
    .poll(() =>
      drawing
        .locator(':scope > div')
        .first()
        .evaluate((element) =>
          Math.abs(
            parseFloat((element as HTMLElement).style.width) - element.parentElement!.clientWidth
          )
        )
    )
    .toBeLessThan(1);
  const drawingBox = await drawing.boundingBox();
  if (!drawingBox) throw new Error('Inspection surface is missing');
  // a click that lands before the first frame is drawn picks nothing: click again until it does
  await expect
    .poll(async () => {
      await page.mouse.click(
        drawingBox.x + drawingBox.width / 2,
        drawingBox.y + drawingBox.height / 2
      );
      return figure.locator('.figure-workspace-status').innerText();
    })
    .toContain('Point 2 · X 5 · Y 5');
  await expect(figure.locator('.figure-inspected-point')).toBeVisible();
  await run('close(pickFigure)');
}
//=============================================================================
