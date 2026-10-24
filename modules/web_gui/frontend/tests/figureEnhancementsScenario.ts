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
import { showFigureBars, waitForFigureLayout } from './figureBars';
//=============================================================================
export async function figureEnhancementsScenario(page: Page) {
  page.setDefaultTimeout(20000);
  const run = figureCommands(page, 'enhancement-check');
  const figure = page.locator('.figure-panel').first();
  const show = async () => {
    await page
      .getByRole('tab', { name: /Workspace enhancements/ })
      .first()
      .click();
    await expect(figure).toBeVisible();
    await waitForFigureLayout(figure);
  };
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Workspace enhancements','WindowStyle','docked'); ax=axes('Parent',f,'Position',[.1 .1 .8 .8]); h=line('Parent',ax,'XData',[0 5 10],'YData',[0 5 10],'DisplayName','Signal'); h2=line('Parent',ax,'XData',[0 5 10],'YData',[8 8 8],'DisplayName','Other'); h3=line('Parent',ax,'XData',[0 5 10],'YData',[2 2 2],'DisplayName','Hidden','Visible','off'); set(ax,'XLim',[0 10],'YLim',[0 10])"
  );
  await show();
  const maximize = figure.getByRole('button', { name: 'Full window', exact: true });
  if (await maximize.count()) await maximize.click();
  await showFigureBars(figure);
  await figure.getByRole('button', { name: 'Properties', exact: true }).click();
  await waitForFigureLayout(figure);
  const inspector = figure.getByRole('complementary', { name: 'Figure properties' });
  await inspector.getByLabel('Object', { exact: true }).selectOption({ label: 'Signal' });
  await inspector.getByLabel('Line style', { exact: true }).selectOption('--');
  await expect(inspector.getByLabel('Line style', { exact: true })).toHaveValue('--');
  await figure.getByRole('button', { name: 'Undo figure edit' }).click();
  await expect(inspector.getByLabel('Line style', { exact: true })).toHaveValue('-');
  await figure.focus();
  await figure.press('Control+Shift+z');
  await expect(inspector.getByLabel('Line style', { exact: true })).toHaveValue('--');
  await inspector.getByLabel('Marker', { exact: true }).selectOption('o');
  await inspector.getByLabel('Marker size', { exact: true }).fill('9');
  await inspector.getByLabel('Marker size', { exact: true }).press('Enter');
  await expect(inspector.getByLabel('Marker size', { exact: true })).toHaveValue('9');
  await inspector.getByLabel('Color picker', { exact: true }).fill('#00ff00');
  await expect(inspector.getByLabel('Color', { exact: true })).toHaveValue('0, 1, 0');
  const legend = figure.getByLabel('Plotted objects', { exact: true });
  await legend.getByRole('button', { name: 'Signal', exact: true }).dblclick();
  await expect(legend.getByRole('button', { name: 'Other', exact: true })).toHaveAttribute(
    'aria-pressed',
    'false'
  );
  await legend.getByRole('button', { name: 'Signal', exact: true }).dblclick();
  await expect(legend.getByRole('button', { name: 'Other', exact: true })).toHaveAttribute(
    'aria-pressed',
    'true'
  );
  await expect(legend.getByRole('button', { name: 'Hidden', exact: true })).toHaveAttribute(
    'aria-pressed',
    'false'
  );
  await run(
    "asserts.isequal(h.LineStyle,'--'); asserts.isequal(h.Marker,'o'); asserts.isequal(h.MarkerSize,9); asserts.isequal(h.Color,[0 1 0]); asserts.isequal(char(h2.Visible),'on'); asserts.isequal(char(h3.Visible),'off')"
  );
  await show();
  await figure.getByRole('button', { name: 'Inspect', exact: true }).click();
  const canvas = figure.locator('.figure-panel__canvas');
  const box = await canvas.boundingBox();
  if (!box) throw new Error('Missing drawing area');
  await page.mouse.click(box.x + box.width / 2, box.y + box.height / 2);
  const tip = figure.locator('.figure-point-tip');
  await expect(tip).toContainText('Signal · Point 2');
  await expect(tip).toContainText('X 5 · Y 5 · Z 0');
  await figure.press('ArrowRight');
  await expect(tip).toContainText('Point 3');
  await figure.press('ArrowLeft');
  await expect(tip).toContainText('Point 2');
  const separator = inspector.getByRole('separator', { name: 'Properties width' });
  const previous = Number(await separator.getAttribute('aria-valuenow'));
  await separator.focus();
  await separator.press('ArrowLeft');
  await expect(separator).toHaveAttribute('aria-valuenow', String(previous + 10));
  await expect
    .poll(() =>
      page.evaluate(() => JSON.parse(localStorage.getItem('nelson.figure.workspace.v1') ?? '{}'))
    )
    .toEqual({ properties: true, navigation: true, details: true, width: previous + 10 });
  await page.reload();
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Workspace enhancements','WindowStyle','docked'); plot(1:3)"
  );
  await show();
  await expect(figure.getByRole('complementary', { name: 'Figure properties' })).toBeVisible();
  await expect(figure.getByLabel('Figure navigation', { exact: true })).toBeVisible();
  await expect(figure.locator('.figure-workspace-status')).toBeVisible();
  await expect(figure.getByRole('separator', { name: 'Properties width' })).toHaveAttribute(
    'aria-valuenow',
    String(previous + 10)
  );
}
//=============================================================================
