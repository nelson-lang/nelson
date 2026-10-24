//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { readFileSync } from 'node:fs';
import { expect, type Page } from '@playwright/test';
import { figureCommands } from './figureCommands';
//=============================================================================
export async function figureColorbarPerformanceScenario(page: Page) {
  const run = figureCommands(page, 'colorbar-performance');
  const source = readFileSync(
    new URL('../../../graphics/examples/colorbar/demo_colorbar.m', import.meta.url),
    'utf8'
  )
    .split('\n')
    .filter((line) => !line.trim().startsWith('%'))
    .join('\n');
  await page.goto('/');
  await page.getByRole('tab', { name: 'Command Window', exact: true }).click();
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    `close('all'); tic(); ${source}; f=gcf(); f.Name='Colorbar performance'; drawnow(); colorbarSeconds=toc(); asserts.istrue(colorbarSeconds<20); handle=__web_graphics_handle__(f,'number'); tic(); for iteration=1:20, state=__web_figure_state__(handle); end; snapshotSeconds=toc(); asserts.isequal(numel(state.axes),4); asserts.istrue(snapshotSeconds<5)`
  );
  await page.getByRole('tab', { name: /Colorbar performance/ }).click();
  const figure = page.locator('.figure-panel').first();
  await expect(figure.locator('canvas').first()).toBeVisible();
  const properties = figure.getByRole('button', { name: 'Properties', exact: true });
  if ((await properties.getAttribute('aria-expanded')) !== 'true') await properties.click();
  await expect(
    figure
      .getByRole('complementary', { name: 'Figure properties' })
      .getByLabel('Plot title', { exact: true })
  ).toBeVisible();
  await expect(figure).not.toHaveAttribute('aria-busy', 'true');
  await run("close('all')");
}
//=============================================================================
