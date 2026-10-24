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
export async function figureLivePanScenario(page: Page) {
  const run = figureCommands(page, 'live-pan');
  const ready = () => expect(page.locator('.figure-workspace-status')).not.toContainText('Pending');
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toContainText('connected:', {
    timeout: 120000,
  });
  await run(
    "close('all'); f=figure('Name','Live pan','WindowStyle','docked'); a1=subplot(2,2,1); plot(1:5); title('Signal'); a2=subplot(2,2,2); t=linspace(0,4*pi,200); plot3(cos(t),sin(t),t); title('Helix'); a3=subplot(2,2,3); surf(peaks(25)); title('Surface'); a4=subplot(2,2,4); plot(5:-1:1); title('Other'); drawnow(); slots=[a1.Position;a2.Position;a3.Position;a4.Position]"
  );
  await page.getByRole('tab', { name: /Live pan/ }).click();
  await showFigureBars(page);
  await page.getByRole('button', { name: 'Properties', exact: true }).click();
  await waitForFigureLayout(page);
  for (const [label, fx, fy] of [
    ['Signal', 0.3, 0.3],
    ['Helix', 0.75, 0.3],
    ['Surface', 0.3, 0.7],
  ] as const) {
    await page.getByRole('combobox', { name: 'Axes', exact: true }).selectOption({ label });
    for (const projection of label === 'Signal'
      ? ['orthographic']
      : ['orthographic', 'perspective']) {
      await page
        .getByRole('combobox', { name: 'Projection', exact: true })
        .selectOption(projection);
      await page.getByRole('button', { name: 'Fit', exact: true }).click();
      await ready();
      const initial = await page
        .getByRole('textbox', { name: 'X limits', exact: true })
        .inputValue();
      const drawing = page.locator('.figure-panel__canvas');
      const reference = await drawing.screenshot();
      const box = await drawing.boundingBox();
      if (!box) throw new Error('Figure drawing area is missing');
      const x = box.x + box.width * fx,
        y = box.y + box.height * fy;
      await page.getByRole('button', { name: 'Pan', exact: true }).click();
      await page.mouse.move(x, y);
      await page.mouse.down();
      try {
        await page.mouse.move(x + 20, y - 10, { steps: 12 });
        // The inspector receives actual engine properties, with the mouse still down.
        const limits = page.getByRole('textbox', { name: 'X limits', exact: true });
        await expect(limits).not.toHaveValue(initial);
        await ready();
        const partial = await limits.inputValue();
        const partialImage = await drawing.screenshot();
        expect(partialImage.equals(reference)).toBe(false);
        await page.mouse.move(x + 55, y - 25, { steps: 40 });
        await expect(limits).not.toHaveValue(partial);
        await ready();
        await drawing.screenshot({
          path: test.info().outputPath(`${label}-${projection}-held.png`),
        });
      } finally {
        await page.mouse.up();
      }
      // Fit immediately after release must run after the final pan displacement.
      await page.getByRole('button', { name: 'Fit', exact: true }).click();
      await ready();
      await expect(page.getByRole('textbox', { name: 'X limits', exact: true })).toHaveValue(
        initial
      );
      await expect.poll(async () => (await drawing.screenshot()).equals(reference)).toBe(true);
    }
  }
  await run('asserts.isequal([a1.Position;a2.Position;a3.Position;a4.Position],slots); close(f)');
}
//=============================================================================
