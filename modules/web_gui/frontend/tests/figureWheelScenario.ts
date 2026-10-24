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
//=============================================================================
export async function checkFigureWheel(
  page: Page,
  run: (command: string) => Promise<void>,
  show: () => Promise<void>
) {
  for (const [label, fy, projection] of [
    ['Signal', 0.3, 'orthographic'],
    ['Surface', 0.7, 'orthographic'],
    ['Surface', 0.7, 'perspective'],
  ] as const) {
    await run(
      `a3.Projection='${projection}'; drawnow(); wheelLimits=a1.XLim; wheelAngle=a3.CameraViewAngle; untouched=[a2.CameraPosition;a4.CameraPosition]`
    );
    await show();
    const drawing = page.locator('.figure-panel__canvas');
    const box = await drawing.boundingBox();
    if (!box) throw new Error('Figure drawing area is missing');
    const x = box.x + box.width * 0.3,
      y = box.y + box.height * fy;
    await page.mouse.move(x, y);
    await expect(page.locator('.figure-axes-indicator')).toHaveText(label);
    const before = await drawing.screenshot();
    await page.mouse.wheel(0, -100);
    await expect.poll(async () => (await drawing.screenshot()).equals(before)).toBe(false);
    await run(
      label === 'Signal'
        ? 'asserts.istrue(diff(a1.XLim)<diff(wheelLimits)); asserts.isequal(a3.CameraViewAngle,wheelAngle)'
        : 'asserts.istrue(a3.CameraViewAngle<wheelAngle); asserts.isequal(a1.XLim,wheelLimits)'
    );
    await run('asserts.isequal([a2.CameraPosition;a4.CameraPosition],untouched)');
    await show();
    await page.mouse.move(x, y);
    await page.mouse.wheel(0, -40);
    await page.mouse.dblclick(x, y);
    await run(
      label === 'Signal'
        ? "asserts.isequal(a1.XLimMode,'auto')"
        : "asserts.isequal(a3.CameraViewAngleMode,'auto')"
    );
  }
  await run("a1.XLim=[10 20]; a3.Projection='orthographic'");
  await show();
}
//=============================================================================
