//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// N3b - Inspector parameter robustness. Editing a block parameter must round-trip
// a valid value and, just as importantly, must not let a nonsensical entry break
// the editor: the app should stay alive and the block should recover once a good
// value is typed again. (nflow parameter fields are free-text -- validation is
// deferred to the engine at run time -- so this asserts graceful handling, not an
// inline validation message.)
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
import { InspectorPanel } from '../pages/InspectorPanel.js';
//=============================================================================
test.describe('N3b - inspector parameter robustness', () => {
  test('a valid parameter edit round-trips', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    const inspector = new InspectorPanel(page);

    const gain = await editor.insertBlock('gain', { x: 300, y: 240 });
    await inspector.open();
    await page.locator(`#blockLayer > [data-block-id="${gain}"]`).click();

    await inspector.setParam('Gain', '3.5');
    await expect.poll(() => inspector.getParam('Gain'), { timeout: 3000 }).toBe('3.5');
  });

  test('a nonsensical parameter entry does not break the editor and recovers', async ({
    mockPage: page,
  }) => {
    const pageErrors: string[] = [];
    page.on('pageerror', (error) => pageErrors.push(String(error)));

    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);
    const inspector = new InspectorPanel(page);

    const gain = await editor.insertBlock('gain', { x: 300, y: 240 });
    await inspector.open();
    await page.locator(`#blockLayer > [data-block-id="${gain}"]`).click();

    // Type garbage into a numeric parameter: the editor must not crash and the
    // block must survive.
    await inspector.setParam('Gain', 'not-a-number');
    await expect(page.locator(`#blockLayer > [data-block-id="${gain}"]`)).toHaveCount(1);
    expect(pageErrors, `no uncaught error from a bad parameter: ${pageErrors.join('; ')}`).toEqual(
      []
    );

    await inspector.setParam('Gain', '2');
    await expect.poll(() => inspector.getParam('Gain'), { timeout: 3000 }).toBe('2');
    expect(pageErrors).toEqual([]);
  });
});
//=============================================================================
