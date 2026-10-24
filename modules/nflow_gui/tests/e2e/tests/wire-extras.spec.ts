//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Power-user wire touches and type-agnostic wiring. A selected bent wire exposes
// draggable corner handles (renderer-wire-corner); double-clicking a wire names
// it (wire-labels); and the click-to-connect flow works between any output and
// any input regardless of the blocks' types.
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
import { CanvasEditor } from '../pages/CanvasEditor.js';
//=============================================================================
// NOTE: manual wire-corner dragging (.wire-corner-handle / moveWireCorner) is
// intentionally NOT covered here. A corner handle only renders when the
// connection's stored `points` has >= 3 entries (a turn), which the router
// writes for wires that route around geometry. Synthetically drawn wires in the
// headless harness keep a 2-point route (the visible bend lives in the rendered
// `d`, not in stored points), so the handle cannot be triggered deterministically
// without coupling the test to router internals. The auto re-routing that keeps a
// wire healthy while a block is dragged is covered by tests/b2b/tests/wire-drag.
test.describe('wire extras', () => {
  test('double-clicking a wire lets you name it', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    await app.open();
    const editor = new CanvasEditor(page);

    const source = await editor.insertBlock('sine', { x: 200, y: 220 });
    const sink = await editor.insertBlock('scope', { x: 480, y: 220 });
    await editor.connect(source, 0, sink, 0);
    await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(1);

    await page.locator('#wireLayer path.wire-hit').first().dblclick({ force: true });
    const input = page.locator('#wireLabelInput');
    await expect(input).toBeVisible({ timeout: 3000 });
    await input.fill('speed_cmd');
    await input.press('Enter');

    await expect(
      page.locator('#overlayLayer text.wire-label', { hasText: 'speed_cmd' })
    ).toHaveCount(1, { timeout: 3000 });
  });

  // Wiring is type-agnostic: any output can drive any input.
  const pairs: Array<[string, string]> = [
    ['gain', 'gain'],
    ['sum', 'scope'],
    ['constant', 'gain'],
    ['integrator', 'scope'],
  ];
  for (const [fromType, toType] of pairs) {
    test(`connects ${fromType} -> ${toType}`, async ({ mockPage: page }) => {
      const app = new AppPage(page);
      await app.open();
      const editor = new CanvasEditor(page);

      const from = await editor.insertBlock(fromType, { x: 200, y: 220 });
      const to = await editor.insertBlock(toType, { x: 480, y: 220 });
      await editor.connect(from, 0, to, 0);

      await expect.poll(() => editor.wireCount(), { timeout: 3000 }).toBe(1);
    });
  }
});
//=============================================================================
