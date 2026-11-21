//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';

for (const autoRoute of [true, false]) {
  test(`saved wire names appear on load without selection (autoRoute=${autoRoute})`, async ({
    mockPage: page,
  }) => {
    await new AppPage(page).open();
    for (const label of ['altitude [m]', 'speed [m/s]']) {
      await page.locator('#fileOpenInput').setInputFiles({
        name: 'labeled.nflow',
        mimeType: 'application/json',
        buffer: Buffer.from(
          JSON.stringify({
            format: 'nflow-2',
            name: 'Saved labels',
            autoRoute,
            blocks: [
              { id: 'source', type: 'constant', x: 100, y: 100, params: { Value: 4 } },
              { id: 'scope', type: 'scope', x: 400, y: 200, params: {} },
            ],
            connections: [{ from: 'source', to: 'scope', fromIndex: 0, toIndex: 0, label }],
          })
        ),
      });
      await expect(page.locator('#overlayLayer .wire-label')).toHaveText(label);
      await expect(page.locator('#overlayLayer .wire-label')).toBeVisible();
      await expect(page.locator('#wireLayer .wire.selected')).toHaveCount(0);
      await expect(page.locator('#blockLayer > .selected')).toHaveCount(0);
    }
    await page.screenshot({ path: `temp/wire-labels-load-${autoRoute}.png` });
  });
}
