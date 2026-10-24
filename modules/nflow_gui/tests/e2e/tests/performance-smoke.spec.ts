//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Performance smoke tests
//=============================================================================
import { test, expect } from '../fixtures/webui-mock.js';
import { AppPage } from '../pages/AppPage.js';
//=============================================================================
test.describe('Performance smoke', () => {
  test('app opens within a reasonable time', async ({ mockPage: page }) => {
    const app = new AppPage(page);
    const start = Date.now();
    await app.open();
    const elapsed = Date.now() - start;

    // Loose threshold for slow CI machines.
    expect(elapsed).toBeLessThan(30_000);
  });
});
//=============================================================================
