//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test } from '@playwright/test';
import { checkModernUi } from '../render_compare/modernUi';
//=============================================================================
test('modern UI retains Qt pixel layout, dials and callbacks after resize', async ({
  page,
}, info) => {
  await page.goto('/');
  await expect(page.locator('.console-host').first()).toContainText('connected: engine', {
    timeout: 60_000,
  });
  await checkModernUi(page, info);
});
//=============================================================================
