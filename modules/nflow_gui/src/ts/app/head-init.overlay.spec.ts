//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('head-init overlay without legacy wrappers', () => {
  beforeEach(() => {
    document.documentElement.innerHTML = '';
    delete (window as any).openFileDialog;
    delete (window as any).saveFileDialog;
  });

  test('legacy file dialog wrappers are not installed', async () => {
    // import the runtime JS head-init (side-effects)
    await import('../../../dist/app/head-init.js').catch(() => null);
    expect((window as any).openFileDialog).toBeUndefined();
    expect((window as any).saveFileDialog).toBeUndefined();
  });
});
//=============================================================================
