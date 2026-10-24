//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('app module', () => {
  test('exports init function', async () => {
    const mod = await import('./app.js');

    expect(typeof mod.init).toBe('function');
  });

  test('init() runs without throwing and updates status bar', async () => {
    document.body.innerHTML = `
      <div id="statusBarInfo"></div>
      <div id="status"></div>
    `;
    global.window.__nf_init_libraries_promise = Promise.resolve();

    // jsdom doesn't implement matchMedia - provide a minimal mock
    if (typeof window.matchMedia !== 'function') {
      global.window.matchMedia = (query) => ({
        matches: false,
        media: query,
        addListener: () => {},
        removeListener: () => {},
        addEventListener: () => {},
        removeEventListener: () => {},
        onchange: null,
        dispatchEvent: () => false,
      });
    }

    const mod = await import('./app.js');
    await expect(mod.init()).resolves.not.toThrow();
    const statusBarInfo = document.getElementById('statusBarInfo')!;
    expect(statusBarInfo.textContent).toBe('Ready');
  });
});
//=============================================================================
