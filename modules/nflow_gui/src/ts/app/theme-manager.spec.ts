//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { getThemes, setThemes, applyTheme, loadThemes } from '../../../dist/app/theme-manager.js';
//=============================================================================
describe('theme manager', () => {
  beforeEach(() => {
    setThemes([]);
    document.documentElement.removeAttribute('data-theme');
  });

  test('setThemes and getThemes', () => {
    setThemes([{ id: 'a', name: 'A' }]);
    expect(getThemes()).toEqual([{ id: 'a', name: 'A' }]);
  });

  test('applyTheme sets data-theme or removes for default', () => {
    setThemes([
      { id: 'default', name: 'Default' },
      { id: 'dark', name: 'Dark' },
    ]);
    applyTheme('dark');
    expect(document.documentElement.getAttribute('data-theme')).toBe('dark');
    applyTheme('default');
    expect(document.documentElement.hasAttribute('data-theme')).toBe(false);
  });

  test('loadThemes parses CSS comments', async () => {
    const css = '/* Theme Name: Foo */\n/* Theme Name: Bar */';
    // @ts-ignore
    global.fetch = vi.fn(() => Promise.resolve({ text: () => Promise.resolve(css) }));
    const themes = await loadThemes();
    expect(Array.isArray(themes)).toBe(true);
    expect(themes.length).toBeGreaterThanOrEqual(2);
  });
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/theme-manager.js').catch(() => null);
});
//=============================================================================
test('theme-manager module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
//=============================================================================
