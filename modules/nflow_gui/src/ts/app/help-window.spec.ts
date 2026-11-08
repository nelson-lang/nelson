//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
  armNflowHelpWindow,
  closeNflowHelpWindow,
  isNflowHelpWindowArmed,
  openNflowHelpWindow,
} from './help-window';
//=============================================================================
interface FakeHelpWindow {
  closed: boolean;
  focus: ReturnType<typeof vi.fn>;
  close: ReturnType<typeof vi.fn>;
  navigateToPage: ReturnType<typeof vi.fn>;
  searchHelp: ReturnType<typeof vi.fn>;
  location: { href: string };
  outerWidth: number;
  outerHeight: number;
  screenX: number;
  screenY: number;
}
//=============================================================================
function makeFakeWindow(): FakeHelpWindow {
  return {
    closed: false,
    focus: vi.fn(),
    close: vi.fn(),
    navigateToPage: vi.fn(),
    searchHelp: vi.fn(),
    location: { href: '' },
    outerWidth: 1100,
    outerHeight: 850,
    screenX: 30,
    screenY: 20,
  };
}
//=============================================================================
const flush = () => new Promise((resolve) => setTimeout(resolve, 0));
//=============================================================================
describe('nflow help-window', () => {
  let openSpy: ReturnType<typeof vi.spyOn>;
  let fakeWin: FakeHelpWindow;

  beforeEach(() => {
    localStorage.clear();
    fakeWin = makeFakeWindow();
    openSpy = vi.spyOn(window, 'open').mockReturnValue(fakeWin as unknown as Window);
    armNflowHelpWindow('en_US');
  });

  afterEach(() => {
    closeNflowHelpWindow();
    openSpy.mockRestore();
    localStorage.clear();
  });

  it('arms with a normalised language', () => {
    expect(isNflowHelpWindowArmed()).toBe(true);
  });

  it('opens a sized window named nelson-help', () => {
    openNflowHelpWindow({ open: './nflow_blocks/source/constant.html' });
    expect(openSpy).toHaveBeenCalledTimes(1);
    const [, name, features] = openSpy.mock.calls[0] as [string, string, string];
    expect(name).toBe('nelson-help');
    expect(features).toMatch(/width=\d+/);
    expect(features).toMatch(/height=\d+/);
  });

  it('navigates an open window in place instead of reopening', () => {
    openNflowHelpWindow({ open: './a.html' });
    openNflowHelpWindow({ open: './b.html' });
    expect(openSpy).toHaveBeenCalledTimes(1);
    expect(fakeWin.navigateToPage).toHaveBeenLastCalledWith('./b.html');
  });

  it('closes the window when the editor goes away', () => {
    openNflowHelpWindow({ open: './a.html' });
    closeNflowHelpWindow();
    expect(fakeWin.close).toHaveBeenCalled();
  });

  it('reloads the window in the new language when the engine switched', async () => {
    armNflowHelpWindow('en_US', () => Promise.resolve('fr_FR'));
    openNflowHelpWindow({ open: './nflow_blocks/source/sine.html' });
    await flush();
    expect(fakeWin.location.href).toContain('help/fr_FR/index.html');
  });
});
//=============================================================================
