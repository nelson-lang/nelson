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
  closeHelpWindow,
  isHelpWindowOpen,
  navigateHelpWindowIfOpen,
  rememberHelpWindowGeometry,
  showHelpInWindow,
} from '../../src/desktop/helpWindow';
//=============================================================================
// The documentation language is read synchronously from a cache; keep rpc inert
// so importing helpIndex never reaches the backend during these tests.
vi.mock('../../src/rpc/client', () => ({ rpc: vi.fn().mockResolvedValue({ lang: 'en_US' }) }));
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
    outerWidth: 1200,
    outerHeight: 900,
    screenX: 60,
    screenY: 40,
  };
}
//=============================================================================
describe('helpWindow', () => {
  let openSpy: ReturnType<typeof vi.spyOn>;
  let fakeWin: FakeHelpWindow;

  beforeEach(() => {
    localStorage.clear();
    fakeWin = makeFakeWindow();
    openSpy = vi.spyOn(window, 'open').mockReturnValue(fakeWin as unknown as Window);
  });

  afterEach(() => {
    closeHelpWindow();
    openSpy.mockRestore();
    localStorage.clear();
  });

  it('opens a sized window the first time and reports it open', () => {
    const win = showHelpInWindow({ open: './a.html' }, 'en_US');
    expect(win).toBe(fakeWin as unknown as Window);
    expect(openSpy).toHaveBeenCalledTimes(1);
    const [, name, features] = openSpy.mock.calls[0] as [string, string, string];
    expect(name).toBe('nelson-help');
    expect(features).toMatch(/width=\d+/);
    expect(features).toMatch(/height=\d+/);
    expect(isHelpWindowOpen()).toBe(true);
  });

  it('navigates an already-open window in place instead of reopening', () => {
    showHelpInWindow({ open: './a.html' }, 'en_US');
    showHelpInWindow({ open: './b.html' }, 'en_US');
    expect(openSpy).toHaveBeenCalledTimes(1);
    expect(fakeWin.focus).toHaveBeenCalled();
    expect(fakeWin.navigateToPage).toHaveBeenLastCalledWith('./b.html');
  });

  it('forces a full reload when asked (language change path)', () => {
    showHelpInWindow({ open: './a.html' }, 'en_US');
    const navigated = navigateHelpWindowIfOpen({ open: './trig/sin.html' }, 'fr_FR', true);
    expect(navigated).toBe(true);
    // A reload bypasses the in-place API and drives the location to the fr shell.
    expect(fakeWin.navigateToPage).not.toHaveBeenCalled();
    expect(fakeWin.location.href).toContain('help/fr_FR/index.html');
  });

  it('does not navigate when no window is open', () => {
    expect(navigateHelpWindowIfOpen({ open: './a.html' })).toBe(false);
  });

  it('closes the window and forgets it', () => {
    showHelpInWindow({ open: './a.html' }, 'en_US');
    closeHelpWindow();
    expect(fakeWin.close).toHaveBeenCalled();
    expect(isHelpWindowOpen()).toBe(false);
  });

  it('remembers geometry and reuses it on the next opening', () => {
    showHelpInWindow({ open: './a.html' }, 'en_US');
    rememberHelpWindowGeometry();
    closeHelpWindow();
    // A fresh opening carries the saved position/size through the features string.
    showHelpInWindow({ open: './a.html' }, 'en_US');
    const lastCall = openSpy.mock.calls.at(-1) as [string, string, string];
    expect(lastCall[2]).toMatch(/left=\d+/);
    expect(lastCall[2]).toMatch(/top=\d+/);
  });
});
//=============================================================================
