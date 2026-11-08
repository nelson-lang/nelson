//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Drives the shared "nelson-help" documentation window from the NFlow editor
// (browser build only). The window name is origin-global, so the editor and the
// desktop share a single help window. Opening must happen synchronously from a
// click handler to keep the user gesture — otherwise the browser blocks the
// popup. Only armed in the WASM build; the native host opens the system browser.
//=============================================================================
type HelpTarget = { open?: string; search?: string };
type HelpViewerWindow = Window & {
  navigateToPage?: (page: string) => void;
  searchHelp?: (term: string) => void;
};
//=============================================================================
const HELP_WINDOW_NAME = 'nelson-help';
let armed = false;
let lang = 'en_US';
let helpWin: Window | null = null;
//=============================================================================
export function armNflowHelpWindow(language: string): void {
  armed = true;
  lang = language === 'fr_FR' ? 'fr_FR' : 'en_US';
}
//=============================================================================
export function isNflowHelpWindowArmed(): boolean {
  return armed;
}
//=============================================================================
function viewerUrl(target: HelpTarget): string {
  // The editor runs at <base>/nflow/index.html; the viewer is at <base>/help/...
  const url = new URL(`../help/${lang}/index.html`, window.location.href);
  url.searchParams.set('lang', lang);
  if (target.open) url.searchParams.set('open', target.open);
  if (target.search) url.searchParams.set('search', target.search);
  return url.toString();
}
//=============================================================================
function navigateExisting(win: Window, target: HelpTarget): void {
  const viewer = win as HelpViewerWindow;
  try {
    if (target.open && typeof viewer.navigateToPage === 'function') {
      viewer.navigateToPage(target.open);
      return;
    }
    if (target.search && typeof viewer.searchHelp === 'function') {
      viewer.searchHelp(target.search);
      return;
    }
  } catch {
    // fall through to a full navigation
  }
  try {
    win.location.href = viewerUrl(target);
  } catch {
    // ignore: the window may have been closed between the check and here
  }
}
//=============================================================================
// Open or reuse the shared help window and show `target`. Call this synchronously
// from the click handler so the first open keeps the gesture; a window we already
// hold is navigated in place (fast) and needs no gesture.
export function openNflowHelpWindow(target: HelpTarget = {}): void {
  // Fast path: we already hold the window — navigate it in place (no reload).
  if (helpWin && !helpWin.closed) {
    try {
      helpWin.focus();
    } catch {
      // ignore: focusing a background window can be blocked, which is harmless
    }
    navigateExisting(helpWin, target);
    return;
  }
  // Otherwise open a fresh window loaded directly at the target.
  try {
    helpWin = window.open(viewerUrl(target), HELP_WINDOW_NAME);
  } catch {
    helpWin = null;
  }
}
//=============================================================================
