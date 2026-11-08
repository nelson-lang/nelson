//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// A single reusable browser window ("nelson-help") that hosts the service-worker
// documentation viewer. The window name is origin-global, so the desktop and the
// NFlow editor windows share one help window. Topic switches drive it in place
// through the viewer's own same-origin API (navigateToPage / searchHelp), so the
// shell (search index, TOC, MathJax) is loaded once and not reloaded per topic.
import { getHelpLangSync, type HelpTarget } from './helpIndex';
//=============================================================================
const HELP_WINDOW_NAME = 'nelson-help';
let helpWin: Window | null = null;
//=============================================================================
type HelpViewerWindow = Window & {
  navigateToPage?: (page: string) => void;
  searchHelp?: (term: string) => void;
};
//=============================================================================
function viewerUrl(lang: string, target: HelpTarget): string {
  const url = new URL(`help/${lang}/index.html`, window.location.href);
  url.searchParams.set('lang', lang);
  if (target.open) url.searchParams.set('open', target.open);
  if (target.search) url.searchParams.set('search', target.search);
  return url.toString();
}
//=============================================================================
// Drive an already-open window. Prefer the viewer's in-place API; fall back to a
// full navigation when it is not ready yet (still loading) or for the homepage.
function navigateExisting(win: Window, lang: string, target: HelpTarget): void {
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
    win.location.href = viewerUrl(lang, target);
  } catch {
    // ignore: the window may have been closed between the check and here
  }
}
//=============================================================================
export function isHelpWindowOpen(): boolean {
  return !!helpWin && !helpWin.closed;
}
//=============================================================================
// Open or reuse the shared help window and show `target`. Returns the window, or
// null when the browser blocked the popup (no user gesture and no window yet),
// so the caller can fall back to the docked panel. Navigating a window we already
// hold needs no gesture; only creating one does.
export function showHelpInWindow(
  target: HelpTarget,
  lang: string = getHelpLangSync()
): Window | null {
  // Fast path: we already hold the window — navigate it in place (no reload).
  if (helpWin && !helpWin.closed) {
    try {
      helpWin.focus();
    } catch {
      // ignore: focusing a background window can be blocked, which is harmless
    }
    navigateExisting(helpWin, lang, target);
    return helpWin;
  }
  // Otherwise open a fresh window loaded directly at the target. A null result
  // means the popup was blocked (no user gesture), so the caller falls back to
  // the docked panel.
  try {
    helpWin = window.open(viewerUrl(lang, target), HELP_WINDOW_NAME);
  } catch {
    helpWin = null;
  }
  return helpWin;
}
//=============================================================================
// Navigate the help window only when it is already open; never opens a popup.
export function navigateHelpWindowIfOpen(
  target: HelpTarget,
  lang: string = getHelpLangSync()
): boolean {
  if (helpWin && !helpWin.closed) {
    navigateExisting(helpWin, lang, target);
    return true;
  }
  return false;
}
//=============================================================================
