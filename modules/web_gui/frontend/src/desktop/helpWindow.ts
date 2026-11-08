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
// The help view is a dedicated OS window (not a tab), so its size and position
// can be remembered between openings. Geometry is stored per browser profile.
const GEOMETRY_KEY = 'nelson-webview.help-window.geometry';
const DEFAULT_WIDTH = 1000;
const DEFAULT_HEIGHT = 800;
//=============================================================================
interface WindowGeometry {
  w: number;
  h: number;
  x: number;
  y: number;
}
//=============================================================================
function readGeometry(): WindowGeometry | null {
  try {
    const raw = localStorage.getItem(GEOMETRY_KEY);
    if (!raw) return null;
    const value = JSON.parse(raw) as Partial<WindowGeometry>;
    if (
      typeof value.w !== 'number' ||
      typeof value.h !== 'number' ||
      typeof value.x !== 'number' ||
      typeof value.y !== 'number'
    ) {
      return null;
    }
    return value as WindowGeometry;
  } catch {
    return null;
  }
}
//=============================================================================
// Capture the live window's geometry so the next opening reuses it. Best-effort:
// a window the user already closed, or one minimised to 0, is ignored.
export function rememberHelpWindowGeometry(): void {
  try {
    if (!helpWin || helpWin.closed) return;
    const w = helpWin.outerWidth;
    const h = helpWin.outerHeight;
    if (w < 120 || h < 120) return;
    const geometry: WindowGeometry = { w, h, x: helpWin.screenX, y: helpWin.screenY };
    localStorage.setItem(GEOMETRY_KEY, JSON.stringify(geometry));
  } catch {
    // ignore: storage may be unavailable, or the window cross-checks may throw
  }
}
//=============================================================================
// Window features for a fresh opening, clamped to the visible screen so a saved
// position on a monitor that is gone does not open the window off-screen.
function windowFeatures(): string {
  const saved = readGeometry();
  let width = DEFAULT_WIDTH;
  let height = DEFAULT_HEIGHT;
  let left: number | null = null;
  let top: number | null = null;
  try {
    const availW = window.screen?.availWidth || DEFAULT_WIDTH;
    const availH = window.screen?.availHeight || DEFAULT_HEIGHT;
    width = Math.min(saved?.w ?? DEFAULT_WIDTH, availW);
    height = Math.min(saved?.h ?? DEFAULT_HEIGHT, availH);
    if (saved) {
      left = Math.max(0, Math.min(saved.x, availW - width));
      top = Math.max(0, Math.min(saved.y, availH - height));
    }
  } catch {
    width = saved?.w ?? DEFAULT_WIDTH;
    height = saved?.h ?? DEFAULT_HEIGHT;
  }
  const parts = [`width=${Math.round(width)}`, `height=${Math.round(height)}`];
  if (left !== null && top !== null)
    parts.push(`left=${Math.round(left)}`, `top=${Math.round(top)}`);
  return parts.join(',');
}
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
// `reload` forces a full navigation (shell reload), needed when the language
// changed: the in-place API only swaps the content frame inside the old shell.
function navigateExisting(win: Window, lang: string, target: HelpTarget, reload = false): void {
  const viewer = win as HelpViewerWindow;
  if (!reload) {
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
// Close the help window and forget it. Called when the window that owns it (the
// desktop) goes away, so the dedicated help window does not outlive its opener.
export function closeHelpWindow(): void {
  rememberHelpWindowGeometry();
  try {
    if (helpWin && !helpWin.closed) helpWin.close();
  } catch {
    // ignore: closing a window the browser already reclaimed is harmless
  }
  helpWin = null;
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
    rememberHelpWindowGeometry();
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
    helpWin = window.open(viewerUrl(lang, target), HELP_WINDOW_NAME, windowFeatures());
  } catch {
    helpWin = null;
  }
  return helpWin;
}
//=============================================================================
// Navigate the help window only when it is already open; never opens a popup.
// `reload` forces a shell reload (used when the documentation language changed).
export function navigateHelpWindowIfOpen(
  target: HelpTarget,
  lang: string = getHelpLangSync(),
  reload = false
): boolean {
  if (helpWin && !helpWin.closed) {
    navigateExisting(helpWin, lang, target, reload);
    return true;
  }
  return false;
}
//=============================================================================
