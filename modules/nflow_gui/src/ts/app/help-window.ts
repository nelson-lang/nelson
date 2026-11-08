//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Drives the dedicated "nelson-help" documentation window from the NFlow editor
// (browser build only). The window name is origin-global; the desktop and the
// editor run in separate browsing contexts, so each holds its own help window.
// Opening must happen synchronously from a click handler to keep the user
// gesture — otherwise the browser blocks the popup. Only armed in the WASM
// build; the native host opens the system browser.
//=============================================================================
type HelpTarget = { open?: string; search?: string };
type HelpViewerWindow = Window & {
  navigateToPage?: (page: string) => void;
  searchHelp?: (term: string) => void;
};
type LanguageProbe = () => Promise<string>;
//=============================================================================
const HELP_WINDOW_NAME = 'nelson-help';
const GEOMETRY_KEY = 'nflow.help-window.geometry';
const DEFAULT_WIDTH = 1000;
const DEFAULT_HEIGHT = 800;
let armed = false;
let lang = 'en_US';
let probeLanguage: LanguageProbe | null = null;
let helpWin: Window | null = null;
//=============================================================================
export function armNflowHelpWindow(language: string, probe?: LanguageProbe): void {
  armed = true;
  lang = language === 'fr_FR' ? 'fr_FR' : 'en_US';
  if (probe) probeLanguage = probe;
}
//=============================================================================
export function isNflowHelpWindowArmed(): boolean {
  return armed;
}
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
// Capture the live window's geometry so the next opening reuses it.
function rememberGeometry(): void {
  try {
    if (!helpWin || helpWin.closed) return;
    const w = helpWin.outerWidth;
    const h = helpWin.outerHeight;
    if (w < 120 || h < 120) return;
    const geometry: WindowGeometry = { w, h, x: helpWin.screenX, y: helpWin.screenY };
    localStorage.setItem(GEOMETRY_KEY, JSON.stringify(geometry));
  } catch {
    // ignore: storage may be unavailable or cross-window reads may throw
  }
}
//=============================================================================
// Window features for a fresh opening, clamped to the visible screen so a saved
// position on a monitor that is gone does not open the window off-screen.
function windowFeatures(): string {
  const saved = readGeometry();
  let width = saved?.w ?? DEFAULT_WIDTH;
  let height = saved?.h ?? DEFAULT_HEIGHT;
  let left: number | null = null;
  let top: number | null = null;
  try {
    const availW = window.screen?.availWidth || DEFAULT_WIDTH;
    const availH = window.screen?.availHeight || DEFAULT_HEIGHT;
    width = Math.min(width, availW);
    height = Math.min(height, availH);
    if (saved) {
      left = Math.max(0, Math.min(saved.x, availW - width));
      top = Math.max(0, Math.min(saved.y, availH - height));
    }
  } catch {
    // keep the saved or default size without clamping
  }
  const parts = [`width=${Math.round(width)}`, `height=${Math.round(height)}`];
  if (left !== null && top !== null) {
    parts.push(`left=${Math.round(left)}`, `top=${Math.round(top)}`);
  }
  return parts.join(',');
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
// `reload` forces a full navigation (shell reload), needed when the language
// changed: the in-place API only swaps the content frame inside the old shell.
function navigateExisting(win: Window, target: HelpTarget, reload = false): void {
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
    win.location.href = viewerUrl(target);
  } catch {
    // ignore: the window may have been closed between the check and here
  }
}
//=============================================================================
// Follow a runtime language switch (there is no language-change push): re-probe
// and, if it changed, re-arm and reload the open window to the same page in the
// new language. Best-effort and asynchronous, so the click stays synchronous.
function followLanguageChange(target: HelpTarget): void {
  const probe = probeLanguage;
  if (!probe) return;
  void probe().then((language) => {
    const next = language === 'fr_FR' ? 'fr_FR' : 'en_US';
    if (next === lang) return;
    lang = next;
    if (helpWin && !helpWin.closed) navigateExisting(helpWin, target, true);
  });
}
//=============================================================================
// Close the help window and forget it. Called when the editor window that owns
// it goes away, so the dedicated help window does not outlive its opener.
export function closeNflowHelpWindow(): void {
  rememberGeometry();
  try {
    if (helpWin && !helpWin.closed) helpWin.close();
  } catch {
    // ignore: closing a window the browser already reclaimed is harmless
  }
  helpWin = null;
}
//=============================================================================
// Open or reuse the dedicated help window and show `target`. Call this
// synchronously from the click handler so the first open keeps the gesture; a
// window we already hold is navigated in place (fast) and needs no gesture.
export function openNflowHelpWindow(target: HelpTarget = {}): void {
  // Fast path: we already hold the window — navigate it in place (no reload).
  if (helpWin && !helpWin.closed) {
    rememberGeometry();
    try {
      helpWin.focus();
    } catch {
      // ignore: focusing a background window can be blocked, which is harmless
    }
    navigateExisting(helpWin, target);
    followLanguageChange(target);
    return;
  }
  // Otherwise open a fresh window loaded directly at the target.
  try {
    helpWin = window.open(viewerUrl(target), HELP_WINDOW_NAME, windowFeatures());
  } catch {
    helpWin = null;
  }
  followLanguageChange(target);
}
//=============================================================================
