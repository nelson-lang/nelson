//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Theme handling: System / Light / Dark, persisted by the desktop store.
// Exposes the Dockview theme class and the app color scheme.
import { getPersistentItem, setPersistentItem } from './persistentStorage';
//=============================================================================
export type ThemePref = 'system' | 'light' | 'dark';
export const THEME_CHANGE_EVENT = 'nelson-webview.theme-change';
const KEY = 'nelson-webview.theme';
export function getThemePref(): ThemePref {
  const v = getPersistentItem(KEY);
  return v === 'light' || v === 'dark' ? v : 'system';
}
//=============================================================================
export function setThemePref(pref: ThemePref): void {
  setPersistentItem(KEY, pref);
}
//=============================================================================
export function requestTheme(pref: ThemePref): void {
  if (typeof window !== 'undefined') {
    window.dispatchEvent(new CustomEvent(THEME_CHANGE_EVENT, { detail: pref }));
  }
}
//=============================================================================
export function resolveScheme(pref: ThemePref): 'light' | 'dark' {
  if (pref === 'system') {
    return typeof window !== 'undefined' &&
      window.matchMedia?.('(prefers-color-scheme: dark)').matches
      ? 'dark'
      : 'light';
  }
  return pref;
}
//=============================================================================
export function dockviewThemeClass(scheme: 'light' | 'dark'): string {
  return scheme === 'dark' ? 'dockview-theme-abyss' : 'dockview-theme-light';
}
//=============================================================================
// Imperative panels (the xterm console) cannot take the scheme as a prop through
// Dockview's static params, so the App keeps this store in sync and panels
// subscribe to it.
export type Scheme = 'light' | 'dark';
let activeScheme: Scheme = resolveScheme(getThemePref());
const schemeListeners = new Set<(s: Scheme) => void>();
export function getActiveScheme(): Scheme {
  return activeScheme;
}
//=============================================================================
export function setActiveScheme(s: Scheme): void {
  if (s === activeScheme) return;
  activeScheme = s;
  schemeListeners.forEach((l) => l(s));
}
//=============================================================================
export function subscribeScheme(cb: (s: Scheme) => void): () => void {
  schemeListeners.add(cb);
  return () => schemeListeners.delete(cb);
}
//=============================================================================
// xterm.js renders to a canvas and ignores CSS variables, so it needs an
// explicit theme object per scheme.
export function xtermTheme(scheme: Scheme): {
  background: string;
  foreground: string;
  cursor: string;
  selectionBackground: string;
} {
  return scheme === 'dark'
    ? {
        background: '#1e1e1e',
        foreground: '#d4d4d4',
        cursor: '#d4d4d4',
        selectionBackground: '#264f78',
      }
    : {
        background: '#ffffff',
        foreground: '#1e1e1e',
        cursor: '#1e1e1e',
        selectionBackground: '#add6ff',
      };
}
//=============================================================================
