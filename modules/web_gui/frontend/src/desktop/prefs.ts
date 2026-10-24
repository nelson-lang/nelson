//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// User preferences (fonts, sizes, behaviour), persisted by the desktop store and
// broadcast so open panels apply them live. Kept deliberately small and
// framework-free: consumers subscribe and re-read, or read once on mount.
import { getPersistentItem, setPersistentItem } from './persistentStorage';
//=============================================================================
export type Prefs = {
  consoleFontSize: number;
  consoleFontFamily: string;
  consoleWrap: boolean;
  editorFontSize: number;
  editorTabSize: number;
};
//=============================================================================
export const DEFAULT_PREFS: Prefs = {
  consoleFontSize: 14,
  consoleFontFamily: 'Consolas, "Cascadia Mono", "DejaVu Sans Mono", monospace',
  consoleWrap: true,
  editorFontSize: 13,
  editorTabSize: 4,
};
//=============================================================================
// Reuse the keys the console already wrote so existing settings carry over.
const KEYS: Record<keyof Prefs, string> = {
  consoleFontSize: 'nelson-webview.console.fontSize',
  consoleFontFamily: 'nelson-webview.console.fontFamily',
  consoleWrap: 'nelson-webview.console.wrap',
  editorFontSize: 'nelson-webview.editor.fontSize',
  editorTabSize: 'nelson-webview.editor.tabSize',
};
//=============================================================================
const EVENT = 'nelson-webview.prefs-changed';
function read<K extends keyof Prefs>(key: K): Prefs[K] {
  const raw = getPersistentItem(KEYS[key]);
  const def = DEFAULT_PREFS[key];
  if (raw == null) return def;
  if (typeof def === 'number') {
    const n = Number(raw);
    return (Number.isFinite(n) ? n : def) as Prefs[K];
  }
  if (typeof def === 'boolean') {
    // The console historically stored wrap as 'on' | 'off'.
    return (raw === 'true' || raw === 'on') as Prefs[K];
  }
  return raw as Prefs[K];
}
//=============================================================================
export function getPrefs(): Prefs {
  return {
    consoleFontSize: read('consoleFontSize'),
    consoleFontFamily: read('consoleFontFamily'),
    consoleWrap: read('consoleWrap'),
    editorFontSize: read('editorFontSize'),
    editorTabSize: read('editorTabSize'),
  };
}
//=============================================================================
export function setPrefs(patch: Partial<Prefs>): void {
  for (const [k, v] of Object.entries(patch)) {
    const key = KEYS[k as keyof Prefs];
    setPersistentItem(key, String(v));
  }
  window.dispatchEvent(new CustomEvent(EVENT, { detail: getPrefs() }));
}
//=============================================================================
export function subscribePrefs(cb: (p: Prefs) => void): () => void {
  const handler = () => cb(getPrefs());
  window.addEventListener(EVENT, handler);
  return () => window.removeEventListener(EVENT, handler);
}
//=============================================================================
