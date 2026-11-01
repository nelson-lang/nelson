//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// desktop strings translated by the engine catalog through i18n.translate; unknown keys stay
//=============================================================================
import { rpc } from './rpc/client';
//=============================================================================
const translations = new Map<string, string>();
const listeners = new Set<() => void>();
let loading: Promise<void> | null = null;
//=============================================================================
export function t(key: string): string {
  return translations.get(key) ?? key;
}
//=============================================================================
// Re-renders subscribers once the catalog arrived.
export function subscribeTranslations(cb: () => void): () => void {
  listeners.add(cb);
  return () => listeners.delete(cb);
}
//=============================================================================
export function loadTranslations(keys: string[]): Promise<void> {
  const missing = keys.filter((k) => !translations.has(k));
  if (!missing.length) return Promise.resolve();
  const request = rpc<{ translations?: Record<string, string> }>('i18n.translate', {
    keys: missing,
  })
    .then((r) => {
      let changed = false;
      for (const [key, text] of Object.entries(r.translations ?? {})) {
        if (typeof text === 'string' && text.length > 0 && translations.get(key) !== text) {
          translations.set(key, text);
          changed = true;
        }
      }
      if (changed) listeners.forEach((cb) => cb());
    })
    .catch(() => undefined);
  loading = request;
  return request;
}
//=============================================================================
export function translationsLoading(): Promise<void> | null {
  return loading;
}
//=============================================================================
// strings of the editor debugger, preloaded at startup
export const DEBUGGER_STRINGS: string[] = [
  'Continue',
  'Step',
  'Step In',
  'Step Out',
  'Run to Here',
  'Pause',
  'Stop',
  'Set/Clear Breakpoint',
  'Set Breakpoint',
  'Clear Breakpoint',
  'Set/Modify Condition...',
  'Set Conditional Breakpoint...',
  'Enable Breakpoint',
  'Disable Breakpoint',
  'Enable All Breakpoints in File',
  'Disable All Breakpoints in File',
  'Clear All Breakpoints in File',
  'Clear All Breakpoints',
  'Pause on Errors',
  'Pause on Caught Errors',
  'Breakpoints',
  'Call Stack',
  'Paused',
  'Paused on error',
  'Running',
  'Condition',
  'Enabled',
  'File',
  'Function',
  'Line',
  'Clear',
  'Clear All',
  'Enable All',
  'Disable All',
  'Debugging ended: the file was saved while paused.',
  'Invalid until the file is saved.',
  'Pause on line %1 only when this expression is true:',
  'Apply',
  'Cancel',
  'DEBUG',
  'No breakpoints',
  'Not paused',
];
//=============================================================================
