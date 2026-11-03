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
// Keys shared with the Qt desktop keep their & shortcut marks and '...': the web shows them without.
export function t(key: string): string {
  const text = translations.get(key) ?? key;
  if (!text.includes('&') && !text.endsWith('...')) return text;
  return text.replace(/&(.)/g, '$1').replace(/\.\.\.$/, '…');
}
//=============================================================================
// Replaces %1, %2... with the given values.
export function tf(key: string, ...values: (string | number | undefined)[]): string {
  return t(key).replace(/%(\d)/g, (m, n: string) => {
    const index = Number(n) - 1;
    return index < values.length ? String(values[index] ?? '') : m;
  });
}
//=============================================================================
// Re-renders subscribers once the catalog arrived.
export function subscribeTranslations(cb: () => void): () => void {
  listeners.add(cb);
  return () => listeners.delete(cb);
}
//=============================================================================
// Loads the whole translated catalog of the current language once (an empty one in English).
export function loadTranslations(): Promise<void> {
  if (loading) return loading;
  loading = rpc<{ translations?: Record<string, string> }>(
    'i18n.translate',
    {},
    { timeoutMs: 10000 }
  )
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
  return loading;
}
//=============================================================================
