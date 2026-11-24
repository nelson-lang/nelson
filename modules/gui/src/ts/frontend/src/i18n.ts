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
// Cache the last catalog in localStorage so the first render after a cold start
// is already translated, instead of flashing untranslated labels until the
// i18n.translate RPC returns. The RPC still runs on every launch and overwrites
// the cache, so a changed catalog is corrected within the first seconds.
const CATALOG_CACHE_KEY = 'nelson-gui.i18n.catalog';
function loadCachedCatalog(): void {
  try {
    const raw = localStorage.getItem(CATALOG_CACHE_KEY);
    if (!raw) return;
    const parsed: unknown = JSON.parse(raw);
    if (parsed && typeof parsed === 'object') {
      for (const [key, text] of Object.entries(parsed as Record<string, unknown>)) {
        if (typeof text === 'string' && text.length > 0) translations.set(key, text);
      }
    }
  } catch {
    // Cache is optional; the RPC remains authoritative.
  }
}
function saveCachedCatalog(catalog: Record<string, string>): void {
  try {
    localStorage.setItem(CATALOG_CACHE_KEY, JSON.stringify(catalog));
  } catch {
    // Non-fatal: the catalog is reloaded from the host next launch.
  }
}
loadCachedCatalog();
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
      const catalog = r.translations ?? {};
      let changed = false;
      for (const [key, text] of Object.entries(catalog)) {
        if (typeof text === 'string' && text.length > 0 && translations.get(key) !== text) {
          translations.set(key, text);
          changed = true;
        }
      }
      saveCachedCatalog(catalog);
      if (changed) listeners.forEach((cb) => cb());
    })
    .catch(() => undefined);
  return loading;
}
//=============================================================================
