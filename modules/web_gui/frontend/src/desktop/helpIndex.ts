//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Shared help resolution: the documentation language and the built-in search
// index (title → page). Used by the dedicated help window (which needs a
// synchronous answer inside a click handler) and by the docked HelpPanel
// fallback. The index is served same-origin by the help service worker from the
// `main` bundle, so it is cheap to fetch and the SW caches it.
import { rpc } from '../rpc/client';
//=============================================================================
type HelpIndexEntry = { title?: string; url?: string; path?: string };
export type HelpTarget = { open?: string; search?: string };
//=============================================================================
let cachedLang: string | null = null;
let langPromise: Promise<string> | null = null;
let indexCache: HelpIndexEntry[] | null = null;
let indexPromise: Promise<HelpIndexEntry[]> | null = null;
//=============================================================================
// The last-resolved language, for gesture handlers that must act synchronously.
// Defaults to English until the one-shot probe resolves.
export function getHelpLangSync(): string {
  return cachedLang ?? 'en_US';
}
//=============================================================================
export async function getHelpLang(): Promise<string> {
  if (cachedLang) return cachedLang;
  if (!langPromise) {
    langPromise = (async () => {
      try {
        const r = await rpc<{ lang?: string }>('help.lang', {});
        cachedLang = r?.lang === 'fr_FR' ? 'fr_FR' : 'en_US';
      } catch {
        cachedLang = 'en_US';
      }
      return cachedLang;
    })();
  }
  return langPromise;
}
//=============================================================================
// Re-probe the engine's documentation language. When it changed since the last
// probe, drop the cached search index so the new language's index is fetched on
// the next resolve. Returns the current language. There is no language-change
// push to subscribe to, so a long-lived help window follows a language switch by
// calling this on each open. Best-effort: keeps the known language on failure.
export async function refreshHelpLang(): Promise<string> {
  let next: string;
  try {
    const r = await rpc<{ lang?: string }>('help.lang', {});
    next = r?.lang === 'fr_FR' ? 'fr_FR' : 'en_US';
  } catch {
    return cachedLang ?? 'en_US';
  }
  if (next !== cachedLang) {
    cachedLang = next;
    indexCache = null;
    indexPromise = null;
  }
  return next;
}
//=============================================================================
async function loadIndex(lang: string): Promise<HelpIndexEntry[]> {
  if (indexCache) return indexCache;
  if (!indexPromise) {
    indexPromise = (async () => {
      try {
        const res = await fetch(
          new URL(`help/${lang}/index.json`, window.location.href).toString()
        );
        const data = res.ok ? await res.json() : [];
        indexCache = Array.isArray(data) ? (data as HelpIndexEntry[]) : [];
      } catch {
        indexCache = [];
      }
      return indexCache;
    })();
  }
  return indexPromise;
}
//=============================================================================
function resolveFrom(index: HelpIndexEntry[], name: string): HelpTarget {
  const exact = index.find((entry) => (entry.title || '').toLowerCase() === name.toLowerCase());
  if (exact && (exact.url || exact.path)) {
    return { open: (exact.url || exact.path) as string };
  }
  // A partial or misspelt name: let the viewer show matching results.
  return { search: name };
}
//=============================================================================
// Resolve from the already-primed cache, or null when it is not loaded yet so
// the caller can open the viewer immediately and refine once the index arrives.
export function resolveHelpTargetSync(name: string): HelpTarget | null {
  const trimmed = (name || '').trim();
  if (!trimmed) return {};
  if (!indexCache) return null;
  return resolveFrom(indexCache, trimmed);
}
//=============================================================================
export async function resolveHelpTarget(name: string): Promise<HelpTarget> {
  const trimmed = (name || '').trim();
  if (!trimmed) return {};
  const lang = await getHelpLang();
  const index = await loadIndex(lang);
  return resolveFrom(index, trimmed);
}
//=============================================================================
// Warm the language and search index at startup so a later gesture (toolbar
// button, `doc` from the terminal) can open the right page synchronously.
export function primeHelpIndex(): void {
  void getHelpLang().then((lang) => {
    void loadIndex(lang);
  });
}
//=============================================================================
