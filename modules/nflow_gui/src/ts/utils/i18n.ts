//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Lightweight i18n for NFlow frontend
// - initI18n(locale): loads `locale/nflow-ui-<locale>.json` (tries a few paths)
// - t(key, params?): lookup with simple {name} interpolation
// - localizeDOM(root?): apply translations to elements with data-i18n attributes
// Indirection: keeps `new URL(rel, base)` resolved at runtime instead of
// being rewritten by Vite's build-time asset analysis.
const MODULE_URL: string = import.meta.url;
/* eslint-disable @typescript-eslint/no-explicit-any */
type Translations = { [key: string]: string };
let translations: Translations = {};
let currentLocale = 'en_US';
async function fetchJson(path: string) {
  const res = await fetch(path);
  if (!res.ok) throw new Error(`fetch ${path} failed: ${res.status}`);
  return res.json();
}
//=============================================================================
export async function initI18n(locale: string): Promise<void> {
  locale = (locale || '').toString().trim();
  const tried = new Set<string>();

  async function tryLoad(lang: string): Promise<boolean> {
    if (!lang) return false;
    if (tried.has(lang)) return false;
    tried.add(lang);

    // Module-relative path is the most reliable when served from the built `dist` tree.
    let moduleRel: string | null = null;
    try {
      moduleRel = new URL(`../locale/nflow-ui-${lang}.json`, MODULE_URL).href;
    } catch (_e) {
      moduleRel = null;
    }

    const candidates = [
      `./locale/nflow-ui-${lang}.json`,
      `locale/nflow-ui-${lang}.json`,
      moduleRel,
      `/locale/nflow-ui-${lang}.json`,
    ].filter(Boolean) as string[];

    for (const p of candidates) {
      try {
        const json = await fetchJson(p);
        translations = json as Translations;
        currentLocale = lang;
        try {
          (window as any).__nf_translate = (k: string, p?: Record<string, string | number>) =>
            t(k, p);
          (window as any).__nf_i18n_locale = currentLocale;
          (window as any).__nf_i18n_loaded = true;
        } catch (_e) {}
        // Persist to localStorage so head-init can use cached translations
        // synchronously on the next page load (avoids English flash).
        try {
          localStorage.setItem('nf-i18n-locale', currentLocale);
          localStorage.setItem('nf-i18n-translations', JSON.stringify(translations));
        } catch (_e) {}
        return true;
      } catch (e) {
        // eslint-disable-next-line no-console
        // console.warn(
        //   `[i18n] failed to load ${p}:`,
        //   e && (e as any).message ? (e as any).message : e
        // );
      }
    }
    return false;
  }

  if (!(await tryLoad(locale))) {
    await tryLoad('en_US');
  }
}
//=============================================================================
export function t(key: string, params?: Record<string, string | number>): string {
  const raw = translations[key];
  const value = typeof raw === 'string' && raw.length > 0 ? raw : key;
  if (!params) return value;
  return Object.keys(params).reduce(
    (s, k) => s.replace(new RegExp(`\\{${k}\\}`, 'g'), String(params[k])),
    value
  );
}
//=============================================================================
export function localizeDOM(root: ParentNode = document): void {
  root.querySelectorAll<HTMLElement>('[data-i18n]').forEach((el) => {
    const key = el.getAttribute('data-i18n');
    if (key) el.textContent = t(key);
  });

  root.querySelectorAll<HTMLElement>('[data-i18n-title]').forEach((el) => {
    const key = el.getAttribute('data-i18n-title');
    if (!key) return;
    // data-shortcut appends a literal accelerator, e.g. "Reset View (Ctrl+0)".
    const shortcut = el.getAttribute('data-shortcut');
    el.title = shortcut ? `${t(key)} (${shortcut})` : t(key);
  });

  root.querySelectorAll<HTMLElement>('[data-i18n-aria]').forEach((el) => {
    const key = el.getAttribute('data-i18n-aria');
    if (key) el.setAttribute('aria-label', t(key));
  });

  root.querySelectorAll<HTMLElement>('[data-i18n-alt]').forEach((el) => {
    const key = el.getAttribute('data-i18n-alt');
    if (key) el.setAttribute('alt', t(key));
  });

  root
    .querySelectorAll<HTMLInputElement | HTMLTextAreaElement>('[data-i18n-placeholder]')
    .forEach((el) => {
      const key = el.getAttribute('data-i18n-placeholder');
      if (key) el.placeholder = t(key);
    });

  root.querySelectorAll<HTMLElement>('[data-i18n-value]').forEach((el) => {
    const key = el.getAttribute('data-i18n-value');
    if (!key) return;
    if (el instanceof HTMLInputElement || el instanceof HTMLButtonElement) {
      (el as HTMLInputElement).value = t(key);
    } else {
      el.setAttribute('value', t(key));
    }
  });
}
//=============================================================================
export function getLocale(): string {
  return currentLocale;
}
//=============================================================================
export function getTranslations(): Translations {
  return translations;
}
//=============================================================================
