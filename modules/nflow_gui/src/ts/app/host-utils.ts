//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
export async function getUiRootPath(): Promise<string | null> {
  let root: string | null = null;
  try {
    root = sessionStorage.getItem('nf-root-path');
  } catch (_e) {
    root = null;
  }
  if (root) return root;
  try {
    root = localStorage.getItem('nf-root-path');
  } catch (_e) {
    root = null;
  }
  if (root) return root;
  try {
    const bs = (window as any).__nf_bootstrap;
    if (bs && typeof bs === 'object') {
      if (typeof bs.uiPath === 'string' && bs.uiPath.length > 0) return bs.uiPath;
      if (typeof bs.uiRoot === 'string' && bs.uiRoot.length > 0) return bs.uiRoot;
      if (typeof bs.rootPath === 'string' && bs.rootPath.length > 0) return bs.rootPath;
    }
  } catch (_e) {}
  try {
    const fn = (globalThis as any).getUiRootPath;
    if (typeof fn === 'function') {
      const r = await fn();
      if (r) return r;
    }
  } catch (_e) {}
  return null;
}
//=============================================================================
export async function getPreferencesPath(): Promise<string | null> {
  try {
    let v: string | null = null;
    try {
      v = sessionStorage.getItem('nf-preferences-path');
    } catch (_e) {
      v = null;
    }
    if (v) return v;
    try {
      v = localStorage.getItem('nf-preferences-path');
    } catch (_e) {
      v = null;
    }
    if (v) return v;

    try {
      const bs = (window as any).__nf_bootstrap;
      if (bs && typeof bs === 'object') {
        if (typeof bs.preferencesPath === 'string' && bs.preferencesPath.length > 0)
          return bs.preferencesPath;
        if (typeof bs.preferences === 'string' && bs.preferences.length > 0) return bs.preferences;
      }
    } catch (_e) {}

    try {
      const fn = (globalThis as any).getNFlowPreferencesDirectory;
      if (typeof fn === 'function') {
        try {
          const r = fn();
          if (r && typeof (r as any).then === 'function') return await r;
          if (typeof r === 'string') return r;
        } catch (_e) {}
      }
    } catch (_e) {}

    return null;
  } catch (_e) {
    return null;
  }
}
/**
 * Return the current locale. Checks sessionStorage/localStorage first,
 * then any bootstrapped `window.__nf_bootstrap` values, and finally any
 * legacy global `getCurrentLocale` function if present.
 */
export async function getCurrentLocale(): Promise<string | null> {
  try {
    let v: string | null = null;
    try {
      v = sessionStorage.getItem('nf-i18n-locale');
    } catch (_e) {
      v = null;
    }
    if (v) return v;
    try {
      v = localStorage.getItem('nf-i18n-locale');
    } catch (_e) {
      v = null;
    }
    if (v) return v;

    try {
      const bs = (window as any).__nf_bootstrap;
      if (bs && typeof bs === 'object') {
        if (typeof bs.locale === 'string' && bs.locale) return bs.locale;
        if (typeof bs.lang === 'string' && bs.lang) return bs.lang;
        if (typeof bs.uiLocale === 'string' && bs.uiLocale) return bs.uiLocale;
      }
    } catch (_e) {}

    try {
      const fn = (globalThis as any).getCurrentLocale;
      if (typeof fn === 'function') {
        try {
          const r = fn();
          if (r && typeof (r as any).then === 'function') return await r;
          if (typeof r === 'string') return r;
        } catch (_e) {}
      }
    } catch (_e) {}

    return null;
  } catch (_e) {
    return null;
  }
}
/**
 * Detect whether the host requested dark theme. Returns `true`/`false` when
 * a definitive answer is available, otherwise `null` so callers can fallback
 * to `matchMedia` or other logic.
 */
export async function isDarkMode(): Promise<boolean | null> {
  try {
    try {
      let s: string | null = null;
      try {
        s = sessionStorage.getItem('nf-theme-dark');
      } catch (_e) {
        s = null;
      }
      if (!s) {
        try {
          s = localStorage.getItem('nf-theme-dark');
        } catch (_e) {
          s = null;
        }
      }
      if (s !== null) return s === '1' || s === 'true';
    } catch (_e) {}

    try {
      const bs = (window as any).__nf_bootstrap;
      if (bs && typeof bs === 'object') {
        if (typeof bs.themeDark === 'boolean') return !!bs.themeDark;
        if (typeof bs.theme === 'string') return bs.theme === 'dark';
      }
    } catch (_e) {}

    try {
      const fn = (globalThis as any).isDarkMode;
      if (typeof fn === 'function') {
        try {
          const r = fn();
          if (r && typeof (r as any).then === 'function') {
            const awaited = await r;
            return awaited === true || String(awaited).toLowerCase() === 'true';
          }
          return r === true || String(r).toLowerCase() === 'true';
        } catch (_e) {}
      }
    } catch (_e) {}

    return null;
  } catch (_e) {
    return null;
  }
}
//=============================================================================
