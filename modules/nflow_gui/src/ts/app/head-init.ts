//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
/* NOTE: window types are declared in global-window.d.ts */
// Start downloading the builtin icon sprite early: this is the first script the
// page runs, so the request goes out during the "Scanning libraries" phase. The
// sprite bundles all builtin block icons, so the icon cache resolves it from cache
// instead of fetching per icon over the slow WebView2 host server.
try {
  const spriteHref = new URL('libraries/_builtin-icons.sprite.svg', document.baseURI).href;
  const link = document.createElement('link');
  link.rel = 'preload';
  link.as = 'fetch';
  link.href = spriteHref;
  link.crossOrigin = 'anonymous';
  document.head.appendChild(link);
} catch (_e) {}
//=============================================================================
// Read host-provided bootstrap variables (if injected by C++ as
// `window.__nf_bootstrap`) and persist a copy to sessionStorage so later
// modules can access them synchronously in the same window navigation.
try {
  const _bs = (window as any).__nf_bootstrap;
  if (_bs && typeof _bs === 'object') {
    try {
      sessionStorage.setItem('nf-bootstrap', JSON.stringify(_bs));
    } catch (_e) {}
    (window as any).__nf_bootstrap = _bs;
  }
} catch (_e) {}
//=============================================================================
// Provide fallback global helpers when the host no longer injects bindings
// (e.g., getUiRootPath, getCurrentLocale, isDarkMode). These read the
// pre-seeded `nf-bootstrap`/sessionStorage/localStorage values so TypeScript
// code can rely on them synchronously.
try {
  const _readBootstrap = (): any => {
    try {
      const s = sessionStorage.getItem('nf-bootstrap');
      if (s) return JSON.parse(s);
    } catch (_e) {}
    try {
      const bs = (window as any).__nf_bootstrap;
      if (bs && typeof bs === 'object') return bs;
    } catch (_e) {}
    return null;
  };

  if (typeof (globalThis as any).getUiRootPath !== 'function') {
    (globalThis as any).getUiRootPath = async (): Promise<string | null> => {
      try {
        let v: string | null = null;
        try {
          v = sessionStorage.getItem('nf-root-path');
        } catch (_e) {
          v = null;
        }
        if (!v) {
          try {
            v = localStorage.getItem('nf-root-path');
          } catch (_e) {
            v = null;
          }
        }
        if (v) return v;
        const bs = _readBootstrap();
        if (bs && typeof bs.uiPath === 'string') return bs.uiPath;
        return null;
      } catch (_e) {
        return null;
      }
    };
  }

  if (typeof (globalThis as any).getCurrentLocale !== 'function') {
    (globalThis as any).getCurrentLocale = (): string | null => {
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
        const bs = _readBootstrap();
        if (bs && typeof bs.locale === 'string') return bs.locale || null;
      } catch (_e) {}
      return null;
    };
  }

  if (typeof (globalThis as any).isDarkMode !== 'function') {
    (globalThis as any).isDarkMode = async (): Promise<boolean> => {
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
        if (s) return s === '1' || s === 'true';
      } catch (_e) {}
      return !!(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);
    };
  }
} catch (_e) {}
//=============================================================================
try {
  // Instant theme, applied synchronously as the very first JS on the page.
  // Primary: URL query param ?dark=1 set by the C++ host in show_wv(), readable
  // with no file I/O and no write permission.
  try {
    let _initialDark: boolean | undefined = undefined;
    try {
      const _darkQuery = new URLSearchParams(window.location.search).get('dark');
      if (_darkQuery !== null) {
        _initialDark = _darkQuery === '1' || _darkQuery === 'true';
      }
    } catch (_e) {}
    // Next: data-theme attribute baked into the HTML by the build script
    // (nflow_dark.html / nflow_light.html). It is set before any JS runs, so no IPC.
    try {
      const _existingTheme = document.documentElement.getAttribute('data-theme');
      if (_initialDark === undefined && _existingTheme === 'dark') {
        _initialDark = true;
      } else if (_initialDark === undefined && _existingTheme === 'light') {
        _initialDark = false;
      }
    } catch (_e) {}
    // Fallback: sessionStorage for same-window bootstrap, then localStorage
    // persisted from a previous session.
    if (_initialDark === undefined) {
      try {
        let _stored: string | null = null;
        try {
          _stored = sessionStorage.getItem('nf-theme-dark');
        } catch (_e) {
          _stored = null;
        }
        if (!_stored) {
          try {
            _stored = localStorage.getItem('nf-theme-dark');
          } catch (_e) {
            _stored = null;
          }
        }
        if (_stored !== null) {
          _initialDark = _stored === '1' || _stored === 'true';
        }
      } catch (_e) {}
    }
    // Persist the detected value so later loads can fall back on localStorage.
    if (_initialDark !== undefined) {
      try {
        localStorage.setItem('nf-theme-dark', _initialDark ? '1' : '0');
      } catch (_e) {}
    }
    if (_initialDark !== undefined) {
      if (_initialDark) {
        document.documentElement.setAttribute('data-theme', 'dark');
      } else {
        document.documentElement.removeAttribute('data-theme');
      }
      document.documentElement.style.background = _initialDark ? '#1e1e1e' : '#ffffff';
      document.documentElement.style.color = _initialDark ? '#cccccc' : '#333333';
    }
  } catch (_e) {}
} catch (_e) {}
//=============================================================================
try {
  // Early theme detection - runs synchronously before any visible content renders.
  // Defines window.__nf_set_theme so app.ts can confirm/update the result later.
  const _applyEarlyTheme = (isDark: boolean): void => {
    try {
      if (isDark) {
        document.documentElement.setAttribute('data-theme', 'dark');
      } else {
        document.documentElement.removeAttribute('data-theme');
      }
      // Set background on <html> directly so the very first painted pixel uses the
      // right colour - the overlay and body inherit this before any stylesheet loads.
      document.documentElement.style.background = isDark ? '#1e1e1e' : '#ffffff';
      document.documentElement.style.color = isDark ? '#cccccc' : '#333333';
    } catch (_e) {}
    // Tint loading overlay inline when it is already in the DOM (re-apply call).
    try {
      const overlay = document.querySelector('.nf-loading-overlay') as HTMLElement | null;
      if (overlay) overlay.style.background = isDark ? '#1e1e1e' : '#fff';
      const loadingText = document.querySelector('.nf-loading-text') as HTMLElement | null;
      if (loadingText) loadingText.style.color = isDark ? '#cccccc' : '#222';
      const spinner = document.querySelector('.nf-loading-spinner') as HTMLElement | null;
      if (spinner) {
        spinner.style.borderColor = isDark ? 'rgba(255,255,255,0.08)' : 'rgba(0,0,0,0.08)';
        spinner.style.borderTopColor = isDark ? 'rgba(255,255,255,0.9)' : 'rgba(0,0,0,0.9)';
      }
    } catch (_e) {}
  };

  (window as any).__nf_set_theme = _applyEarlyTheme;

  const _detectAndApplyTheme = async (): Promise<void> => {
    try {
      const isDarkFn = (window as any).isDarkMode;
      if (typeof isDarkFn === 'function') {
        const res = await isDarkFn();
        const isDark = res === true || String(res).toLowerCase() === 'true';
        (window as any).__nf_theme_detected = isDark;
        _applyEarlyTheme(isDark);
        // Persist for next load - so the correct theme is applied instantly.
        try {
          localStorage.setItem('nf-theme-dark', isDark ? '1' : '0');
        } catch (_e) {}
        return;
      }
    } catch (_e) {}
    const isDark = !!(
      window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches
    );
    (window as any).__nf_theme_detected = isDark;
    _applyEarlyTheme(isDark);
  };

  if (typeof (window as any).isDarkMode === 'function') {
    void _detectAndApplyTheme();
  } else {
    // If the HTML already has a baked data-theme attribute (from nflow_dark/light.html),
    // respect it - do NOT overwrite with the OS media-query fallback.
    const _bakedTheme = document.documentElement.getAttribute('data-theme');
    if (_bakedTheme === 'dark' || _bakedTheme === 'light') {
      const _initialDark = _bakedTheme === 'dark';
      (window as any).__nf_theme_detected = _initialDark;
      _applyEarlyTheme(_initialDark);
    } else {
      const _initialDark = !!(
        window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches
      );
      (window as any).__nf_theme_detected = _initialDark;
      _applyEarlyTheme(_initialDark);
    }

    // setTimeout(0) fires in the NEXT task - after all synchronous <script> tags on the
    // page have executed (including the host-injected webui.js that exposes isDarkMode).
    // This is the earliest possible point at which isDarkMode is guaranteed to be present.
    setTimeout((): void => {
      if (typeof (window as any).isDarkMode === 'function') {
        void _detectAndApplyTheme();
        return;
      }
      // isDarkMode still not available - keep polling as a last-resort fallback.
      let _themeRetries = 0;
      const _themeRetryId = setInterval((): void => {
        _themeRetries += 1;
        if (typeof (window as any).isDarkMode === 'function') {
          clearInterval(_themeRetryId);
          void _detectAndApplyTheme();
        } else if (_themeRetries >= 30) {
          clearInterval(_themeRetryId);
        }
      }, 100);
    }, 0);
  }
} catch (_e) {}
//=============================================================================
try {
  document.documentElement.classList.add('nf-loading');
  try {
    // Quick inline translation for the first paint: if the host pre-seeded a
    // locale, translate the overlay text now so users do not see English before
    // the i18n files load.
    try {
      let _storedLocale: string | null = null;
      try {
        _storedLocale = sessionStorage.getItem('nf-i18n-locale');
      } catch (_e) {
        _storedLocale = null;
      }
      if (!_storedLocale) {
        try {
          _storedLocale = localStorage.getItem('nf-i18n-locale');
        } catch (_e) {
          _storedLocale = null;
        }
      }

      if (_storedLocale) {
        const _lc = String(_storedLocale).toLowerCase();
        if (_lc.startsWith('fr')) {
          try {
            const lt = document.querySelector('.nf-loading-text') as HTMLElement | null;
            if (lt) lt.textContent = 'Chargement…';
            const st = document.getElementById('nf-loading-status') as HTMLElement | null;
            if (st) st.textContent = 'Démarrage…';
          } catch (_e) {}
        }
      }
    } catch (_e) {}
  } catch (_e) {}

  // Early i18n bootstrap. The C++ host pre-seeds localStorage with 'nf-i18n-locale'
  // before redirecting to nflow.html. Cached translations are applied at once so
  // the MutationObserver below can translate data-i18n elements early.
  const _i18nTranslations: Record<string, string> = {};

  try {
    let _cached: string | null = null;
    let _cachedLocale: string | null = null;
    try {
      _cached = sessionStorage.getItem('nf-i18n-translations');
    } catch (_e) {
      _cached = null;
    }
    if (!_cached) {
      try {
        _cached = localStorage.getItem('nf-i18n-translations');
      } catch (_e) {
        _cached = null;
      }
    }
    try {
      _cachedLocale = sessionStorage.getItem('nf-i18n-locale');
    } catch (_e) {
      _cachedLocale = null;
    }
    if (!_cachedLocale) {
      try {
        _cachedLocale = localStorage.getItem('nf-i18n-locale');
      } catch (_e) {
        _cachedLocale = null;
      }
    }
    if (_cached && _cachedLocale && _cachedLocale !== 'en_US') {
      Object.assign(_i18nTranslations, JSON.parse(_cached));
    }
  } catch (_e) {}

  const _i18nTranslate = (key: string): string => {
    const v = _i18nTranslations[key];
    return typeof v === 'string' && v.length > 0 ? v : key;
  };

  // MutationObserver: translate [data-i18n] elements as the parser adds them,
  // so the very first paint already shows the correct language.
  try {
    if (Object.keys(_i18nTranslations).length > 0) {
      const _translateEl = (el: Element) => {
        const k = el.getAttribute('data-i18n');
        if (k) el.textContent = _i18nTranslate(k);
      };
      const _obs = new MutationObserver((mutations) => {
        for (const m of mutations) {
          for (const node of m.addedNodes) {
            if (node.nodeType !== 1) continue;
            const el = node as Element;
            if (el.hasAttribute && el.hasAttribute('data-i18n')) _translateEl(el);
            try {
              el.querySelectorAll?.('[data-i18n]').forEach(_translateEl);
            } catch (_e) {}
          }
        }
      });
      _obs.observe(document.documentElement, { childList: true, subtree: true });
      window.addEventListener('DOMContentLoaded', () => _obs.disconnect(), { once: true });
    }
  } catch (_e) {}

  (window as any).__nf_translate = _i18nTranslate;

  // Async fetch refreshes the translations (covers the first load, when storage is
  // empty).
  const _i18nFetchLocale = async (locale: string): Promise<boolean> => {
    const candidates = [
      `./locale/nflow-ui-${locale}.json`,
      `/locale/nflow-ui-${locale}.json`,
      `locale/nflow-ui-${locale}.json`,
    ];
    for (const url of candidates) {
      try {
        const res = await fetch(url);
        if (!res.ok) continue;
        const json = await res.json();
        Object.assign(_i18nTranslations, json);
        (window as any).__nf_i18n_locale = locale;
        (window as any).__nf_i18n_loaded = true;
        try {
          document.querySelectorAll<HTMLElement>('[data-i18n]').forEach((el) => {
            const k = el.getAttribute('data-i18n');
            if (k) el.textContent = _i18nTranslate(k);
          });
        } catch (_e) {}
        try {
          try {
            sessionStorage.setItem('nf-i18n-locale', locale);
            sessionStorage.setItem('nf-i18n-translations', JSON.stringify(_i18nTranslations));
          } catch (_e) {}
          localStorage.setItem('nf-i18n-locale', locale);
          localStorage.setItem('nf-i18n-translations', JSON.stringify(_i18nTranslations));
        } catch (_e) {}
        return true;
      } catch (_e) {
        /* try next */
      }
    }
    return false;
  };

  (async () => {
    try {
      // Prefer the locale cached by the C++ bootstrap page; otherwise poll
      // getCurrentLocale (dev-server mode).
      let _locale: string | null = null;
      try {
        const _cached = localStorage.getItem('nf-i18n-locale');
        if (_cached && _cached !== 'en_US') _locale = _cached;
      } catch (_e) {}

      if (!_locale) {
        const _getLocale = (): string | null => {
          try {
            const fn = (window as any).getCurrentLocale;
            if (typeof fn !== 'function') return null;
            const r = fn();
            if (typeof r === 'string' && r) return r;
          } catch (_e) {}
          return null;
        };

        _locale = _getLocale();
        if (!_locale) {
          for (let _i = 0; _i < 30 && !_locale; _i++) {
            await new Promise<void>((r) => setTimeout(r, 50));
            _locale = _getLocale();
          }
        }

        if (!_locale) {
          try {
            const fn = (window as any).getCurrentLocale;
            if (typeof fn === 'function') {
              const r = fn();
              if (r && typeof (r as any).then === 'function') {
                _locale = await r;
              }
            }
          } catch (_e) {}
        }
      }

      if (_locale && _locale !== 'en_US') {
        await _i18nFetchLocale(_locale);
      }
    } catch (_e) {}
  })();

  const nfReadyPromise = new Promise<void>((resolve) => {
    document.addEventListener('nf-ready', () => resolve(), { once: true });
  });

  const setLoadingStatus = (text: string | null, withEllipsis = false) => {
    try {
      const el = document.getElementById('nf-loading-status');
      if (el) {
        if (text) {
          try {
            el.setAttribute('data-i18n', text);
          } catch (_e) {}
          el.textContent = _i18nTranslate(text);
        } else {
          el.textContent = '';
        }

        if (withEllipsis) el.classList.add('nf-loading-step-ellipsis');
        else el.classList.remove('nf-loading-step-ellipsis');
      }
      const main = document.querySelector('.nf-loading-text');
      if (main && text) {
        try {
          main.setAttribute('aria-label', _i18nTranslate(text));
        } catch (_e) {}
      }
    } catch (e) {}
  };

  try {
    document.addEventListener('nf-libraries-start', () =>
      setLoadingStatus('Scanning libraries…', true)
    );
    document.addEventListener('nf-libraries-ready', () => setLoadingStatus('Libraries loaded'));
    document.addEventListener('nf-ready', () => setLoadingStatus('Initializing UI…'));
    if ((window as any).__nf_init_libraries_promise) {
      setLoadingStatus('Scanning libraries…', true);
      (window as any).__nf_init_libraries_promise.finally(() =>
        setLoadingStatus('Libraries loaded')
      );
    }
  } catch (e) {}

  const libsPromise = new Promise<void>((resolve) => {
    if ((window as any).__nf_init_libraries_promise) {
      (window as any).__nf_init_libraries_promise.finally(() => resolve());
    } else {
      document.addEventListener('nf-libraries-ready', () => resolve(), { once: true });
      document.addEventListener(
        'nf-libraries-start',
        () => {
          document.documentElement.classList.add('nf-loading-activity');
        },
        { once: true }
      );
    }
  });

  libsPromise.finally(() => document.documentElement.classList.remove('nf-loading-activity'));

  const fallback = new Promise<void>((resolve) => setTimeout(resolve, 15000));

  // Remove the loading overlay once the app signals readiness (nf-ready).
  // Do NOT block overlay removal on library loading so the UI can appear
  // while libraries continue loading in the background.
  Promise.race([nfReadyPromise, fallback]).then(() => {
    try {
      document.documentElement.classList.remove('nf-loading');
    } catch (_e) {}
  });
} catch (e) {}
//=============================================================================
try {
  document.addEventListener(
    'nf-ready',
    () => {
      try {
        document.body.tabIndex = -1;
      } catch (e) {}
      try {
        if (window.focus) window.focus();
      } catch (e) {}
      try {
        if (document.body.focus) document.body.focus();
      } catch (e) {}
    },
    { once: true }
  );
  window.addEventListener('focus', () => {
    try {
      if (window.focus) window.focus();
    } catch (e) {}
    try {
      if (document.body.focus) document.body.focus();
    } catch (e) {}
  });
} catch (e) {}
//=============================================================================
