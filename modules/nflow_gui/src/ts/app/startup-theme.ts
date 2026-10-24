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
import { loadThemes, applyTheme, getThemes } from './theme-manager.js';
import { isDarkMode } from './host-utils.js';
//=============================================================================
// Picks the light or dark theme at startup (seeded value, host, then OS).
export async function applyStartupTheme(): Promise<void> {
  await loadThemes();
  const themes = getThemes();
  if (themes.length > 0) {
    let chosenTheme: any = themes.find((t: any) => t.id === 'light') || themes[0];
    let isDarkDetected = false;
    try {
      // Prefer seeded value from sessionStorage (same-window) then localStorage.
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
        isDarkDetected = _stored === '1' || _stored === 'true';
        chosenTheme = isDarkDetected
          ? themes.find((t: any) => t.id === 'dark') || themes[0]
          : themes.find((t: any) => t.id === 'light') || themes[0];
      } else {
        try {
          const r = await isDarkMode();
          if (r !== null) {
            isDarkDetected = Boolean(r);
            chosenTheme = isDarkDetected
              ? themes.find((t: any) => t.id === 'dark') || themes[0]
              : themes.find((t: any) => t.id === 'light') || themes[0];
          } else if (
            window.matchMedia &&
            window.matchMedia('(prefers-color-scheme: dark)').matches
          ) {
            chosenTheme = themes.find((t: any) => t.id === 'dark') || themes[0];
            isDarkDetected = true;
          } else {
            chosenTheme = themes.find((t: any) => t.id === 'light') || themes[0];
            isDarkDetected = false;
          }
        } catch (_e) {
          chosenTheme = themes.find((t: any) => t.id === 'light') || themes[0];
          isDarkDetected = false;
        }
      }
    } catch (_e) {
      chosenTheme = themes.find((t: any) => t.id === 'light') || themes[0];
      isDarkDetected = false;
    }

    applyTheme(chosenTheme.id);

    try {
      const setInline = (window as any).__nf_set_theme;
      if (typeof setInline === 'function') setInline(Boolean(isDarkDetected));
    } catch (e) {}
  }
}
//=============================================================================
