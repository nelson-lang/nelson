// ============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================

/* Ambient global window augmentations used by the web UI runtime */
declare global {
  interface Window {
    __nf_init_libraries_promise?: Promise<any>;
    /** Native host API - resolves to true when the OS/app is in dark mode. */
    isDarkMode?: () => Promise<boolean | string>;
    /** Native host API - returns current locale (e.g. "en_US"). */
    getCurrentLocale?: () => string | Promise<string>;
    /** Native host API - returns UI root path or installation path. */
    getUiRootPath?: () => string | Promise<string>;
    /** Native host API - returns preferences directory path. */
    getNFlowPreferencesDirectory?: () => string | Promise<string>;
    /** Applied early by head-init; app.ts calls this again after full theme load. */
    __nf_set_theme?: (isDark: boolean) => void;
    /** Cached result of the last isDarkMode / matchMedia detection. */
    __nf_theme_detected?: boolean;
    [key: string]: any;
  }
}

export {};
