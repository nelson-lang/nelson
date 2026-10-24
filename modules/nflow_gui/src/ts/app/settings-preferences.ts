//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getUiPref, setUiPref } from './ui-prefs.js';
//=============================================================================
const LANGUAGE_KEY = 'nf-codegen-language';
const INCLUDE_MAIN_KEY = 'nf-codegen-include-main';
const BUILD_AFTER_KEY = 'nf-codegen-build-after';
function restoreCheckbox(element: HTMLInputElement, key: string, fallback: boolean): void {
  element.checked = getUiPref(key, fallback ? '1' : '0') === '1';
}
//=============================================================================
export function bindSettingsPreferences(): void {
  const language = document.getElementById('codegenLang') as HTMLSelectElement | null;
  const includeMain = document.getElementById('codegenIncludeMain') as HTMLInputElement | null;
  const buildAfter = document.getElementById('codegenBuildAfterGen') as HTMLInputElement | null;
  if (!language || !includeMain || !buildAfter || language.dataset.preferencesBound === '1') return;

  const savedLanguage = getUiPref(LANGUAGE_KEY, 'rust');
  language.value = savedLanguage === 'c' ? 'c' : 'rust';
  restoreCheckbox(includeMain, INCLUDE_MAIN_KEY, true);
  restoreCheckbox(buildAfter, BUILD_AFTER_KEY, false);

  language.dataset.preferencesBound = '1';
  language.addEventListener('change', () => setUiPref(LANGUAGE_KEY, language.value));
  includeMain.addEventListener('change', () =>
    setUiPref(INCLUDE_MAIN_KEY, includeMain.checked ? '1' : '0')
  );
  buildAfter.addEventListener('change', () =>
    setUiPref(BUILD_AFTER_KEY, buildAfter.checked ? '1' : '0')
  );
}
//=============================================================================
