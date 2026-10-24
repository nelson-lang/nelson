//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { bindSettingsPreferences } from './settings-preferences.js';
import { _resetUiPrefsForTests } from './ui-prefs.js';
//=============================================================================
beforeEach(() => {
  _resetUiPrefsForTests();
  localStorage.clear();
  document.body.innerHTML = `
    <select id="codegenLang"><option value="rust">Rust</option><option value="c">C</option></select>
    <input id="codegenIncludeMain" type="checkbox">
    <input id="codegenBuildAfterGen" type="checkbox">
  `;
});
//=============================================================================
test('restores and updates code generation settings', () => {
  localStorage.setItem('nf-codegen-language', 'c');
  localStorage.setItem('nf-codegen-include-main', '0');
  localStorage.setItem('nf-codegen-build-after', '1');
  bindSettingsPreferences();

  const language = document.getElementById('codegenLang') as HTMLSelectElement;
  const includeMain = document.getElementById('codegenIncludeMain') as HTMLInputElement;
  const buildAfter = document.getElementById('codegenBuildAfterGen') as HTMLInputElement;
  expect(language.value).toBe('c');
  expect(includeMain.checked).toBe(false);
  expect(buildAfter.checked).toBe(true);

  language.value = 'rust';
  language.dispatchEvent(new Event('change'));
  includeMain.checked = true;
  includeMain.dispatchEvent(new Event('change'));
  expect(localStorage.getItem('nf-codegen-language')).toBe('rust');
  expect(localStorage.getItem('nf-codegen-include-main')).toBe('1');
});
//=============================================================================
