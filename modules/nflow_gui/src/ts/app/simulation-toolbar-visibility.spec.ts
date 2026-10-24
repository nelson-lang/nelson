//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, beforeEach, expect, test } from 'vitest';
import {
  restoreSimulationToolbar,
  toggleSimulationToolbar,
} from './simulation-toolbar-visibility.js';
import { _resetUiPrefsForTests } from './ui-prefs.js';
//=============================================================================
beforeEach(() => {
  _resetUiPrefsForTests();
  localStorage.clear();
  document.body.innerHTML = '<div id="floatingToolbar" style="display: flex"></div>';
});
//=============================================================================
afterEach(() => {
  _resetUiPrefsForTests();
  localStorage.clear();
  document.body.innerHTML = '';
});
//=============================================================================
test('simulation toolbar is visible by default', () => {
  restoreSimulationToolbar();
  expect(document.getElementById('floatingToolbar')?.hidden).toBe(false);
});
//=============================================================================
test('simulation toolbar toggles and persists its visibility', () => {
  const toolbar = document.getElementById('floatingToolbar')!;

  toggleSimulationToolbar();
  expect(toolbar.hidden).toBe(true);
  expect(localStorage.getItem('nf-simulation-toolbar-open')).toBe('0');

  toggleSimulationToolbar();
  expect(toolbar.hidden).toBe(false);
  expect(localStorage.getItem('nf-simulation-toolbar-open')).toBe('1');
});
//=============================================================================
test('simulation toolbar restores the persisted hidden state', () => {
  localStorage.setItem('nf-simulation-toolbar-open', '0');
  restoreSimulationToolbar();
  expect(document.getElementById('floatingToolbar')?.hidden).toBe(true);
});
//=============================================================================
