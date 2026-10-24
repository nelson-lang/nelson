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
  restoreCanvasNavigationTools,
  toggleCanvasNavigationTools,
} from './canvas-navigation-tools.js';
import { _resetUiPrefsForTests } from './ui-prefs.js';
//=============================================================================
beforeEach(() => {
  _resetUiPrefsForTests();
  localStorage.clear();
  document.body.innerHTML = '<div class="canvas-navigation-tools"></div>';
});
//=============================================================================
afterEach(() => {
  _resetUiPrefsForTests();
  localStorage.clear();
  document.body.innerHTML = '';
});
//=============================================================================
test('view controls are visible by default', () => {
  restoreCanvasNavigationTools();
  expect(document.querySelector<HTMLElement>('.canvas-navigation-tools')?.hidden).toBe(false);
});
//=============================================================================
test('view controls toggle and persist their visibility', () => {
  const tools = document.querySelector<HTMLElement>('.canvas-navigation-tools')!;

  toggleCanvasNavigationTools();
  expect(tools.hidden).toBe(true);
  expect(localStorage.getItem('nf-canvas-navigation-open')).toBe('0');

  toggleCanvasNavigationTools();
  expect(tools.hidden).toBe(false);
  expect(localStorage.getItem('nf-canvas-navigation-open')).toBe('1');
});
//=============================================================================
test('view controls restore the persisted hidden state', () => {
  localStorage.setItem('nf-canvas-navigation-open', '0');
  restoreCanvasNavigationTools();
  expect(document.querySelector<HTMLElement>('.canvas-navigation-tools')?.hidden).toBe(true);
});
//=============================================================================
