//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as sh from './shortcuts-help';
//=============================================================================
test('shortcuts-help module loads', () => {
  expect(typeof sh).toBe('object');
});
//=============================================================================
import { showShortcutsHelp, hideShortcutsHelp } from '../../../dist/app/shortcuts-help.js';
//=============================================================================
describe('shortcuts help overlay', () => {
  afterEach(() => {
    try {
      hideShortcutsHelp();
    } catch (e) {}
    const back = document.getElementById('nflow-shortcuts-backdrop');
    if (back && back.parentNode) back.parentNode.removeChild(back);
    const modal = document.getElementById('nflow-shortcuts-help');
    if (modal && modal.parentNode) modal.parentNode.removeChild(modal);
  });

  test('showShortcutsHelp adds backdrop and modal', () => {
    showShortcutsHelp();
    const back = document.getElementById('nflow-shortcuts-backdrop');
    const modal = document.getElementById('nflow-shortcuts-help');
    expect(back).not.toBeNull();
    expect(modal).not.toBeNull();
    expect((modal as HTMLElement).style.display).toBe('block');
  });

  test('hideShortcutsHelp removes modal and backdrop', () => {
    showShortcutsHelp();
    hideShortcutsHelp();
    expect(document.getElementById('nflow-shortcuts-backdrop')).toBeNull();
    expect(document.getElementById('nflow-shortcuts-help')).toBeNull();
  });
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/shortcuts-help.js');
});
//=============================================================================
test('showShortcutsHelp appends modal and backdrop', () => {
  document.body.innerHTML = '';
  mod.showShortcutsHelp();
  const back = document.getElementById('nflow-shortcuts-backdrop');
  const modal = document.getElementById('nflow-shortcuts-help');
  expect(back).toBeTruthy();
  expect(modal).toBeTruthy();
  mod.hideShortcutsHelp();
  expect(document.getElementById('nflow-shortcuts-backdrop')).toBeNull();
  expect(document.getElementById('nflow-shortcuts-help')).toBeNull();
});
//=============================================================================
export {};
//=============================================================================
