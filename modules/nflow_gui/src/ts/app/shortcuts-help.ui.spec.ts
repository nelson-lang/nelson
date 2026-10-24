//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { showShortcutsHelp, hideShortcutsHelp } from './shortcuts-help.js';
//=============================================================================
describe('shortcuts-help UI', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    // ensure platform non-mac for predictable text
    Object.defineProperty(navigator, 'platform', { value: 'Win32', configurable: true });
  });

  test('showShortcutsHelp creates backdrop and modal, hide removes them', () => {
    showShortcutsHelp();
    const backdrop = document.getElementById('nflow-shortcuts-backdrop');
    const modal = document.getElementById('nflow-shortcuts-help');
    expect(backdrop).toBeTruthy();
    expect(modal).toBeTruthy();
    expect(modal!.textContent).toContain('Keyboard Shortcuts');

    hideShortcutsHelp();
    expect(document.getElementById('nflow-shortcuts-backdrop')).toBeNull();
    expect(document.getElementById('nflow-shortcuts-help')).toBeNull();
  });
});
//=============================================================================
