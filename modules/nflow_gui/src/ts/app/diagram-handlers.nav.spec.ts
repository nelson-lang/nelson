//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { setSubsystemUpBtn, updateSubsystemNavUi } from './diagram-handlers.js';
import { state } from './state.js';
//=============================================================================
describe('diagram-handlers updateSubsystemNavUi', () => {
  beforeEach(() => {
    state.subsystemStack = [];
    document.body.classList.remove('is-root-diagram');
  });

  test('hides button and toggles body class when stack is empty', () => {
    const btn = document.createElement('button');
    setSubsystemUpBtn(btn);
    state.subsystemStack = [];
    updateSubsystemNavUi();
    expect(btn.hidden).toBe(true);
    expect(btn.getAttribute('aria-hidden')).toBe('true');
    expect(document.body.classList.contains('is-root-diagram')).toBe(true);
  });

  test('shows button when stack has entries', () => {
    const btn = document.createElement('button');
    setSubsystemUpBtn(btn);
    state.subsystemStack = [{}, {}];
    updateSubsystemNavUi();
    expect(btn.hidden).toBe(false);
    expect(btn.getAttribute('aria-hidden')).toBe('false');
    expect(document.body.classList.contains('is-root-diagram')).toBe(false);
  });
});
//=============================================================================
