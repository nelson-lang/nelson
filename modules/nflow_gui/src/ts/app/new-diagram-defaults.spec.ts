//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { bindNewDiagramDefaults, readNewDiagramDefaults } from './new-diagram-defaults.js';
import { _resetUiPrefsForTests } from './ui-prefs.js';
//=============================================================================
beforeEach(() => {
  _resetUiPrefsForTests();
  localStorage.clear();
  document.body.innerHTML = '<input id="runtimeInput" value="10">';
});
//=============================================================================
test('persists valid values used by future new diagrams', () => {
  bindNewDiagramDefaults();
  const runtime = document.getElementById('runtimeInput') as HTMLInputElement;
  runtime.value = '25';
  runtime.dispatchEvent(new Event('change'));
  expect(readNewDiagramDefaults().runtime).toBe(25);
});
//=============================================================================
test('rejects invalid persisted numerical defaults', () => {
  localStorage.setItem('nf-new-diagram-runtime', '-4');
  localStorage.setItem('nf-new-diagram-sample-time', 'invalid');
  expect(readNewDiagramDefaults().runtime).toBe(10);
  expect(readNewDiagramDefaults().sampleTime).toBe(0.01);
});
//=============================================================================
