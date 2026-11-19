//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment jsdom */
import {
  addDiagnostic,
  clearRunDiagnostics,
  getDiagnostics,
  _resetDiagnosticsForTests,
} from './diagnostics-service';
//=============================================================================
describe('clearRunDiagnostics', () => {
  beforeEach(() => {
    _resetDiagnosticsForTests();
  });

  test('removes the run (simulation / model) diagnostics and keeps the rest', () => {
    addDiagnostic({ severity: 'error', message: 'sim boom', source: 'simulation' });
    addDiagnostic({ severity: 'warning', message: 'model warn', source: 'model' });
    addDiagnostic({ severity: 'warning', message: 'lib', source: 'library' });
    addDiagnostic({ severity: 'info', message: 'app', source: 'application' });
    addDiagnostic({ severity: 'warning', message: 'edit', source: 'editor' });

    clearRunDiagnostics();

    const sources = getDiagnostics()
      .map((d) => d.source)
      .sort();
    expect(sources).toEqual(['application', 'editor', 'library']);
  });

  test('a second run does not keep the first run diagnostics', () => {
    addDiagnostic({ severity: 'error', message: 'run 1 error', source: 'simulation' });
    clearRunDiagnostics(); // start of run 2
    addDiagnostic({ severity: 'warning', message: 'run 2 warning', source: 'simulation' });

    const messages = getDiagnostics().map((d) => d.message);
    expect(messages).toContain('run 2 warning');
    expect(messages).not.toContain('run 1 error');
  });

  test('is a no-op when there are no run diagnostics', () => {
    addDiagnostic({ severity: 'warning', message: 'lib', source: 'library' });
    clearRunDiagnostics();
    expect(getDiagnostics().length).toBe(1);
  });
});
//=============================================================================
