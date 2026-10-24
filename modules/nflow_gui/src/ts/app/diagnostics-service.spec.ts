//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  _resetDiagnosticsForTests,
  addDiagnostic,
  clearDiagnostics,
  clearDocumentDiagnostics,
  getDiagnostics,
  replaceDiagnostics,
  subscribeDiagnostics,
} from './diagnostics-service.js';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = `
    <button class="diagnostic-counter"><span id="diagnosticErrorCount"></span></button>
    <button class="diagnostic-counter"><span id="diagnosticWarningCount"></span></button>
    <span id="diagnosticTabCount"></span>`;
  _resetDiagnosticsForTests();
});
//=============================================================================
test('adds, deduplicates, and counts diagnostics', () => {
  const first = addDiagnostic({
    severity: 'error',
    message: 'Invalid gain.',
    source: 'model',
    blockId: 'gain-1',
  });
  const duplicate = addDiagnostic({
    severity: 'error',
    message: 'Invalid gain.',
    source: 'model',
    blockId: 'gain-1',
  });

  expect(duplicate).toBe(first);
  expect(getDiagnostics()).toHaveLength(1);
  expect(document.getElementById('diagnosticErrorCount')?.textContent).toBe('1');
  // The bottom-panel tab badge (#diagnosticTabCount) is no longer written here:
  // it is owned by the React <DiagnosticTabBadge/>, which subscribes to this
  // service so it can never go stale relative to the panel.
});
//=============================================================================
test('preserves a related help topic', () => {
  addDiagnostic({
    severity: 'warning',
    message: 'Library path is unavailable.',
    source: 'library',
    helpTopic: 'nflow_workspace',
  });

  expect(getDiagnostics()[0]?.helpTopic).toBe('nflow_workspace');
});
//=============================================================================
test('replaces and clears the diagnostic list', () => {
  const snapshots: number[] = [];
  const unsubscribe = subscribeDiagnostics((items) => snapshots.push(items.length));
  replaceDiagnostics([
    { severity: 'warning', message: 'Inherited sample time.', source: 'simulation' },
    { severity: 'info', message: '  Solver initialized.  ', source: 'simulation' },
    { severity: 'error', message: 'Expression failed.', source: 'model', blockId: '' },
  ]);
  expect(getDiagnostics()).toHaveLength(3);
  expect(getDiagnostics().map((item) => item.severity)).toEqual(['error', 'warning', 'info']);
  expect(getDiagnostics()[2]?.message).toBe('Solver initialized.');
  expect(getDiagnostics()[0]?.blockId).toBeUndefined();
  expect(document.getElementById('diagnosticWarningCount')?.textContent).toBe('1');
  clearDiagnostics();
  expect(getDiagnostics()).toHaveLength(0);
  expect(snapshots).toEqual([0, 3, 0]);
  unsubscribe();
});
//=============================================================================
test('a document switch drops the model diagnostics and keeps library warnings', () => {
  const snapshots: number[] = [];
  const unsubscribe = subscribeDiagnostics((items) => snapshots.push(items.length));
  addDiagnostic({
    severity: 'warning',
    message: 'Library path is unavailable.',
    source: 'library',
  });
  addDiagnostic({
    severity: 'error',
    message: 'Invalid gain.',
    source: 'model',
    blockId: 'gain-1',
  });
  addDiagnostic({ severity: 'error', message: 'Output to output.', source: 'editor' });
  addDiagnostic({ severity: 'warning', message: 'Step too large.', source: 'simulation' });
  addDiagnostic({ severity: 'error', message: 'Save failed.', source: 'application' });
  expect(getDiagnostics()).toHaveLength(5);

  clearDocumentDiagnostics();

  expect(getDiagnostics().map((item) => item.source)).toEqual(['library']);
  expect(document.getElementById('diagnosticErrorCount')?.textContent).toBe('0');
  expect(document.getElementById('diagnosticWarningCount')?.textContent).toBe('1');
  // Nothing document-bound left: a second switch is a no-op and notifies nobody.
  clearDocumentDiagnostics();
  expect(snapshots).toEqual([0, 1, 2, 3, 4, 5, 1]);
  unsubscribe();
});
//=============================================================================
test('the status-bar counters carry a count-aware tooltip', () => {
  const errorCounter = document
    .getElementById('diagnosticErrorCount')
    ?.closest('.diagnostic-counter');
  const warningCounter = document
    .getElementById('diagnosticWarningCount')
    ?.closest('.diagnostic-counter');

  // Empty: the tooltip is the action hint, not a "0, 0" tally.
  expect(errorCounter?.getAttribute('title')).toBe('Open diagnostics');

  addDiagnostic({ severity: 'error', message: 'Boom.', source: 'model' });
  addDiagnostic({ severity: 'error', message: 'Bang.', source: 'model' });
  addDiagnostic({ severity: 'warning', message: 'Careful.', source: 'simulation' });

  expect(errorCounter?.getAttribute('title')).toBe('Errors: 2, Warnings: 1');
  expect(warningCounter?.getAttribute('title')).toBe('Errors: 2, Warnings: 1');
});
//=============================================================================
