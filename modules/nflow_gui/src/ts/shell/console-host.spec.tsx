//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { mountConsolePanel, openBottomPanelTab, toggleConsolePanel } from './console-host.js';
import { vi } from 'vitest';
import { _resetForTests } from '../app/status-service.js';
import { _resetDiagnosticsForTests, addDiagnostic } from '../app/diagnostics-service.js';
import { _resetUiPrefsForTests } from '../app/ui-prefs.js';
//=============================================================================
beforeEach(() => {
  _resetForTests();
  _resetDiagnosticsForTests();
  _resetUiPrefsForTests();
  document.body.innerHTML = '<div class="main-editor"><div class="editor-content"></div></div>';
  try {
    localStorage.clear();
  } catch {
    /* ignore */
  }
});
//=============================================================================
test('mountConsolePanel adds a collapsed bottom panel under the editor', () => {
  mountConsolePanel();
  const panel = document.getElementById('bottomPanel');
  expect(panel).not.toBeNull();
  expect(panel?.classList.contains('collapsed')).toBe(true);
  expect(panel?.dataset.reactRoot).toBe('bottom-console');
  expect(document.getElementById('consolePanelHost')).not.toBeNull();
  expect(document.querySelector('[data-testid="console-panel"]')).not.toBeNull();
  expect(panel?.querySelector('.bottom-panel-title')?.textContent).toBe('Console');
  expect(document.getElementById('diagnosticsPanelHost')).not.toBeNull();
});
//=============================================================================
test('mountConsolePanel is idempotent', () => {
  mountConsolePanel();
  mountConsolePanel();
  expect(document.querySelectorAll('#bottomPanel').length).toBe(1);
});
//=============================================================================
test('toggleConsolePanel expands and collapses, persisting the state', () => {
  mountConsolePanel();
  const panel = document.getElementById('bottomPanel')!;
  toggleConsolePanel();
  expect(panel.classList.contains('collapsed')).toBe(false);
  expect(localStorage.getItem('nf-console-open')).toBe('1');
  toggleConsolePanel();
  expect(panel.classList.contains('collapsed')).toBe(true);
  expect(localStorage.getItem('nf-console-open')).toBe('0');
});
//=============================================================================
test('open state is restored from localStorage on mount', () => {
  localStorage.setItem('nf-console-open', '1');
  mountConsolePanel();
  const panel = document.getElementById('bottomPanel')!;
  expect(panel.classList.contains('collapsed')).toBe(false);
});
//=============================================================================
test('opens diagnostics tab and navigates to a related block', async () => {
  mountConsolePanel();
  addDiagnostic({
    severity: 'error',
    message: 'Invalid gain.',
    source: 'model',
    blockId: 'gain-1',
  });
  let selected = '';
  document.addEventListener('nf-select-block', (event) => {
    selected = String((event as CustomEvent).detail?.blockId || '');
  });
  await vi.waitFor(() => {
    expect(document.querySelector('.nf-diagnostic-row')).not.toBeNull();
  });
  openBottomPanelTab('diagnostics');
  const row = document.querySelector<HTMLButtonElement>('.nf-diagnostic-main');
  expect(row?.textContent).toContain('Invalid gain.');
  row?.click();
  expect(selected).toBe('gain-1');
  expect(localStorage.getItem('nf-bottom-panel-tab')).toBe('diagnostics');
  expect(document.querySelector<HTMLButtonElement>('.nf-diagnostic-help')?.title).toBe(
    'Open related help'
  );
});
//=============================================================================
test('shows the diagnostics empty state and disables clear', () => {
  mountConsolePanel();
  openBottomPanelTab('diagnostics');

  expect(document.querySelector('.nf-diagnostics-empty')?.textContent).toContain('No diagnostics.');
  expect(document.querySelectorAll('.nf-diagnostics-filter')).toHaveLength(3);
  expect(document.querySelector<HTMLButtonElement>('.nf-diagnostics-clear')?.disabled).toBe(true);
});
//=============================================================================
test('filters diagnostics by severity and clears the list', async () => {
  mountConsolePanel();
  addDiagnostic({ severity: 'error', message: 'Invalid gain.', source: 'model' });
  addDiagnostic({ severity: 'warning', message: 'Inherited sample time.', source: 'simulation' });
  addDiagnostic({ severity: 'info', message: 'Solver initialized.', source: 'simulation' });

  await vi.waitFor(() => {
    expect(document.querySelectorAll('.nf-diagnostic-row')).toHaveLength(3);
  });

  const warningFilter = document.querySelector<HTMLButtonElement>(
    '.nf-diagnostics-filter.nf-diagnostic-warning'
  );
  expect(warningFilter?.textContent).toContain('1');
  warningFilter?.click();

  await vi.waitFor(() => {
    expect(document.querySelector('.nf-diagnostics-list')?.textContent).not.toContain(
      'Inherited sample time.'
    );
  });
  expect(warningFilter?.getAttribute('aria-pressed')).toBe('false');
  expect(JSON.parse(localStorage.getItem('nf-diagnostics-filters') || '{}').warning).toBe(false);

  const clearButton = document.querySelector<HTMLButtonElement>('.nf-diagnostics-clear');
  expect(clearButton?.disabled).toBe(false);
  clearButton?.click();

  await vi.waitFor(() => {
    expect(document.querySelector('.nf-diagnostics-empty')?.textContent).toContain(
      'No diagnostics.'
    );
  });
  expect(clearButton?.disabled).toBe(true);
});
//=============================================================================
test('restores persisted diagnostic filters', () => {
  localStorage.setItem(
    'nf-diagnostics-filters',
    JSON.stringify({ error: true, warning: false, info: true })
  );
  mountConsolePanel();
  const warningFilter = document.querySelector<HTMLButtonElement>(
    '.nf-diagnostics-filter.nf-diagnostic-warning'
  );
  expect(warningFilter?.getAttribute('aria-pressed')).toBe('false');
});
//=============================================================================
