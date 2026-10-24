//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { DiagnosticTabBadge } from './console-host.js';
import {
  _resetDiagnosticsForTests,
  addDiagnostic,
  clearDiagnostics,
} from '../app/diagnostics-service.js';
//=============================================================================
// The badge reads the same diagnostics service as the panel, so it never shows
// a stale count (e.g. "1" left over from a failed Run after the panel emptied).
test('DiagnosticTabBadge reflects errors+warnings live and returns to 0 on clear', async () => {
  _resetDiagnosticsForTests();
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);

  await act(async () => {
    root.render(<DiagnosticTabBadge />);
  });
  const badge = () => host.querySelector('#diagnosticTabCount');
  expect(badge()?.textContent).toBe('0');
  expect(badge()?.className).not.toContain('active');

  await act(async () => {
    addDiagnostic({ severity: 'error', message: 'Boom.', source: 'simulation' });
  });
  expect(badge()?.textContent).toBe('1');
  expect(badge()?.className).toContain('active');

  await act(async () => {
    addDiagnostic({ severity: 'warning', message: 'Careful.', source: 'model' });
  });
  expect(badge()?.textContent).toBe('2');

  // Info does not count toward the badge (matches the panel's error+warning tally).
  await act(async () => {
    addDiagnostic({ severity: 'info', message: 'FYI.', source: 'simulation' });
  });
  expect(badge()?.textContent).toBe('2');

  await act(async () => {
    clearDiagnostics();
  });
  expect(badge()?.textContent).toBe('0');
  expect(badge()?.className).not.toContain('active');

  await act(async () => {
    root.unmount();
  });
  host.remove();
});
//=============================================================================
