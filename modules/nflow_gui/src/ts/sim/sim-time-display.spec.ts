//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Simulation time/progress feed: partial results dispatch nf-sim-time and
// the simulation command bar renders it.
//=============================================================================
import { processPartialScopes } from './parse-results.js';
import { _resetForTests } from '../app/status-service.js';
//=============================================================================
beforeEach(() => {
  _resetForTests();
  document.body.innerHTML = `
    <input id="runtimeInput" value="5" />
    <div id="floatingToolbar" style="display:none"></div>
    <svg><g id="blockLayer"></g></svg>`;
});
//=============================================================================
test('processPartialScopes dispatches nf-sim-time with time and progress', () => {
  const seen: any[] = [];
  const onTime = (e: Event) => seen.push((e as CustomEvent).detail);
  document.addEventListener('nf-sim-time', onTime);
  processPartialScopes({ progress: 0.42, currentTime: 1.25, scopes: [] }, new Map(), null, null);
  document.removeEventListener('nf-sim-time', onTime);
  expect(seen.length).toBe(1);
  expect(seen[0].time).toBeCloseTo(1.25);
  expect(seen[0].progress).toBe(42);
  expect(seen[0].done).toBe(false);
});
//=============================================================================
test('simulation command bar renders current time, target, and progress', async () => {
  await import('../app/floating-toolbar.js');
  const init = (globalThis as any).initFloatingToolbar;
  expect(typeof init).toBe('function');
  init();
  const display = document.getElementById('simTimeDisplay');
  expect(display).not.toBeNull();
  document.dispatchEvent(
    new CustomEvent('nf-sim-time', { detail: { time: 2.5, progress: 50, done: false } })
  );
  expect(display?.textContent).toContain('2.50 / 5.00 s');
  expect(document.querySelector('.simulation-progress-track')?.getAttribute('aria-valuenow')).toBe(
    '50'
  );
  document.dispatchEvent(
    new CustomEvent('nf-sim-time', { detail: { time: 5, progress: 100, done: true } })
  );
  expect(display?.textContent).toContain('5.00 / 5.00 s');
});
//=============================================================================
