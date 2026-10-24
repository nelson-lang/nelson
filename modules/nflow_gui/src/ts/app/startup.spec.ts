//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Startup hardening: failures must surface in the loading overlay instead of
// being silently swallowed.
//=============================================================================
import { vi } from 'vitest';
import { reportStartupError } from './app.js';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = `
    <div class="nf-loading-overlay">
      <div class="nf-loading-spinner"></div>
      <div id="nf-loading-status">Starting…</div>
    </div>`;
  document.documentElement.classList.remove('nf-startup-failed');
});
//=============================================================================
test('reportStartupError writes the failure into the loading overlay', () => {
  const spy = vi.spyOn(console, 'error').mockImplementation(() => {});
  reportStartupError(new Error('boom during init'));
  spy.mockRestore();
  const status = document.getElementById('nf-loading-status')!;
  expect(status.textContent).toBe('Startup failed: boom during init');
  expect(status.classList.contains('nf-loading-error')).toBe(true);
  expect(document.documentElement.classList.contains('nf-startup-failed')).toBe(true);
  const spinner = document.querySelector('.nf-loading-spinner') as HTMLElement;
  expect(spinner.style.display).toBe('none');
});
//=============================================================================
test('reportStartupError always logs to the console', () => {
  const spy = vi.spyOn(console, 'error').mockImplementation(() => {});
  reportStartupError('plain failure');
  expect(spy).toHaveBeenCalledWith('[nflow] startup failed:', 'plain failure');
  spy.mockRestore();
});
//=============================================================================
