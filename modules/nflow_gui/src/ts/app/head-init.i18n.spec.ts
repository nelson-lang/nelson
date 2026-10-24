//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, test, expect, vi } from 'vitest';
//=============================================================================
const importDistHeadInit = async () => {
  await import('../../../dist/app/head-init.js').catch(() => null);
};
//=============================================================================
beforeEach(() => {
  vi.resetModules();
  document.documentElement.innerHTML = '';
  document.body.innerHTML = '';
  document.documentElement.className = '';
  document.documentElement.removeAttribute('data-theme');
  document.documentElement.style.background = '';
  document.documentElement.style.color = '';
  try {
    localStorage.clear();
  } catch (_e) {}
  try {
    sessionStorage.clear();
  } catch (_e) {}
});
//=============================================================================
test('head-init fetches translations when only nf-i18n-locale is seeded', async () => {
  try {
    localStorage.setItem('nf-i18n-locale', 'fr_FR');
  } catch (_e) {}

  document.body.innerHTML = `
    <div class="nf-loading-overlay" aria-hidden="true">
      <div class="nf-loading-box">
        <div class="nf-loading-text" data-i18n="Loading…">Loading…</div>
        <div id="nf-loading-status" class="nf-loading-step" aria-live="polite" data-i18n="Starting…">Starting…</div>
      </div>
    </div>
  `;

  const translations = { 'Loading…': 'Loaded!', 'Starting…': 'Started!' };
  (global as any).fetch = vi.fn(async () => ({ ok: true, json: async () => translations }));

  await importDistHeadInit();

  for (let i = 0; i < 40; i++) {
    if ((window as any).__nf_i18n_loaded) break;
    // eslint-disable-next-line no-await-in-loop
    await new Promise((r) => setTimeout(r, 25));
  }

  expect(document.querySelector('.nf-loading-text')!.textContent).toBe('Loaded!');
  expect((document.getElementById('nf-loading-status') as HTMLElement).textContent).toBe(
    'Started!'
  );
  expect(localStorage.getItem('nf-i18n-translations')).not.toBeNull();
});
//=============================================================================
export {};
//=============================================================================
