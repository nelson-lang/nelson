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
function hexToRgbString(hex: string): string {
  const h = hex.replace('#', '');
  const full =
    h.length === 3
      ? h
          .split('')
          .map((c) => c + c)
          .join('')
      : h;
  const n = parseInt(full, 16);
  const r = (n >> 16) & 255;
  const g = (n >> 8) & 255;
  const b = n & 255;
  return `rgb(${r}, ${g}, ${b})`;
}
//=============================================================================
beforeEach(() => {
  // Ensure a fresh module environment so importing head-init runs side-effects each test
  vi.resetModules();
  document.documentElement.innerHTML = '';
  document.body.innerHTML = '';
  document.documentElement.className = '';
  document.documentElement.removeAttribute('data-theme');
  document.documentElement.style.background = '';
  document.documentElement.style.color = '';
  window.history.replaceState({}, '', '/');
  localStorage.clear();
  sessionStorage.clear();
});
//=============================================================================
test('head-init applies the dark URL override', async () => {
  window.history.replaceState({}, '', '/?dark=1');

  await importDistHeadInit();

  expect(document.documentElement.getAttribute('data-theme')).toBe('dark');
  expect(localStorage.getItem('nf-theme-dark')).toBe('1');
});
//=============================================================================
test('head-init respects baked dark data-theme', async () => {
  document.documentElement.setAttribute('data-theme', 'dark');
  document.body.innerHTML = `
    <div class="nf-loading-overlay" aria-hidden="true">
      <div class="nf-loading-box">
        <div class="nf-loading-spinner" role="img" aria-label="Loading"></div>
        <div class="nf-loading-text">Loading…</div>
        <div id="nf-loading-status" class="nf-loading-step" aria-live="polite">Starting…</div>
      </div>
    </div>
  `;

  await importDistHeadInit();

  expect(document.documentElement.getAttribute('data-theme')).toBe('dark');
  const expectedBg = hexToRgbString('#1e1e1e');
  const expectedColor = hexToRgbString('#cccccc');
  const actualBg = document.documentElement.style.background;
  const actualColor = document.documentElement.style.color;
  expect(actualBg === '#1e1e1e' || actualBg === expectedBg).toBe(true);
  expect(actualColor === '#cccccc' || actualColor === expectedColor).toBe(true);

  const overlay = document.querySelector('.nf-loading-overlay') as HTMLElement | null;
  expect(overlay).not.toBeNull();
  const overlayBg = overlay!.style.background;
  expect(overlayBg === '#1e1e1e' || overlayBg === expectedBg).toBe(true);

  const spinner = overlay!.querySelector('.nf-loading-spinner') as HTMLElement | null;
  expect(spinner).not.toBeNull();
  const spinnerTop = spinner!.style.borderTopColor.replace(/\s+/g, '');
  expect(
    spinnerTop === 'rgba(255,255,255,0.9)' ||
      spinnerTop === 'rgba(255, 255, 255, 0.9)'.replace(/\s+/g, '')
  ).toBe(true);
});
//=============================================================================
test('head-init respects baked light data-theme', async () => {
  document.documentElement.setAttribute('data-theme', 'light');
  document.body.innerHTML = `
    <div class="nf-loading-overlay" aria-hidden="true">
      <div class="nf-loading-box">
        <div class="nf-loading-spinner" role="img" aria-label="Loading"></div>
        <div class="nf-loading-text">Loading…</div>
        <div id="nf-loading-status" class="nf-loading-step" aria-live="polite">Starting…</div>
      </div>
    </div>
  `;

  await importDistHeadInit();

  const expectedBg = hexToRgbString('#ffffff');
  const expectedColor = hexToRgbString('#333333');
  const actualBg = document.documentElement.style.background;
  const actualColor = document.documentElement.style.color;
  expect(actualBg === '#ffffff' || actualBg === expectedBg).toBe(true);
  expect(actualColor === '#333333' || actualColor === expectedColor).toBe(true);

  const overlay = document.querySelector('.nf-loading-overlay') as HTMLElement | null;
  expect(overlay).not.toBeNull();
  const overlayBg = overlay!.style.background;
  expect(overlayBg === '#fff' || overlayBg === '#ffffff' || overlayBg === expectedBg).toBe(true);

  const spinner = overlay!.querySelector('.nf-loading-spinner') as HTMLElement | null;
  expect(spinner).not.toBeNull();
  const spinnerTop = spinner!.style.borderTopColor.replace(/\s+/g, '');
  expect(
    spinnerTop === 'rgba(0,0,0,0.9)' || spinnerTop === 'rgba(0, 0, 0, 0.9)'.replace(/\s+/g, '')
  ).toBe(true);
});
//=============================================================================
export {};
//=============================================================================
