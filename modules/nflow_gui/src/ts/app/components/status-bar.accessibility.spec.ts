//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Accessibility tests for status-bar component
//=============================================================================
describe('mountStatusBar - accessibility', () => {
  let mountStatusBar: () => void;

  beforeAll(async () => {
    const mod = await import('./status-bar.tsx');
    mountStatusBar = mod.mountStatusBar;
  });

  beforeEach(() => {
    document.body.innerHTML = '';
    const container = document.createElement('div');
    container.className = 'status-bar';
    document.body.appendChild(container);
    mountStatusBar();
  });

  test('statusBarInfo has role="status"', () => {
    const el = document.getElementById('statusBarInfo');
    expect(el?.getAttribute('role')).toBe('status');
  });

  test('statusBarInfo has aria-live="polite"', () => {
    const el = document.getElementById('statusBarInfo');
    expect(el?.getAttribute('aria-live')).toBe('polite');
  });

  test('statusBarInfo has aria-atomic="true"', () => {
    const el = document.getElementById('statusBarInfo');
    expect(el?.getAttribute('aria-atomic')).toBe('true');
  });

  test('statusBarProgress is present in the DOM', () => {
    const el = document.getElementById('statusBarProgress');
    expect(el).toBeTruthy();
  });

  test('statusBarProgress has role="progressbar"', () => {
    const el = document.getElementById('statusBarProgress');
    expect(el?.getAttribute('role')).toBe('progressbar');
  });

  test('statusBarProgress has aria-valuemin="0"', () => {
    const el = document.getElementById('statusBarProgress');
    expect(el?.getAttribute('aria-valuemin')).toBe('0');
  });

  test('statusBarProgress has aria-valuemax="100"', () => {
    const el = document.getElementById('statusBarProgress');
    expect(el?.getAttribute('aria-valuemax')).toBe('100');
  });

  test('statusBarProgress is hidden by default', () => {
    const el = document.getElementById('statusBarProgress');
    expect(el?.hasAttribute('hidden')).toBe(true);
  });

  test('status bar does not duplicate diagram view controls', () => {
    const buttons = document.querySelectorAll('#gridSettingsBtn');
    expect(buttons.length).toBe(0);
  });

  test('lockIndicator is aria-hidden (decorative)', () => {
    const el = document.getElementById('lockIndicator');
    expect(el?.getAttribute('aria-hidden')).toBe('true');
  });
});
//=============================================================================
