//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Simple unit tests for frontend i18n runtime
//=============================================================================
import { vi } from 'vitest';
//=============================================================================
let i18n: any;
beforeEach(async () => {
  vi.resetAllMocks();
  document.body.innerHTML = '';
  i18n = await import('../../../dist/utils/i18n.js');
});
//=============================================================================
test('initI18n loads translations and t() interpolation', async () => {
  const translations = {
    Hello: 'Hello!',
    'Interpolate {name}': 'Hello, {name}!',
    'DOM Text': 'DOM Text',
    'Title Text': 'Title Text',
    Placeholder: 'Enter value',
  };
  (global as any).fetch = vi.fn(async () => ({ ok: true, json: async () => translations }));

  await i18n.initI18n('en_US');
  expect(i18n.getLocale()).toBe('en_US');
  expect(i18n.t('Hello')).toBe('Hello!');
  expect(i18n.t('Interpolate {name}', { name: 'Alice' })).toBe('Hello, Alice!');
  expect(i18n.t('Missing')).toBe('Missing');
});
//=============================================================================
test('localizeDOM applies translations to elements', async () => {
  const translations = {
    'DOM Text': 'DOM Text',
    'Title Text': 'Title Text',
    Placeholder: 'Enter value',
  };
  (global as any).fetch = vi.fn(async () => ({ ok: true, json: async () => translations }));
  await i18n.initI18n('en_US');

  const root = document.createElement('div');
  root.innerHTML = `
    <span data-i18n="DOM Text"></span>
    <button data-i18n-title="Title Text"></button>
    <input data-i18n-placeholder="Placeholder" />
  `;
  i18n.localizeDOM(root);
  expect(root.querySelector('span')!.textContent).toBe('DOM Text');
  expect((root.querySelector('button') as HTMLButtonElement).title).toBe('Title Text');
  expect((root.querySelector('input') as HTMLInputElement).placeholder).toBe('Enter value');
});
//=============================================================================
test('localizeDOM appends data-shortcut to the localized tooltip', async () => {
  const translations = { 'Reset View': 'Reset View' };
  (global as any).fetch = vi.fn(async () => ({ ok: true, json: async () => translations }));
  await i18n.initI18n('en_US');

  const root = document.createElement('div');
  root.innerHTML = `
    <button id="a" data-i18n-title="Reset View" data-shortcut="Ctrl+0"></button>
    <button id="b" data-i18n-title="Reset View"></button>
  `;
  i18n.localizeDOM(root);
  expect((root.querySelector('#a') as HTMLButtonElement).title).toBe('Reset View (Ctrl+0)');
  // No data-shortcut -> plain translation, unchanged.
  expect((root.querySelector('#b') as HTMLButtonElement).title).toBe('Reset View');
});
//=============================================================================
export {};
//=============================================================================
