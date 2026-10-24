//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
  _resetPersistentStorageForTests,
  flushPersistentStorage,
  getPersistentItem,
  initializePersistentStorage,
  resetInterfacePreferences,
  setPersistentItem,
} from '../../src/desktop/persistentStorage';
//=============================================================================
describe('host-backed desktop preferences', () => {
  const calls: { method: string; params: Record<string, unknown> }[] = [];

  beforeEach(() => {
    vi.useFakeTimers();
    localStorage.clear();
    calls.length = 0;
    _resetPersistentStorageForTests();
    window.webui = {
      call: vi.fn(async (_name: string, payload: string) => {
        const request = JSON.parse(payload) as {
          id: number;
          method: string;
          params: Record<string, unknown>;
        };
        calls.push({ method: request.method, params: request.params });
        const result =
          request.method === 'prefs.load'
            ? {
                schema: 1,
                values: { 'nelson-webview.shell.visibility': '{"toolbar":false}' },
                path: 'C:/prefs/web-gui-ui-prefs.json',
              }
            : { ok: true };
        return JSON.stringify({ id: request.id, result });
      }),
    };
  });

  afterEach(() => {
    delete window.webui;
    vi.useRealTimers();
  });

  it('merges host values over legacy local preferences and migrates both', async () => {
    localStorage.setItem('nelson-webview.theme', 'dark');
    await initializePersistentStorage();

    expect(getPersistentItem('nelson-webview.theme')).toBe('dark');
    expect(getPersistentItem('nelson-webview.shell.visibility')).toBe('{"toolbar":false}');
    await flushPersistentStorage();
    expect(calls.map((call) => call.method)).toEqual(['prefs.load', 'prefs.save']);
    expect(calls[1].params.values).toMatchObject({
      'nelson-webview.theme': 'dark',
      'nelson-webview.shell.visibility': '{"toolbar":false}',
    });
  });

  it('mirrors changes locally and writes the complete versioned snapshot', async () => {
    await initializePersistentStorage();
    setPersistentItem('nelson-webview.editor.tabSize', '2');
    await flushPersistentStorage();

    expect(localStorage.getItem('nelson-webview.editor.tabSize')).toBe('2');
    expect(calls.at(-1)?.params).toMatchObject({
      schema: 1,
      values: { 'nelson-webview.editor.tabSize': '2' },
    });
  });

  it('resets interface state without clearing recent files', async () => {
    await initializePersistentStorage();
    setPersistentItem('nelson-webview.theme', 'dark');
    setPersistentItem('nelson-webview.recentFiles', '["demo.m"]');
    await resetInterfacePreferences();

    expect(getPersistentItem('nelson-webview.theme')).toBeNull();
    expect(getPersistentItem('nelson-webview.recentFiles')).toBe('["demo.m"]');
  });
});
//=============================================================================
