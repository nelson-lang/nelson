//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import {
  loadUiPrefs,
  getUiPref,
  setUiPref,
  flushUiPrefs,
  resetUiPrefs,
  _resetUiPrefsForTests,
} from './ui-prefs.js';
//=============================================================================
function installRpcBackend(handler: (method: string, params: Record<string, unknown>) => unknown) {
  (globalThis as any).window.webui = {
    call: async (name: string, payload: string) => {
      if (name !== 'rpc') throw new Error(`unexpected call: ${name}`);
      const envelope = JSON.parse(payload);
      return JSON.stringify({
        id: envelope.id,
        result: await handler(envelope.method, envelope.params || {}),
      });
    },
  };
}
//=============================================================================
beforeEach(() => {
  _resetUiPrefsForTests();
  try {
    localStorage.clear();
    sessionStorage.clear();
  } catch {
    /* ignore */
  }
  delete (globalThis as any).window.webui;
  vi.useRealTimers();
});
//=============================================================================
test('loads preferences from the host file (host-wrapped response)', async () => {
  sessionStorage.setItem('nf-preferences-path', 'C:/prefs');
  const read = vi.fn(() =>
    JSON.stringify({ success: true, content: JSON.stringify({ 'nf-minimap-open': '0' }) })
  );
  installRpcBackend((method, params) => {
    expect(method).toBe('nflow.file.readText');
    return read(params.path as string);
  });
  await loadUiPrefs();
  expect(getUiPref('nf-minimap-open')).toBe('0');
  expect(read).toHaveBeenCalledWith(expect.stringContaining('nflow-ui-prefs.json'));
});
//=============================================================================
test('falls back to localStorage when the host file is unavailable', async () => {
  localStorage.setItem('nf-console-open', '1');
  await loadUiPrefs();
  expect(getUiPref('nf-console-open')).toBe('1');
  expect(getUiPref('unknown-key', 'dflt')).toBe('dflt');
});
//=============================================================================
test('setUiPref mirrors to localStorage and writes the host file (debounced)', async () => {
  vi.useFakeTimers();
  sessionStorage.setItem('nf-preferences-path', 'C:/prefs');
  const write = vi.fn(() => '{}');
  installRpcBackend((method, params) => {
    if (method === 'nflow.file.readText') return JSON.stringify({ success: false });
    expect(method).toBe('nflow.file.writeText');
    return write(params.content as string, params.path as string);
  });
  await loadUiPrefs();

  setUiPref('nf-minimap-open', '0');
  setUiPref('nf-console-open', '1');
  expect(localStorage.getItem('nf-minimap-open')).toBe('0');
  expect(write).not.toHaveBeenCalled();
  await vi.advanceTimersByTimeAsync(400);
  expect(write).toHaveBeenCalledTimes(1);
  const [data, path] = write.mock.calls[0] as unknown as [string, string];
  const parsed = JSON.parse(data);
  expect(parsed._schemaVersion).toBe('2');
  expect(parsed['nf-minimap-open']).toBe('0');
  expect(parsed['nf-console-open']).toBe('1');
  expect(path).toContain('nflow-ui-prefs.json');
  vi.useRealTimers();
});
//=============================================================================
test('migrates legacy sidebar preferences', async () => {
  localStorage.setItem('sidebarWidth', '321px');
  localStorage.setItem('sidebarCollapsed', 'true');
  await loadUiPrefs();
  expect(getUiPref('nf-sidebar-width')).toBe('321px');
  expect(getUiPref('nf-sidebar-open')).toBe('0');
  expect(localStorage.getItem('sidebarWidth')).toBeNull();
  expect(localStorage.getItem('sidebarCollapsed')).toBeNull();
});
//=============================================================================
test('merges a concurrent host preference before writing', async () => {
  sessionStorage.setItem('nf-preferences-path', 'C:/prefs');
  let host = { 'nf-inspector-open': '1' } as Record<string, unknown>;
  installRpcBackend((method, params) => {
    if (method === 'nflow.file.readText') {
      return JSON.stringify({ success: true, content: JSON.stringify(host) });
    }
    host = JSON.parse(params.content as string) as Record<string, unknown>;
    return JSON.stringify({ success: true });
  });
  await loadUiPrefs();
  host['nf-inspector-open'] = '0';
  host['nf-console-open'] = '1';
  setUiPref('nf-minimap-open', '0');
  await flushUiPrefs();
  expect(host['nf-console-open']).toBe('1');
  expect(host['nf-inspector-open']).toBe('0');
  expect(host['nf-minimap-open']).toBe('0');
});
//=============================================================================
test('reset removes managed preferences without clearing recent files', async () => {
  localStorage.setItem('nf-console-open', '1');
  localStorage.setItem('nf-recent-files-v1', '["keep.nflow"]');
  setUiPref('nf-inspector-open', '1');
  await resetUiPrefs();
  expect(localStorage.getItem('nf-console-open')).toBeNull();
  expect(localStorage.getItem('nf-inspector-open')).toBeNull();
  expect(localStorage.getItem('nf-recent-files-v1')).toBe('["keep.nflow"]');
});
//=============================================================================
test('cache wins over stale localStorage after load', async () => {
  sessionStorage.setItem('nf-preferences-path', 'C:/prefs');
  localStorage.setItem('nf-minimap-open', '1');
  installRpcBackend((method) => {
    expect(method).toBe('nflow.file.readText');
    return JSON.stringify({ success: true, content: JSON.stringify({ 'nf-minimap-open': '0' }) });
  });
  await loadUiPrefs();
  expect(getUiPref('nf-minimap-open')).toBe('0');
});
//=============================================================================
