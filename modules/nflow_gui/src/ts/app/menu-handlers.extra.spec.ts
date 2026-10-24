//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { performSave, performSaveAs, setStatusEl } from '../../../dist/app/menu-handlers.js';
import { setCurrentFilePath, getCurrentFilePath } from '../../../dist/app/state.js';
//=============================================================================
function installRpcBackend(
  handler: (method: string, params: Record<string, unknown>) => unknown,
  nativePath: string | null = '/tmp/native.nflow'
) {
  (window as any).webui = {
    call: async (name: string, payload: string) => {
      if (name !== 'nflow_rpc' && name !== 'rpc') throw new Error(`unexpected call: ${name}`);
      const envelope = JSON.parse(payload);
      if (envelope.method === 'nflow.host.mode') {
        return JSON.stringify({
          id: envelope.id,
          result: { mode: 'webview' },
        });
      }
      if (envelope.method === 'nflow.fs.nativeDialog') {
        return JSON.stringify({
          id: envelope.id,
          result: JSON.stringify(
            nativePath === null
              ? { available: true, canceled: true }
              : { available: true, path: nativePath }
          ),
        });
      }
      return JSON.stringify({
        id: envelope.id,
        result: await handler(envelope.method, envelope.params || {}),
      });
    },
  };
}
//=============================================================================
describe('menu-handlers save operations', () => {
  let statusEl: HTMLElement;
  beforeEach(() => {
    statusEl = document.createElement('div');
    setStatusEl(statusEl);
    setCurrentFilePath(null);
    delete (window as any).webui;
    delete (window as any).nflow_rpc;
    delete (window as any).rpc;
  });

  test('performSave delegates to nflow.file.writeText when path set', async () => {
    setCurrentFilePath('my.nflow');
    const backend = vi.fn(() => JSON.stringify({ success: true }));
    installRpcBackend((method, params) => {
      expect(method).toBe('nflow.file.writeText');
      expect(params.path).toBe('my.nflow');
      return backend(method, params);
    });
    const res = await performSave('x:1', 'default');
    expect(res.success).toBe(true);
    expect(statusEl.textContent).toMatch(/Saved/);
    expect(backend).toHaveBeenCalled();
  });

  test('performSaveAs picks a destination then writes and updates current path', async () => {
    installRpcBackend((method) => {
      expect(method).toBe('nflow.file.writeText');
      return JSON.stringify({ success: true });
    }, 'saved.nflow');
    const res = await performSaveAs('x:1', 'def');
    expect(res.success).toBe(true);
    expect(getCurrentFilePath()).toBe('saved.nflow');
  });
});
//=============================================================================
