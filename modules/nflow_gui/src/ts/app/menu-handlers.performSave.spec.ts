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
import { performOpen, performSave, performSaveAs, setStatusEl } from './menu-handlers.js';
import { setCurrentFilePath, state } from './state.js';
import { clearHostModeCacheForTests } from './host-mode.js';
//=============================================================================
function installRpcBackend(
  handler: (method: string, params: Record<string, unknown>) => unknown,
  nativePath: string | null = '/tmp/native.nflow',
  hostMode: 'webview' | 'web' = 'webview'
) {
  (window as any).webui = {
    call: async (name: string, payload: string) => {
      if (name !== 'nflow_rpc' && name !== 'rpc') throw new Error(`unexpected call: ${name}`);
      const envelope = JSON.parse(payload);
      if (envelope.method === 'nflow.host.mode') {
        return JSON.stringify({
          id: envelope.id,
          result: { mode: hostMode },
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
  beforeEach(() => {
    document.body.innerHTML = '';
    delete (window as any).webui;
    delete (window as any).nflow_rpc;
    delete (window as any).rpc;
    delete (window as any).showSaveFilePicker;
    clearHostModeCacheForTests();
  });

  test('performSave uses nflow.file.writeText and updates status', async () => {
    const status = document.createElement('div');
    setStatusEl(status);
    setCurrentFilePath('/tmp/test.nflow');
    const backend = vi.fn(() => JSON.stringify({ success: true }));
    installRpcBackend((method, params) => {
      expect(method).toBe('nflow.file.writeText');
      expect(params.path).toBe('/tmp/test.nflow');
      return backend(method, params);
    });

    const res = await performSave('yaml-content', 'default.nflow');
    expect(res).toEqual({ success: true });
    expect(status.textContent).toMatch(/Saved/);
    expect(backend).toHaveBeenCalled();
  });

  test('performSaveAs picks a destination then writes via nflow.file.writeText', async () => {
    const status = document.createElement('div');
    setStatusEl(status);
    installRpcBackend((method, params) => {
      expect(method).toBe('nflow.file.writeText');
      expect(params.path).toBe('/tmp/default.nflow');
      expect(params.content).toBe('yaml-content');
      return JSON.stringify({ success: true });
    }, '/tmp/default.nflow');

    const res = await performSaveAs('yaml-content', 'default.nflow');
    expect(res.success).toBe(true);
    expect(status.textContent).toMatch(/Saved/);
  });

  test('uses browser upload and download when no filesystem host is connected', async () => {
    const input = document.createElement('input');
    input.id = 'fileOpenInput';
    const open = vi.spyOn(input, 'click');
    document.body.appendChild(input);

    await performOpen();
    expect(open).toHaveBeenCalledOnce();

    const createObjectURL = vi.fn(() => 'blob:nflow-test');
    const revokeObjectURL = vi.fn();
    Object.defineProperties(URL, {
      createObjectURL: { configurable: true, value: createObjectURL },
      revokeObjectURL: { configurable: true, value: revokeObjectURL },
    });
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => {});

    const result = await performSaveAs('{"name":"browser"}', 'browser.nflow');

    expect(result).toMatchObject({
      success: true,
      fileName: 'browser.nflow',
      browserDownload: true,
    });
    expect(createObjectURL).toHaveBeenCalledOnce();
    expect(click).toHaveBeenCalledOnce();
    expect(revokeObjectURL).toHaveBeenCalledWith('blob:nflow-test');
  });

  test('uses browser upload for Open in WebAssembly web mode', async () => {
    const input = document.createElement('input');
    input.id = 'fileOpenInput';
    const open = vi.spyOn(input, 'click');
    document.body.appendChild(input);

    installRpcBackend(
      (method) => {
        if (method === 'nflow.file.readText') throw new Error('should not read a virtual path');
        return JSON.stringify({ success: true });
      },
      '/native/ignored.nflow',
      'web'
    );

    await performOpen();

    expect(open).toHaveBeenCalledOnce();
  });

  test('uses browser save picker for Save As in WebAssembly web mode', async () => {
    const write = vi.fn(async () => undefined);
    const close = vi.fn(async () => undefined);
    (window as any).showSaveFilePicker = vi.fn(async () => ({
      name: 'picked.nflow',
      createWritable: async () => ({ write, close }),
    }));
    installRpcBackend(
      (method) => {
        if (method === 'nflow.file.writeText') throw new Error('should not write a virtual path');
        return JSON.stringify({ success: true });
      },
      '/native/ignored.nflow',
      'web'
    );

    const result = await performSaveAs('{"name":"browser"}', 'browser.nflow');

    expect(result).toMatchObject({
      success: true,
      fileName: 'picked.nflow',
      browserFilePicker: true,
    });
    expect((window as any).showSaveFilePicker).toHaveBeenCalledOnce();
    expect(write).toHaveBeenCalledOnce();
    expect(close).toHaveBeenCalledOnce();
  });

  test('commits last-save metadata only after a successful write', async () => {
    setCurrentFilePath('/tmp/metadata.nflow');
    state.lastSavedBy = 'Previous';
    state.lastSavedAt = '2025-01-01T00:00:00.000Z';
    const content = JSON.stringify({
      version: '1.0.0',
      metadata: {
        created: '2024-01-01T00:00:00.000Z',
        lastSavedBy: 'Carol',
        lastSavedAt: '2026-08-08T18:35:29.000Z',
      },
    });
    installRpcBackend(() => JSON.stringify({ success: true }));

    await performSave(content, 'metadata.nflow');

    expect(state.lastSavedBy).toBe('Carol');
    expect(state.lastSavedAt).toBe('2026-08-08T18:35:29.000Z');
  });

  test('keeps previous last-save metadata when a write fails', async () => {
    setCurrentFilePath('/tmp/metadata.nflow');
    state.lastSavedBy = 'Previous';
    state.lastSavedAt = '2025-01-01T00:00:00.000Z';
    const content = JSON.stringify({
      metadata: {
        lastSavedBy: 'Carol',
        lastSavedAt: '2026-08-08T18:35:29.000Z',
      },
    });
    installRpcBackend(() => JSON.stringify({ success: false, error: 'disk error' }));

    await performSave(content, 'metadata.nflow');

    expect(state.lastSavedBy).toBe('Previous');
    expect(state.lastSavedAt).toBe('2025-01-01T00:00:00.000Z');
  });
});
//=============================================================================
