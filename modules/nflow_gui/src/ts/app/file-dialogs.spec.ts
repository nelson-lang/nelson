//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, expect, test } from 'vitest';
import {
  chooseDirectoryViaDialog,
  openFilePicker,
  openViaDialog,
  saveViaDialog,
  type FilePickerRequestDetail,
} from './file-dialogs';
import { clearHostModeCacheForTests } from './host-mode';
import { FILE_PICKER_EVENT } from './components/modal-events';
//=============================================================================
const g = globalThis as any;
afterEach(() => {
  delete g.window.webui;
  delete g.window.nflow_rpc;
  delete g.window.rpc;
  clearHostModeCacheForTests();
});
//=============================================================================
function installRpc(
  handler: (method: string, params: Record<string, unknown>) => unknown,
  nativePath: string | null,
  nativeUnavailableReason = '',
  hostMode: 'webview' | 'web' = 'webview'
): void {
  g.window = g.window || {};
  g.window.webui = {
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
            nativeUnavailableReason
              ? { available: false, reason: nativeUnavailableReason }
              : nativePath === null
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
function installHtmlPicker(path: string | null): () => void {
  const handler = (event: Event): void => {
    const detail = (event as CustomEvent).detail as { resolve?: (path: string | null) => void };
    event.preventDefault();
    detail.resolve?.(path);
  };
  document.addEventListener(FILE_PICKER_EVENT, handler);
  return () => document.removeEventListener(FILE_PICKER_EVENT, handler);
}
//=============================================================================
test('openFilePicker falls back to HTML when the native dialog backend is unavailable', async () => {
  installRpc(
    () => {
      throw new Error('unexpected file RPC');
    },
    null,
    'qt_bridge_unavailable'
  );
  const cleanup = installHtmlPicker('/html/fallback.nflow');
  try {
    const path = await openFilePicker({ mode: 'open' });
    expect(path).toBe('/html/fallback.nflow');
  } finally {
    cleanup();
  }
});
//=============================================================================
test('openFilePicker resolves with the path the native dialog provides', async () => {
  installRpc(() => {
    throw new Error('unexpected file RPC');
  }, '/tmp/chosen.nflow');
  const path = await openFilePicker({ mode: 'open' });
  expect(path).toBe('/tmp/chosen.nflow');
});
//=============================================================================
test('openFilePicker uses the remembered directory in the HTML picker', async () => {
  const calls: string[] = [];
  installRpc(
    (method) => {
      calls.push(method);
      if (method === 'nflow.fs.dialogStart') {
        return JSON.stringify({ directory: '/remembered/open' });
      }
      throw new Error(`unexpected file RPC: ${method}`);
    },
    '/native/ignored.nflow',
    '',
    'web'
  );
  let initialDirectory = '';
  const handler = (event: Event): void => {
    const detail = (event as CustomEvent).detail as FilePickerRequestDetail;
    event.preventDefault();
    initialDirectory = detail.defaultPath || '';
    detail.resolve('/html/chosen.nflow');
  };
  document.addEventListener(FILE_PICKER_EVENT, handler);
  try {
    const path = await openFilePicker({ mode: 'open' });
    expect(path).toBe('/html/chosen.nflow');
    expect(initialDirectory).toBe('/remembered/open');
    expect(calls).toContain('nflow.fs.dialogStart');
  } finally {
    document.removeEventListener(FILE_PICKER_EVENT, handler);
  }
});
//=============================================================================
test('saveViaDialog writes the picked path and reports the split path', async () => {
  let seen: Record<string, unknown> | null = null;
  installRpc((method, params) => {
    expect(method).toBe('nflow.file.writeText');
    seen = params;
    return JSON.stringify({ success: true });
  }, '/data/out/model.nflow');
  const res = await saveViaDialog({
    typeOfFile: 'nflow',
    content: 'BODY',
    defaultName: 'model.nflow',
  });
  expect(res.success).toBe(true);
  expect(res.filePath).toBe('/data/out/model.nflow');
  expect(res.fileName).toBe('model.nflow');
  expect(seen).toEqual({ path: '/data/out/model.nflow', content: 'BODY' });
});
//=============================================================================
test('saveViaDialog uses the HTML picker in web mode', async () => {
  let seen: Record<string, unknown> | null = null;
  installRpc(
    (method, params) => {
      expect(method).toBe('nflow.file.writeText');
      seen = params;
      return JSON.stringify({ success: true });
    },
    '/native/ignored.nflow',
    '',
    'web'
  );
  const cleanup = installHtmlPicker('/html/out/model.nflow');
  try {
    const res = await saveViaDialog({
      typeOfFile: 'nflow',
      content: 'BODY',
      defaultName: 'model.nflow',
    });
    expect(res.success).toBe(true);
    expect(res.filePath).toBe('/html/out/model.nflow');
    expect(seen).toEqual({ path: '/html/out/model.nflow', content: 'BODY' });
  } finally {
    cleanup();
  }
});
//=============================================================================
test('saveViaDialog reports cancel when the picker is dismissed', async () => {
  installRpc(() => {
    throw new Error('unexpected file RPC');
  }, null);
  const res = await saveViaDialog({ typeOfFile: 'svg', content: 'X', defaultName: 'a.svg' });
  expect(res.success).toBe(false);
  expect(res.canceled).toBe(true);
});
//=============================================================================
test('openViaDialog reads the picked file through nflow.file.readText', async () => {
  installRpc((method, params) => {
    expect(method).toBe('nflow.file.readText');
    expect(params.path).toBe('/data/in/model.nflow');
    return JSON.stringify({ success: true, content: 'LOADED' });
  }, '/data/in/model.nflow');
  const res = await openViaDialog({ extension: '.nflow' });
  expect(res.success).toBe(true);
  expect(res.content).toBe('LOADED');
  expect(res.fileName).toBe('model.nflow');
});
//=============================================================================
test('openViaDialog uses the HTML picker in web mode', async () => {
  installRpc(
    (method, params) => {
      expect(method).toBe('nflow.file.readText');
      expect(params.path).toBe('/html/in/model.nflow');
      return JSON.stringify({ success: true, content: 'LOADED' });
    },
    '/native/ignored.nflow',
    '',
    'web'
  );
  const cleanup = installHtmlPicker('/html/in/model.nflow');
  try {
    const res = await openViaDialog({ extension: '.nflow' });
    expect(res.success).toBe(true);
    expect(res.content).toBe('LOADED');
    expect(res.fileName).toBe('model.nflow');
  } finally {
    cleanup();
  }
});
//=============================================================================
test('chooseDirectoryViaDialog returns the picked directory', async () => {
  installRpc(() => {
    throw new Error('unexpected file RPC');
  }, '/data/build');
  const res = await chooseDirectoryViaDialog();
  expect(res.success).toBe(true);
  expect(res.path).toBe('/data/build');
});
//=============================================================================
test('chooseDirectoryViaDialog uses the HTML picker in web mode', async () => {
  installRpc(
    () => {
      throw new Error('unexpected file RPC');
    },
    '/native/ignored',
    '',
    'web'
  );
  const cleanup = installHtmlPicker('/html/build');
  try {
    const res = await chooseDirectoryViaDialog();
    expect(res.success).toBe(true);
    expect(res.path).toBe('/html/build');
  } finally {
    cleanup();
  }
});
//=============================================================================
test('saveViaDialog uses the system dialog when available (no HTML picker)', async () => {
  const calls: string[] = [];
  g.window = g.window || {};
  g.window.webui = {
    call: async (name: string, payload: string) => {
      expect(name === 'nflow_rpc' || name === 'rpc').toBe(true);
      const envelope = JSON.parse(payload);
      calls.push(envelope.method);
      if (envelope.method === 'nflow.host.mode') {
        return JSON.stringify({
          id: envelope.id,
          result: { mode: 'webview' },
        });
      }
      if (envelope.method === 'nflow.fs.nativeDialog') {
        return JSON.stringify({
          id: envelope.id,
          result: JSON.stringify({ available: true, path: '/native/pick.nflow' }),
        });
      }
      if (envelope.method === 'nflow.file.writeText') {
        return JSON.stringify({ id: envelope.id, result: JSON.stringify({ success: true }) });
      }
      throw new Error(`unexpected ${envelope.method}`);
    },
  };
  const res = await saveViaDialog({ typeOfFile: 'nflow', content: 'B', defaultName: 'x.nflow' });
  expect(res.success).toBe(true);
  expect(res.filePath).toBe('/native/pick.nflow');
  expect(calls).toContain('nflow.fs.nativeDialog');
  expect(calls).toContain('nflow.file.writeText');
});
//=============================================================================
test('system dialog cancel returns without writing', async () => {
  const calls: string[] = [];
  g.window = g.window || {};
  g.window.webui = {
    call: async (name: string, payload: string) => {
      expect(name === 'nflow_rpc' || name === 'rpc').toBe(true);
      const envelope = JSON.parse(payload);
      calls.push(envelope.method);
      if (envelope.method === 'nflow.host.mode') {
        return JSON.stringify({
          id: envelope.id,
          result: { mode: 'webview' },
        });
      }
      if (envelope.method === 'nflow.fs.nativeDialog') {
        return JSON.stringify({
          id: envelope.id,
          result: JSON.stringify({ available: true, canceled: true }),
        });
      }
      throw new Error(`unexpected ${envelope.method}`);
    },
  };
  const res = await saveViaDialog({ typeOfFile: 'nflow', content: 'B', defaultName: 'x.nflow' });
  expect(res.success).toBe(false);
  expect(calls).not.toContain('nflow.file.writeText');
});
//=============================================================================
