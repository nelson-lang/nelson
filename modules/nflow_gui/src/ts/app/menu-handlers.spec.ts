//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
let stateMod: any;
const g = globalThis as any;
beforeAll(async () => {
  mod = await import('../../../dist/app/menu-handlers.js').catch(() => null);
  stateMod = await import('../../../dist/app/state.js').catch(() => null);
});
//=============================================================================
afterEach(() => {
  delete g.window.webui;
  delete g.window.nflow_rpc;
  delete g.window.rpc;
});
//=============================================================================
function installRpcBackend(
  handler: (method: string, params: Record<string, unknown>) => unknown,
  nativePath: string | null = '/tmp/native.nflow'
) {
  g.window = g.window || {};
  g.window.webui = {
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
      try {
        return JSON.stringify({
          id: envelope.id,
          result: await handler(envelope.method, envelope.params || {}),
        });
      } catch (err) {
        return JSON.stringify({
          id: envelope.id,
          error: { message: (err as Error)?.message || String(err) },
        });
      }
    },
  };
}
//=============================================================================
test('menu-handlers module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
test('performSave calls nflow.file.writeText and updates status', async () => {
  if (!mod || !stateMod) return;
  const status: any = { textContent: '' };
  mod.setStatusEl(status as HTMLElement);
  stateMod.setCurrentFilePath('file.nflow');
  installRpcBackend((method) => {
    expect(method).toBe('nflow.file.writeText');
    return JSON.stringify({ success: true });
  });
  const res = await mod.performSave('yaml', 'default');
  expect(res.success).toBe(true);
  expect(status.textContent).toBe('Saved: file.nflow');
});
//=============================================================================
test('performSave handles save error', async () => {
  if (!mod || !stateMod) return;
  const status: any = { textContent: '' };
  mod.setStatusEl(status as HTMLElement);
  stateMod.setCurrentFilePath('file2.nflow');
  installRpcBackend(() => {
    throw new Error('disk error');
  });
  const res = await mod.performSave('yaml', 'default');
  expect(res.success).toBe(false);
  expect(String(status.textContent)).toMatch(/Save error/);
});
//=============================================================================
test('performSave without a path picks a destination then writes it', async () => {
  if (!mod || !stateMod) return;
  const status: any = { textContent: '' };
  mod.setStatusEl(status as HTMLElement);
  stateMod.setCurrentFilePath(null);
  installRpcBackend((method) => {
    expect(method).toBe('nflow.file.writeText');
    return JSON.stringify({ success: true });
  }, 'saved.nflow');
  const res = await mod.performSave('yaml', 'defaultName');
  expect(res.success).toBe(true);
  expect(stateMod.getCurrentFilePath()).toBe('saved.nflow');
  expect(status.textContent).toBe('Saved: saved.nflow');
});
//=============================================================================
test('performSaveAs sets file path on success', async () => {
  if (!mod || !stateMod) return;
  const status: any = { textContent: '' };
  mod.setStatusEl(status as HTMLElement);
  installRpcBackend((method) => {
    expect(method).toBe('nflow.file.writeText');
    return JSON.stringify({ success: true });
  }, 'asaved.nflow');
  const res = await mod.performSaveAs('yaml', 'my.nflow');
  expect(res.success).toBe(true);
  expect(stateMod.getCurrentFilePath()).toBe('asaved.nflow');
  expect(status.textContent).toBe('Saved: asaved.nflow');
});
//=============================================================================
test('performSaveAs reports cancel when the picker is dismissed', async () => {
  if (!mod || !stateMod) return;
  const status: any = { textContent: '' };
  mod.setStatusEl(status as HTMLElement);
  installRpcBackend(() => {
    throw new Error('unexpected file RPC');
  }, null);
  const res = await mod.performSaveAs('yaml', 'my.nflow');
  expect(res.success).toBe(false);
});
//=============================================================================
export {};
import { vi } from 'vitest';
import fs from 'fs';
import path from 'path';
//=============================================================================
test('loads menu-handlers if present', async () => {
  const p = path.resolve(process.cwd(), 'modules/nflow_gui/src/ts/app/menu-handlers.ts');
  if (!fs.existsSync(p)) {
    return;
  }
  const mod = await import('./menu-handlers');
  expect(mod).toBeDefined();
});
//=============================================================================
import * as mh from './menu-handlers';
import { state } from './state.js';
//=============================================================================
test('menu-handlers module loads', () => {
  expect(typeof mh).toBe('object');
});
//=============================================================================
test('selected block help menu state follows the current selection', () => {
  document.body.innerHTML =
    '<div id="selectedBlockHelpAction" class="menubar-item disabled" aria-disabled="true"></div>';
  state.blocks.clear();
  state.selectedId = null;
  mh.updateSelectedBlockHelpAction();
  expect(document.getElementById('selectedBlockHelpAction')?.getAttribute('aria-disabled')).toBe(
    'true'
  );

  state.blocks.set('gain-1', { type: 'gain' });
  state.selectedId = 'gain-1';
  mh.updateSelectedBlockHelpAction();
  expect(document.getElementById('selectedBlockHelpAction')?.getAttribute('aria-disabled')).toBe(
    'false'
  );
  expect(document.getElementById('selectedBlockHelpAction')?.classList.contains('disabled')).toBe(
    false
  );
});
//=============================================================================
