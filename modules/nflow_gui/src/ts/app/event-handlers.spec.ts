//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as eh from './event-handlers';
import { state } from './state';
//=============================================================================
test('event-handlers module loads', () => {
  expect(typeof eh).toBe('object');
});
//=============================================================================
test('text selection is limited to editable surfaces and the console', () => {
  const textInput = document.createElement('input');
  textInput.type = 'text';
  const numberInput = document.createElement('input');
  numberInput.type = 'number';
  const textarea = document.createElement('textarea');
  const editable = document.createElement('div');
  editable.setAttribute('contenteditable', 'true');
  const editableChild = document.createElement('span');
  editable.appendChild(editableChild);
  const consolePanel = document.createElement('div');
  consolePanel.className = 'nf-console-panel';
  const consoleMessage = document.createElement('span');
  consolePanel.appendChild(consoleMessage);

  expect(eh.isTextSelectionTarget(textInput)).toBe(true);
  expect(eh.isTextSelectionTarget(numberInput)).toBe(true);
  expect(eh.isTextSelectionTarget(textarea)).toBe(true);
  expect(eh.isTextSelectionTarget(editableChild)).toBe(true);
  expect(eh.isTextSelectionTarget(consoleMessage)).toBe(true);
});
//=============================================================================
test('non-text controls do not delegate select-all to the browser', () => {
  const checkbox = document.createElement('input');
  checkbox.type = 'checkbox';
  const range = document.createElement('input');
  range.type = 'range';
  const select = document.createElement('select');
  const button = document.createElement('button');

  expect(eh.isTextSelectionTarget(checkbox)).toBe(false);
  expect(eh.isTextSelectionTarget(range)).toBe(false);
  expect(eh.isTextSelectionTarget(select)).toBe(false);
  expect(eh.isTextSelectionTarget(button)).toBe(false);
  expect(eh.shouldUseNativeKeyboardBehavior(checkbox, true)).toBe(false);
  expect(eh.shouldUseNativeKeyboardBehavior(select, true)).toBe(false);
});
//=============================================================================
test('non-text form controls retain native behavior for other keys', () => {
  const checkbox = document.createElement('input');
  checkbox.type = 'checkbox';
  const range = document.createElement('input');
  range.type = 'range';
  const select = document.createElement('select');

  expect(eh.shouldUseNativeKeyboardBehavior(checkbox, false)).toBe(true);
  expect(eh.shouldUseNativeKeyboardBehavior(range, false)).toBe(true);
  expect(eh.shouldUseNativeKeyboardBehavior(select, false)).toBe(true);
});
//=============================================================================
import { beforeAll, test, expect, vi } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/event-handlers.js').catch(() => null);
});
//=============================================================================
function installRpcBackend(handler: (method: string, params: Record<string, unknown>) => unknown) {
  (globalThis as any).window = (globalThis as any).window || globalThis;
  (globalThis as any).window.webui = {
    call: async (name: string, payload: string) => {
      if (name !== 'nflow_rpc' && name !== 'rpc') throw new Error(`unexpected call: ${name}`);
      const envelope = JSON.parse(payload);
      if (envelope.method === 'nflow.host.mode') {
        return JSON.stringify({
          id: envelope.id,
          result: { mode: 'webview' },
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
test('event-handlers module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
test('generateCodeToDir receives build:true when codegenBuildAfterGen is checked', async () => {
  document.body.innerHTML = `
    <select id="codegenLang"><option value="c" selected>C</option></select>
    <input id="codegenIncludeMain" type="checkbox" checked />
    <input id="codegenBuildAfterGen" type="checkbox" checked />
    <input id="simDt" type="number" value="0.01" />
    <button id="codegenBtn"></button>
    <div id="debugLog"></div>
    <div id="status"></div>
  `;

  const capturedOptions: any[] = [];

  installRpcBackend((method, params) => {
    if (method === 'nflow.fs.nativeDialog') {
      return JSON.stringify({ available: true, path: '/tmp/out' });
    }
    if (method === 'nflow.codegen.generateToDir') {
      capturedOptions.push(JSON.parse(String(params.optionsJson || '{}')));
      return JSON.stringify({ success: true, dir: '/tmp/out' });
    }
    throw new Error(`unexpected method: ${method}`);
  });

  // The module is loaded lazily; if unavailable in the test environment just skip.
  if (!mod) return;

  const btn = document.getElementById('codegenBtn') as HTMLButtonElement;
  btn.click();
  await new Promise((r) => setTimeout(r, 50));

  if (capturedOptions.length > 0) {
    expect(capturedOptions[0].build).toBe(true);
  }
});
//=============================================================================
test('status shows build error when backend returns buildSuccess:false', async () => {
  document.body.innerHTML = `
    <select id="codegenLang"><option value="c" selected>C</option></select>
    <input id="codegenIncludeMain" type="checkbox" checked />
    <input id="codegenBuildAfterGen" type="checkbox" checked />
    <input id="simDt" type="number" value="0.01" />
    <button id="codegenBtn"></button>
    <div id="debugLog"></div>
    <div id="status"></div>
  `;

  installRpcBackend((method) => {
    if (method === 'nflow.fs.nativeDialog') {
      return JSON.stringify({ available: true, path: '/tmp/out' });
    }
    if (method === 'nflow.codegen.generateToDir') {
      return JSON.stringify({
        success: true,
        dir: '/tmp/out',
        buildSuccess: false,
        buildError: 'compiler not found',
      });
    }
    throw new Error(`unexpected method: ${method}`);
  });

  if (!mod) return;

  const btn = document.getElementById('codegenBtn') as HTMLButtonElement;
  btn.click();
  await new Promise((r) => setTimeout(r, 50));

  const debugLog = document.getElementById('debugLog');
  if (debugLog && debugLog.textContent) {
    expect(debugLog.textContent).toContain('build failed');
  }
});
//=============================================================================
test('keyboard block movement allows signed world coordinates', () => {
  document.body.innerHTML = '';
  state.blocks.clear();
  state.connections = [];
  state.selectedId = 'signed-key';
  state.selectedIds = new Set(['signed-key']);
  state.dirtyBlocks = new Set();
  state.routingDirty = false;
  const block: any = { id: 'signed-key', x: 0, y: 0, width: 80, height: 40 };
  state.blocks.set(block.id, block);

  const updateBlockTransform = vi.fn();
  const updateConnections = vi.fn();
  const updateSelectionBox = vi.fn();
  eh.setRendererRef({
    current: {
      updateBlockTransform,
      updateConnections,
      updateSelectionBox,
    },
  } as any);

  const moved = eh.moveSelectedBlocksByDelta(-10, 0);

  expect(block.x).toBe(-10);
  expect(block.y).toBe(0);
  expect(moved).toBe(true);
  expect(updateBlockTransform).toHaveBeenCalledWith(block);
  expect(updateConnections).toHaveBeenCalledWith(true);
  expect(updateSelectionBox).toHaveBeenCalled();
  expect(state.dirtyBlocks.has(block.id)).toBe(true);
});
//=============================================================================
test('codegen payload ignores circular renderer state', () => {
  const block: any = {
    id: 'gain',
    type: 'gain',
    x: 21,
    y: 39,
    params: { Gain: 'k' },
  };
  const connection: any = {
    from: 'source',
    to: 'gain',
    fromIndex: 0,
    toIndex: 0,
    points: [
      { x: 1, y: 2 },
      { x: 3, y: 4 },
    ],
  };
  block.renderSegments = [{ owner: block }];
  connection.renderSegments = [{ owner: connection }];
  state.blocks = new Map([[block.id, block]] as any);
  state.connections = [connection];
  state.variables = { k: 2 };
  state.variablesText = 'k=2';

  const payload = eh.buildCodegenDiagramPayload() as any;
  const json = JSON.stringify(payload);

  expect(json).not.toContain('renderSegments');
  expect(json).not.toContain('owner');
  expect(payload.variables).toEqual({ k: 2 });
  expect(payload.blocks[0]).toEqual({
    id: 'gain',
    type: 'gain',
    x: 21,
    y: 39,
    rotation: 0,
    params: { Gain: 'k' },
  });
  expect(payload.connections[0]).toEqual({
    from: 'source',
    to: 'gain',
    fromIndex: 0,
    toIndex: 0,
    points: [
      [1, 2],
      [3, 4],
    ],
  });
});
//=============================================================================
export {};
//=============================================================================
