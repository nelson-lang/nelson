//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit tests for host-backed preferences loading in grid-manager
import { vi } from 'vitest';
//=============================================================================
function installRpcBackend(handler: (method: string, params: Record<string, unknown>) => unknown) {
  (global as any).window.webui = {
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
  document.body.innerHTML = '';
  vi.resetModules();
  vi.resetAllMocks();
  delete (global as any).window.webui;
});
//=============================================================================
test('loadGridSettings applies host-wrapped response', async () => {
  vi.resetModules();
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  vi.doMock('./host-utils.js', () => ({
    getPreferencesPath: async () => '/tmp',
  }));

  installRpcBackend((method) => {
    expect(method).toBe('nflow.file.readText');
    return JSON.stringify({
      success: true,
      content: JSON.stringify({ gridVisible: true, gridSize: 7 }),
    });
  });

  const mod = await import('./grid-manager.js');

  const container = document.createElement('div');
  container.id = 'svgCanvas';
  document.body.appendChild(container);

  await mod.initGridManager();

  const s = mod.getGridState();
  expect(s.visible).toBe(true);
  expect(s.size).toBe(7);
});
//=============================================================================
test('loadGridSettings applies plain JSON response', async () => {
  vi.resetModules();
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  vi.doMock('./host-utils.js', () => ({
    getPreferencesPath: async () => '/tmp',
  }));

  installRpcBackend((method) => {
    expect(method).toBe('nflow.file.readText');
    return JSON.stringify({ gridVisible: true, gridSize: 9 });
  });

  const mod = await import('./grid-manager.js');

  const container = document.createElement('div');
  container.id = 'svgCanvas';
  document.body.appendChild(container);

  await mod.initGridManager();

  const s = mod.getGridState();
  expect(s.visible).toBe(true);
  expect(s.size).toBe(9);
});
//=============================================================================
test('grid settings actions use React modal events', async () => {
  vi.doMock('./host-utils.js', () => ({
    getPreferencesPath: async () => '',
  }));

  const mod = await import('./grid-manager.js');

  document.body.innerHTML = `
    <div id="canvas"><div id="svgCanvas"></div></div>
    <button id="gridToggleBtn"></button>
    <button id="gridSettingsBtn"></button>
    <input id="gridSizeInput" value="24" />
    <input id="gridSizeSlider" value="24" />
    <button id="gridSettingsApplyBtn"></button>
  `;

  const showListener = vi.fn();
  const hideListener = vi.fn();
  document.addEventListener('nflow:grid-settings-show', showListener);
  document.addEventListener('nflow:grid-settings-hide', hideListener);

  await mod.initGridManager();

  document.getElementById('gridSettingsBtn')!.click();
  expect(showListener).toHaveBeenCalledTimes(1);

  (document.getElementById('gridSizeInput') as HTMLInputElement).value = '24';
  document.getElementById('gridSettingsApplyBtn')!.click();
  expect(mod.getGridSize()).toBe(24);
  expect(hideListener).toHaveBeenCalledTimes(1);

  document.removeEventListener('nflow:grid-settings-show', showListener);
  document.removeEventListener('nflow:grid-settings-hide', hideListener);
});
//=============================================================================
export {};
//=============================================================================
