//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, describe, expect, test, vi } from 'vitest';
//=============================================================================
const mocks = vi.hoisted(() => ({
  nflowRpc: vi.fn(),
  openFilePicker: vi.fn(),
}));
//=============================================================================
vi.mock('./native-rpc.js', () => ({
  nflowRpc: mocks.nflowRpc,
  canUseNFlowRpc: () => true,
}));
//=============================================================================
vi.mock('./host-utils.js', () => ({
  getPreferencesPath: () => Promise.resolve('C:/preferences'),
}));
//=============================================================================
vi.mock('./file-dialogs.js', () => ({
  openFilePicker: mocks.openFilePicker,
}));
//=============================================================================
import { initModelicaSettings } from './modelica-settings.js';
//=============================================================================
function mountSettings(): void {
  document.body.innerHTML = `
    <input id="modelicaPathInput" />
    <button id="modelicaBrowseBtn">Browse</button>
    <div id="modelicaPathStatus" data-state="checking">
      <span id="modelicaPathStatusLabel"></span>
      <small id="modelicaPathStatusDetails"></small>
    </div>`;
}
//=============================================================================
function installInfo(info: Record<string, unknown>): void {
  mocks.nflowRpc.mockImplementation((method: string) => {
    if (method === 'nflow.file.readText') {
      return Promise.resolve(JSON.stringify({ success: false }));
    }
    if (method === 'nflow.modelica.info') return Promise.resolve(JSON.stringify(info));
    if (method === 'nflow.file.writeText')
      return Promise.resolve(JSON.stringify({ success: true }));
    return Promise.reject(new Error(`unexpected RPC: ${method}`));
  });
}
//=============================================================================
describe('OpenModelica settings detection', () => {
  beforeEach(() => {
    mocks.nflowRpc.mockReset();
    mocks.openFilePicker.mockReset();
    mountSettings();
  });

  test('shows the detected version and executable when FMU export is ready', async () => {
    installInfo({
      available: true,
      capable: true,
      omc: 'C:/OpenModelica/bin/omc.exe',
      version: 'OpenModelica 1.27.0',
      reason: 'ready',
    });

    initModelicaSettings();

    await vi.waitFor(() => {
      expect(document.getElementById('modelicaPathStatus')?.dataset.state).toBe('ready');
    });
    expect(document.getElementById('modelicaPathStatusLabel')?.textContent).toBe(
      'Detected and ready'
    );
    expect(document.getElementById('modelicaPathStatusDetails')?.textContent).toContain(
      'OpenModelica 1.27.0'
    );
    expect(document.getElementById('modelicaPathStatusDetails')?.textContent).toContain('omc.exe');
  });

  test('distinguishes an incomplete installation from a missing one', async () => {
    installInfo({
      available: true,
      capable: false,
      omc: 'C:/OpenModelica/bin/omc.exe',
      version: 'OpenModelica 1.27.0',
      reason: 'FMI export runtime is missing.',
    });
    initModelicaSettings();

    await vi.waitFor(() => {
      expect(document.getElementById('modelicaPathStatus')?.dataset.state).toBe('warning');
    });
    expect(document.getElementById('modelicaPathStatusDetails')?.textContent).toContain(
      'FMI export runtime is missing.'
    );

    mountSettings();
    installInfo({ available: false, capable: false, reason: 'omc was not found.' });
    initModelicaSettings();

    await vi.waitFor(() => {
      expect(document.getElementById('modelicaPathStatus')?.dataset.state).toBe('missing');
    });
    expect(document.getElementById('modelicaPathStatusLabel')?.textContent).toBe('Not detected');
  });

  test('checks the resolved installation again after saving an override', async () => {
    installInfo({ available: true, capable: true, version: 'OpenModelica 1.27.0' });
    initModelicaSettings();
    await vi.waitFor(() => {
      expect(document.getElementById('modelicaPathStatus')?.dataset.state).toBe('ready');
    });

    const input = document.getElementById('modelicaPathInput') as HTMLInputElement;
    input.value = 'C:/OpenModelica';
    input.dispatchEvent(new Event('change'));

    await vi.waitFor(() => {
      expect(mocks.nflowRpc).toHaveBeenCalledWith('nflow.file.writeText', {
        path: 'C:/preferences/nflow_modelica.json',
        content: JSON.stringify({ omcPath: 'C:/OpenModelica' }),
      });
      expect(
        mocks.nflowRpc.mock.calls.filter(([method]) => method === 'nflow.modelica.info')
      ).toHaveLength(2);
    });
  });
});
//=============================================================================
