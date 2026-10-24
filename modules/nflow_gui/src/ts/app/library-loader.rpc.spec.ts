//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, expect, test, vi } from 'vitest';
//=============================================================================
const mocks = vi.hoisted(() => ({
  rpc: vi.fn(),
  canUse: vi.fn(() => true),
}));
//=============================================================================
vi.mock('./native-rpc.js', () => ({
  canUseNFlowRpc: mocks.canUse,
  nflowRpc: mocks.rpc,
}));
//=============================================================================
vi.mock('./host-utils.js', () => ({ getPreferencesPath: vi.fn(async () => 'C:\\prefs') }));
import {
  applySearchPaths,
  getDiscoveredManifests,
  getLoadedLibraries,
  getSearchPaths,
  initLibraries,
  persistLibraryPreferences,
  setSearchPaths,
} from './library-loader.js';
//=============================================================================
function configResponse(config: Record<string, unknown>): string {
  return JSON.stringify({ success: true, content: JSON.stringify(config) });
}
//=============================================================================
beforeEach(() => {
  mocks.rpc.mockReset();
  mocks.canUse.mockReset();
  mocks.canUse.mockReturnValue(true);
  getDiscoveredManifests().clear();
  getLoadedLibraries().clear();
  setSearchPaths([]);
  vi.stubGlobal(
    'fetch',
    vi.fn(async () => ({ ok: false }))
  );
});
//=============================================================================
test('setSearchPaths is followed by getSearchPaths and uses the read-back order', async () => {
  mocks.rpc.mockImplementation(async (method: string) => {
    if (method === 'nflow.library.setSearchPaths') {
      return {
        success: true,
        paths: ['C:\\untrusted'],
        manifests: [],
        locations: [
          { path: 'C:\\normalized-b', available: true, libraryCount: 2 },
          { path: 'C:\\normalized-a', available: false, libraryCount: 0 },
        ],
      };
    }
    if (method === 'nflow.library.getSearchPaths') {
      return ['C:\\normalized-a', 'C:\\normalized-b'];
    }
    if (method === 'nflow.file.readText') return configResponse({ enabledLibraries: [] });
    if (method === 'nflow.file.writeText') return JSON.stringify({ success: true });
    return [];
  });

  const result = await applySearchPaths(['C:\\requested']);

  expect(result.paths).toEqual(['C:\\normalized-a', 'C:\\normalized-b']);
  expect(result.locations).toEqual([
    { path: 'C:\\normalized-a', available: false, libraryCount: 0 },
    { path: 'C:\\normalized-b', available: true, libraryCount: 2 },
  ]);
  const methods = mocks.rpc.mock.calls.map(([method]) => method);
  expect(methods.indexOf('nflow.library.setSearchPaths')).toBeLessThan(
    methods.indexOf('nflow.library.getSearchPaths')
  );
});
//=============================================================================
test('a failed setSearchPaths response is rejected without reading paths back', async () => {
  mocks.rpc.mockResolvedValue({ success: false, message: 'Scan failed.' });

  await expect(applySearchPaths(['C:\\requested'])).rejects.toThrow('Scan failed.');
  expect(mocks.rpc).toHaveBeenCalledTimes(1);
  expect(mocks.rpc).toHaveBeenCalledWith('nflow.library.setSearchPaths', {
    paths: ['C:\\requested'],
  });
});
//=============================================================================
test('a configuration without searchPaths uses the native default without migration', async () => {
  mocks.rpc.mockImplementation(async (method: string) => {
    if (method === 'nflow.file.readText') return configResponse({ enabledLibraries: [] });
    if (method === 'nflow.library.getSearchPaths') return ['C:\\native-default'];
    if (method === 'nflow.library.list') return [];
    if (method === 'nflow.file.writeText') return JSON.stringify({ success: true });
    return [];
  });

  await initLibraries();

  expect(getSearchPaths()).toEqual(['C:\\native-default']);
  expect(mocks.rpc).not.toHaveBeenCalledWith('nflow.library.setSearchPaths', expect.anything());
});
//=============================================================================
test('startup restores configured paths before enabling recorded external libraries', async () => {
  const manifest = {
    id: 'custom.restored',
    title: 'Restored',
    _dir: 'C:\\libraries\\restored',
    blocks: [{ type: 'restoredBlock' }],
  };
  mocks.rpc.mockImplementation(async (method: string) => {
    if (method === 'nflow.file.readText') {
      return configResponse({
        enabledLibraries: ['custom.restored'],
        searchPaths: ['C:\\libraries'],
      });
    }
    if (method === 'nflow.library.setSearchPaths') {
      return {
        success: true,
        paths: ['C:\\libraries'],
        manifests: [manifest],
        locations: [{ path: 'C:\\libraries', available: true, libraryCount: 1 }],
        diagnostics: [],
      };
    }
    if (method === 'nflow.library.getSearchPaths') return ['C:\\libraries'];
    if (method === 'nflow.library.list') return [manifest];
    if (method === 'nflow.file.writeText') return JSON.stringify({ success: true });
    return [];
  });

  await initLibraries();

  expect(getSearchPaths()).toEqual(['C:\\libraries']);
  expect(getLoadedLibraries().has('custom.restored')).toBe(true);
  const methods = mocks.rpc.mock.calls.map(([method]) => method);
  expect(methods.indexOf('nflow.library.setSearchPaths')).toBeLessThan(
    methods.indexOf('nflow.library.list')
  );
});
//=============================================================================
test('persistence stores search paths and external enabled identifiers only', async () => {
  setSearchPaths(['C:\\libraries']);
  getLoadedLibraries().set('builtin.source', {
    id: 'builtin.source',
    manifest: {},
    library: {},
  });
  getLoadedLibraries().set('custom.enabled', {
    id: 'custom.enabled',
    manifest: {},
    library: {},
  });
  mocks.rpc.mockResolvedValue(JSON.stringify({ success: true }));

  await persistLibraryPreferences();

  const write = mocks.rpc.mock.calls.find(([method]) => method === 'nflow.file.writeText');
  expect(write).toBeTruthy();
  expect(JSON.parse(write?.[1].content)).toEqual({
    enabledLibraries: ['custom.enabled'],
    searchPaths: ['C:\\libraries'],
  });
});
//=============================================================================
test('persistence rejects an unsuccessful write response', async () => {
  mocks.rpc.mockResolvedValue(JSON.stringify({ success: false, error: 'Cannot write file' }));

  await expect(persistLibraryPreferences()).rejects.toThrow('Cannot write file');
});
//=============================================================================
