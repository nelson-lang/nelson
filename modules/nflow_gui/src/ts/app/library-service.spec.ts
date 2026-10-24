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
import { state } from './state.js';
//=============================================================================
const mocks = vi.hoisted(() => ({
  manifests: new Map<string, Record<string, unknown>>(),
  loaded: new Map<string, unknown>(),
  paths: ['C:\\libraries-a', 'C:\\libraries-b'],
  applySearchPaths: vi.fn(),
  persist: vi.fn(),
  unload: vi.fn(),
  rpc: vi.fn(),
  diagnostic: vi.fn(),
  status: vi.fn(),
}));
//=============================================================================
vi.mock('./library-loader.js', () => ({
  applySearchPaths: mocks.applySearchPaths,
  buildBlockLibrary: vi.fn(() => []),
  enableLibraryFromManifest: vi.fn(),
  getDiscoveredManifests: () => mocks.manifests,
  getLoadedLibraries: () => mocks.loaded,
  getSearchPaths: () => mocks.paths.slice(),
  loadLibrary: vi.fn(async () => ({})),
  persistLibraryPreferences: mocks.persist,
  unloadLibrary: mocks.unload,
}));
//=============================================================================
vi.mock('./native-rpc.js', () => ({ canUseNFlowRpc: vi.fn(() => true), nflowRpc: mocks.rpc }));
vi.mock('./block-library-handlers.js', () => ({
  setBlockLibrary: vi.fn(),
  renderBlockLibrary: vi.fn(),
}));
//=============================================================================
vi.mock('../render/render.js', () => ({ refreshBlockTemplates: vi.fn() }));
vi.mock('./diagnostics-service.js', () => ({ addDiagnostic: mocks.diagnostic }));
vi.mock('./status-service.js', () => ({ pushStatus: mocks.status }));
import {
  addLibraryLocation,
  getLibraryServiceSnapshot,
  moveLibraryLocation,
  removeLibraryLocation,
  replaceLibraryLocation,
  rescanLibraries,
  selectLibraryBlocks,
  setLibraryEnabled,
} from './library-service.js';
//=============================================================================
beforeEach(() => {
  mocks.manifests.clear();
  mocks.loaded.clear();
  mocks.applySearchPaths.mockReset();
  mocks.persist.mockReset();
  mocks.unload.mockReset();
  mocks.rpc.mockReset();
  mocks.diagnostic.mockReset();
  mocks.status.mockReset();
  state.blocks.clear();
});
//=============================================================================
test('built-ins are enabled and external usage is derived from block types', () => {
  mocks.manifests.set('builtin.source', { id: 'builtin.source', title: 'Source', blocks: [] });
  mocks.manifests.set('custom.lib', {
    id: 'custom.lib',
    title: 'Custom',
    _dir: 'C:\\libraries-a\\custom',
    blocks: [{ type: 'customBlock' }],
  });
  state.blocks.set('b1', { id: 'b1', type: 'nflow.custom.lib.customBlock' });

  const snapshot = getLibraryServiceSnapshot();
  expect(snapshot.libraries.find((item) => item.id === 'builtin.source')?.enabled).toBe(true);
  expect(snapshot.libraries.find((item) => item.id === 'custom.lib')?.usedBlockIds).toEqual(['b1']);
});
//=============================================================================
test('a library used by the diagram cannot be disabled', async () => {
  mocks.manifests.set('custom.lib', {
    id: 'custom.lib',
    _dir: 'C:\\libraries-a\\custom',
    blocks: [{ type: 'customBlock' }],
  });
  mocks.loaded.set('custom.lib', {});
  state.blocks.set('b1', { id: 'b1', type: 'customBlock' });
  const library = getLibraryServiceSnapshot().libraries[0];

  await expect(setLibraryEnabled(library, false)).rejects.toThrow('used by the current diagram');
  expect(mocks.unload).not.toHaveBeenCalled();
});
//=============================================================================
test('a location containing a used library cannot be removed', async () => {
  mocks.manifests.set('custom.lib', {
    id: 'custom.lib',
    _dir: 'C:\\libraries-a\\custom',
    blocks: [{ type: 'customBlock' }],
  });
  state.blocks.set('b1', { id: 'b1', type: 'customBlock' });

  await expect(removeLibraryLocation(0)).rejects.toThrow('used by the diagram');
  expect(mocks.applySearchPaths).not.toHaveBeenCalled();
});
//=============================================================================
test('adding a location preserves priority order and persists it', async () => {
  mocks.applySearchPaths.mockResolvedValue({
    success: true,
    paths: [...mocks.paths, 'C:\\libraries-c'],
    manifests: [],
    locations: [],
    diagnostics: [],
  });
  await addLibraryLocation('C:\\libraries-c');

  expect(mocks.applySearchPaths).toHaveBeenCalledWith([
    'C:\\libraries-a',
    'C:\\libraries-b',
    'C:\\libraries-c',
  ]);
  expect(mocks.persist).toHaveBeenCalled();
});
//=============================================================================
test('a preference write failure rejects the serialized mutation', async () => {
  mocks.applySearchPaths.mockResolvedValue({
    success: true,
    paths: [...mocks.paths, 'C:\\libraries-c'],
    manifests: [],
    locations: [],
    diagnostics: [],
  });
  mocks.persist.mockRejectedValue(new Error('Library preferences could not be saved.'));

  await expect(addLibraryLocation('C:\\libraries-c')).rejects.toThrow(
    'Library preferences could not be saved.'
  );
});
//=============================================================================
test('locating and moving locations preserve the requested ordering', async () => {
  mocks.applySearchPaths.mockResolvedValue({
    success: true,
    paths: mocks.paths,
    manifests: [],
    locations: [],
    diagnostics: [],
  });

  await replaceLibraryLocation(1, 'C:\\libraries-c');
  expect(mocks.applySearchPaths).toHaveBeenNthCalledWith(1, ['C:\\libraries-a', 'C:\\libraries-c']);

  await moveLibraryLocation(1, -1);
  expect(mocks.applySearchPaths).toHaveBeenNthCalledWith(2, ['C:\\libraries-b', 'C:\\libraries-a']);
});
//=============================================================================
test('rescan publishes discovery diagnostics and a success status', async () => {
  mocks.applySearchPaths.mockResolvedValue({
    success: true,
    paths: mocks.paths,
    manifests: [],
    locations: [],
    diagnostics: ['Duplicate library ignored: custom.lib'],
    message: 'Library locations scanned.',
  });

  await rescanLibraries();

  expect(mocks.diagnostic).toHaveBeenCalledWith({
    severity: 'warning',
    message: 'Duplicate library ignored: custom.lib',
    source: 'library',
  });
  expect(mocks.status).toHaveBeenCalledWith(
    expect.objectContaining({ level: 'success', message: 'Library locations scanned.' })
  );
});
//=============================================================================
test('mutations are serialized while a scan is pending', async () => {
  let releaseFirst: ((value: unknown) => void) | undefined;
  mocks.applySearchPaths
    .mockImplementationOnce(
      () =>
        new Promise((resolve) => {
          releaseFirst = resolve;
        })
    )
    .mockResolvedValue({
      success: true,
      paths: mocks.paths,
      manifests: [],
      locations: [],
      diagnostics: [],
    });

  const first = addLibraryLocation('C:\\libraries-c');
  const second = addLibraryLocation('C:\\libraries-d');
  await Promise.resolve();
  await Promise.resolve();
  expect(mocks.applySearchPaths).toHaveBeenCalledTimes(1);
  expect(getLibraryServiceSnapshot().busy).toBe(true);

  releaseFirst?.({
    success: true,
    paths: mocks.paths,
    manifests: [],
    locations: [],
    diagnostics: [],
  });
  await first;
  await second;
  expect(mocks.applySearchPaths).toHaveBeenCalledTimes(2);
  expect(getLibraryServiceSnapshot().busy).toBe(false);
});
//=============================================================================
test('a failed native unload is reported and leaves the library enabled', async () => {
  mocks.manifests.set('custom.native', {
    id: 'custom.native',
    native: 'blocks.dll',
    _dir: 'C:\\libraries-a\\native',
    blocks: [],
  });
  mocks.loaded.set('custom.native', {});
  mocks.rpc.mockResolvedValue({ success: false, message: 'Native unload failed.' });
  const library = getLibraryServiceSnapshot().libraries[0];

  await expect(setLibraryEnabled(library, false)).rejects.toThrow('Native unload failed.');
  expect(mocks.unload).not.toHaveBeenCalled();
  expect(mocks.status).toHaveBeenCalledWith(
    expect.objectContaining({
      level: 'error',
      message: 'Native unload failed.',
      diagnosticSource: 'library',
    })
  );
});
//=============================================================================
test('selecting used blocks sends all ids and centers the first', () => {
  const listener = vi.fn();
  document.addEventListener('nf-select-block', listener);
  selectLibraryBlocks(['b2', 'b3']);
  const event = listener.mock.calls[0][0] as CustomEvent;
  expect(event.detail).toEqual({ blockId: 'b2', blockIds: ['b2', 'b3'] });
  document.removeEventListener('nf-select-block', listener);
});
//=============================================================================
