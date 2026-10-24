//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { closeLibraryManager, mountLibraryManager, refreshPanel } from './library-manager.js';
import { getDiscoveredManifests, getLoadedLibraries } from './library-loader.js';
//=============================================================================
describe('library-manager mounting', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    getDiscoveredManifests().clear();
    getLoadedLibraries().clear();
  });

  test('renders the dedicated empty view with accessible controls', () => {
    const container = document.createElement('div');
    mountLibraryManager(container);

    expect(container.querySelector('[aria-label="Library Manager"]')).toBeTruthy();
    expect(container.querySelector('input[type="search"]')).toBeTruthy();
    expect(container.textContent).toContain('Library locations');
    expect(container.textContent).toContain('No external libraries discovered.');
    expect(() => refreshPanel()).not.toThrow();
  });

  test('separates built-in and external libraries and keeps built-ins read-only', () => {
    getDiscoveredManifests().set('builtin.source', {
      id: 'builtin.source',
      title: 'Source',
      version: '1.0.0',
      blocks: [{ type: 'constant' }],
    });
    getDiscoveredManifests().set('custom.library', {
      id: 'custom.library',
      title: 'Custom Library',
      blocks: [{ type: 'customBlock' }],
      _dir: 'C:\\libraries\\custom',
    });
    getLoadedLibraries().set('builtin.source', {
      id: 'builtin.source',
      manifest: {},
      library: {},
    } as never);

    const container = document.createElement('div');
    mountLibraryManager(container);

    expect(container.querySelectorAll('.lib-manager-library-row')).toHaveLength(2);
    expect(container.textContent).toContain('Built-in');
    expect(container.textContent).toContain('Custom Library');
    expect(container.querySelectorAll('[role="switch"]')).toHaveLength(1);
    expect((container.querySelector('[role="switch"]') as HTMLInputElement).checked).toBe(false);
  });

  test('refreshPanel reflects newly discovered libraries', () => {
    const container = document.createElement('div');
    mountLibraryManager(container);
    expect(container.textContent).not.toContain('Math');

    getDiscoveredManifests().set('builtin.math', {
      id: 'builtin.math',
      title: 'Math',
      version: '1.0.0',
      blocks: [],
    });
    refreshPanel();

    expect(container.textContent).toContain('Math');
  });

  test('close restores the block palette and focuses its search field', () => {
    document.body.innerHTML = `
      <div id="blockLibraryView" hidden><input id="blockLibrarySearch"></div>
      <div id="libraryManager"></div>
    `;
    closeLibraryManager();

    expect(document.getElementById('blockLibraryView')?.hasAttribute('hidden')).toBe(false);
    expect(document.getElementById('libraryManager')?.hasAttribute('hidden')).toBe(true);
    expect(document.activeElement).toBe(document.getElementById('blockLibrarySearch'));
  });
});
//=============================================================================
