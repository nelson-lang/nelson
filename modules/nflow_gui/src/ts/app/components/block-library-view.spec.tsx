//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  filterBlockLibraryView,
  normalizeLibrarySearch,
  renderBlockLibraryView,
} from './block-library-view.js';
import { _resetUiPrefsForTests } from '../ui-prefs.js';
//=============================================================================
beforeEach(() => {
  _resetUiPrefsForTests();
  document.body.innerHTML = '<div id="library"></div><div id="blockLibraryEmpty" hidden></div>';
  localStorage.clear();
});
//=============================================================================
test('normalizes accents and letter case', () => {
  expect(normalizeLibrarySearch('  GénÉrateur  ')).toBe('generateur');
});
//=============================================================================
test('filters blocks by type, label, or category', () => {
  const container = document.getElementById('library')!;
  renderBlockLibraryView(container, [
    {
      id: 'sources',
      title: 'Sources',
      blocks: [{ type: 'sine', label: 'Sine Wave' }],
    },
    {
      id: 'math',
      title: 'Math Operations',
      blocks: [{ type: 'gain', label: 'Gain' }],
    },
  ]);

  expect(filterBlockLibraryView(container, 'GÁIN')).toBe(1);
  expect(container.querySelector<HTMLElement>('[data-type="gain"]')?.hidden).toBe(false);
  expect(container.querySelector<HTMLElement>('[data-type="sine"]')?.hidden).toBe(true);
  expect(filterBlockLibraryView(container, 'missing')).toBe(0);
  expect(document.getElementById('blockLibraryEmpty')?.hidden).toBe(false);
});
//=============================================================================
test('temporary search expansion does not replace the saved group state', () => {
  localStorage.setItem('nf-library-group-sources-open', '0');
  const container = document.getElementById('library')!;
  renderBlockLibraryView(container, [
    {
      id: 'sources',
      title: 'Sources',
      blocks: [{ type: 'sine', label: 'Sine Wave' }],
    },
  ]);
  const group = container.querySelector<HTMLDetailsElement>('details.tool-group')!;
  expect(group.open).toBe(false);
  filterBlockLibraryView(container, 'sine');
  expect(group.open).toBe(true);
  expect(localStorage.getItem('nf-library-group-sources-open')).toBe('0');
  filterBlockLibraryView(container, '');
  expect(group.open).toBe(false);
});
//=============================================================================
