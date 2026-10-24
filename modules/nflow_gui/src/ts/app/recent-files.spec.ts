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
  getRecentFiles,
  addRecentFile,
  persistRecentFiles,
  clearRecentFiles,
} from './recent-files';
//=============================================================================
describe('recent-files', () => {
  beforeEach(() => {
    try {
      localStorage.clear();
    } catch (_e) {}
  });

  test('starts empty', () => {
    expect(getRecentFiles()).toEqual([]);
  });

  test('addRecentFile adds entries and dedupes', () => {
    addRecentFile('/path/a');
    addRecentFile('/path/b');
    addRecentFile('/path/a');
    expect(getRecentFiles()).toEqual(['/path/a', '/path/b']);
  });

  test('limits to 10 entries', () => {
    for (let i = 0; i < 12; i++) addRecentFile('file' + i);
    const list = getRecentFiles();
    expect(list.length).toBeLessThanOrEqual(10);
    expect(list[0]).toBe('file11');
    expect(list[9]).toBe('file2');
  });

  test('persistRecentFiles writes to localStorage', () => {
    persistRecentFiles(['a', 'b', 'c']);
    const raw = localStorage.getItem('nf-recent-files-v1');
    expect(raw).not.toBeNull();
    expect(JSON.parse(String(raw))).toEqual(['a', 'b', 'c']);
  });

  test('clearRecentFiles removes the key', () => {
    addRecentFile('x');
    clearRecentFiles();
    expect(getRecentFiles()).toEqual([]);
  });
});
//=============================================================================
