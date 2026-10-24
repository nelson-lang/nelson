//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import {
  scoreMatch,
  filterTypes,
  getQuickInsertTypes,
  openQuickInsert,
  setRendererRef,
} from './quick-insert.js';
import { _resetForTests, subscribeStatus } from './status-service.js';
//=============================================================================
beforeEach(() => {
  _resetForTests();
  document.body.innerHTML = '';
});
//=============================================================================
describe('scoreMatch / filterTypes', () => {
  test('exact match beats prefix, prefix beats substring, substring beats subsequence', () => {
    expect(scoreMatch('gain', 'gain')).toBeGreaterThan(scoreMatch('gain', 'gainx'));
    expect(scoreMatch('ga', 'gain')).toBeGreaterThan(scoreMatch('ga', 'integrator'));
    expect(scoreMatch('sum', 'sum')).toBeGreaterThan(0);
    expect(scoreMatch('xyz', 'gain')).toBe(0);
  });

  test('filterTypes ranks all matches (no cap: the list scrolls)', () => {
    const types = [
      'gain',
      'sum',
      'integrator',
      'sine',
      'saturation',
      'sink',
      'step',
      'scope',
      'switch',
      'stateSpace',
    ];
    const res = filterTypes('s', types);
    expect(res.length).toBe(types.filter((t) => t.toLowerCase().includes('s')).length);
    expect(filterTypes('gain', types)[0]).toBe('gain');
    expect(filterTypes('', types).length).toBe(types.length);
  });

  test('registry exposes insertable builtin types', () => {
    const types = getQuickInsertTypes();
    expect(types.length).toBeGreaterThan(5);
    expect(types).toContain('gain');
    expect(types).toContain('sum');
  });
});
//=============================================================================
describe('openQuickInsert popup', () => {
  test('opens an input, Enter inserts the top match at the world position', () => {
    const createBlock = vi.fn(() => ({ id: 'b1' }));
    const updateConnections = vi.fn();
    setRendererRef({ current: { createBlock, updateConnections } });

    openQuickInsert(100, 100, 320, 240);
    const input = document.getElementById('quickInsertInput') as HTMLInputElement;
    expect(input).not.toBeNull();

    const seen: any[] = [];
    const unsub = subscribeStatus((item) => item && seen.push(item));
    input.value = 'gain';
    input.dispatchEvent(new Event('input'));
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter' }));
    unsub();

    expect(createBlock).toHaveBeenCalledWith('gain', 320, 240);
    expect(updateConnections).toHaveBeenCalled();
    expect(document.getElementById('quickInsertPopup')).toBeNull();
    // The status entry is linked to the created block (clickable console).
    expect(seen[0]?.blockId).toBe('b1');
  });

  test('Escape closes the popup without inserting', () => {
    const createBlock = vi.fn();
    setRendererRef({ current: { createBlock } });
    openQuickInsert(50, 50, 10, 10);
    const input = document.getElementById('quickInsertInput') as HTMLInputElement;
    input.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }));
    expect(document.getElementById('quickInsertPopup')).toBeNull();
    expect(createBlock).not.toHaveBeenCalled();
  });
});
//=============================================================================
