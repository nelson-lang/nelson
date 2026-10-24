//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { initHistory } from './history.js';
import { state } from './state.js';
//=============================================================================
describe('history duplicateSelection', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    state.blocks.clear();
    state.connections = [];
    state.selectedId = null;
    state.selectedIds = new Set();
  });

  test('duplicateSelection creates new blocks and updates selection', () => {
    const orig = { id: 1, type: 'ramp', x: 10, y: 10, params: {} } as any;
    state.blocks.set(1, orig);
    state.selectedId = 1;
    state.selectedIds = new Set([1]);

    const mockRenderer = {
      current: {
        createBlock: vi.fn((type: string, x: number, y: number, opts: any) => ({ id: 2 })),
        createConnection: vi.fn(),
        updateSelectionBox: vi.fn(),
      },
    };
    initHistory({ renderer: mockRenderer as any });

    return import('./history.js').then((m) => {
      const res = (m as any).duplicateSelection();
      expect(res).toBe(true);
      expect(mockRenderer.current.createBlock).toHaveBeenCalled();
      expect(state.selectedIds.has(2)).toBe(true);
    });
  });
});
//=============================================================================
