//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
describe('history clipboard regression - string ID select-all', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    state.blocks.clear();
    state.connections = [];
    state.selectedId = null;
    state.selectedIds = new Set();
  });

  afterEach(() => {
    try {
      delete (global as any).navigator;
    } catch (_e) {}
    state.blocks.clear();
    state.connections = [];
    state.selectedId = null;
    state.selectedIds = new Set();
  });

  test('copy/paste preserves internal wires when block ids are strings', async () => {
    const b1 = { id: 'b1', type: 'ramp', x: 10, y: 10, params: {} } as any;
    const b2 = { id: 'b2', type: 'gain', x: 40, y: 10, params: {} } as any;
    state.blocks.set('b1', b1);
    state.blocks.set('b2', b2);

    state.connections = [{ from: 'b1', to: 'b2', fromIndex: 0, toIndex: 0, points: [] } as any];

    state.selectedIds = new Set(Array.from(state.blocks.keys()));

    // no clipboard API, so history uses the in-memory fallback
    try {
      delete (global as any).navigator;
    } catch (_e) {}

    let nextId = 1000;
    const createBlockMock = vi.fn((type: string, x: number, y: number, opts: any) => ({
      id: nextId++,
    }));
    const createConnectionMock = vi.fn();
    const updateSelectionBoxMock = vi.fn();

    const mockRenderer = {
      current: {
        createBlock: createBlockMock,
        createConnection: createConnectionMock,
        updateSelectionBox: updateSelectionBoxMock,
      },
    } as any;

    initHistory({ renderer: mockRenderer });
    const m = await import('./history.js');

    const copied = await (m as any).copySelection();
    expect(copied).toBe(true);

    const pasted = await (m as any).pasteSelection();
    expect(pasted).toBe(true);

    expect(createBlockMock).toHaveBeenCalledTimes(2);
    expect(createConnectionMock).toHaveBeenCalledTimes(1);
    expect(state.selectedIds.size).toBe(2);
  });
});
//=============================================================================
export {};
//=============================================================================
