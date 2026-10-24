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
describe('history clipboard - named ports regression', () => {
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

  test('copy/paste preserves internal wires for named-port style ids', async () => {
    const p1 = { id: 'p_plant_out', type: 'labelSource', x: 10, y: 10, params: {} } as any;
    const p2 = { id: 'pi_plant_in', type: 'labelSink', x: 40, y: 10, params: {} } as any;
    state.blocks.set('p_plant_out', p1);
    state.blocks.set('pi_plant_in', p2);

    state.connections = [
      { from: 'p_plant_out', to: 'pi_plant_in', fromIndex: 0, toIndex: 0, points: [] } as any,
    ];

    state.selectedIds = new Set(Array.from(state.blocks.keys()));

    // force in-memory clipboard fallback
    try {
      delete (global as any).navigator;
    } catch (_e) {}

    let nextId = 500;
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
