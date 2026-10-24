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
describe('history clipboard operations', () => {
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

  test('copySelection stores payload (fallback) and pasteSelection can restore it', async () => {
    const orig = { id: 1, type: 'ramp', x: 10, y: 10, params: {} } as any;
    state.blocks.set(1, orig);
    state.selectedId = 1;
    state.selectedIds = new Set([1]);

    // no clipboard API, so the in-memory fallback is exercised
    try {
      delete (global as any).navigator;
    } catch (_e) {}

    const createBlockMock = vi.fn((type: string, x: number, y: number, opts: any) => ({
      id: 200,
    }));
    const mockRenderer = {
      current: {
        createBlock: createBlockMock,
        createConnection: vi.fn(),
        updateSelectionBox: vi.fn(),
        deleteBlock: vi.fn(),
      },
    } as any;

    initHistory({ renderer: mockRenderer as any });

    const m = await import('./history.js');
    const res = await (m as any).copySelection();
    expect(res).toBe(true);

    const pres = await (m as any).pasteSelection();
    expect(pres).toBe(true);
    expect(createBlockMock).toHaveBeenCalledTimes(1);
  });

  test('pasteSelection recreates blocks and connections from clipboard', async () => {
    const payload = {
      __nflow__: true,
      created: Date.now(),
      blocks: [
        { id: 1, type: 'ramp', x: 10, y: 10, rotation: 0, params: {} },
        { id: 2, type: 'gain', x: 40, y: 10, rotation: 0, params: {} },
      ],
      connections: [{ from: 1, to: 2, fromIndex: 0, toIndex: 0 }],
    } as any;
    const clipboardText = JSON.stringify(payload);

    (global as any).navigator = {
      clipboard: {
        readText: vi.fn(async () => clipboardText),
        writeText: vi.fn(async () => {}),
      },
    } as any;

    let nextId = 100;
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

    initHistory({ renderer: mockRenderer as any });

    const m = await import('./history.js');
    const res = await (m as any).pasteSelection();
    expect(res).toBe(true);
    expect(createBlockMock).toHaveBeenCalledTimes(2);
    expect(createConnectionMock).toHaveBeenCalledTimes(1);
    expect(state.selectedIds.size).toBe(2);
  });

  test('cutSelection copies then deletes selected blocks', async () => {
    const orig = { id: 1, type: 'ramp', x: 10, y: 10, params: {} } as any;
    state.blocks.set(1, orig);
    state.selectedId = 1;
    state.selectedIds = new Set([1]);

    let written: string | null = null;
    const writeMock = vi.fn(async (t: string) => {
      written = t;
    });
    const deleteMock = vi.fn();

    (global as any).navigator = { clipboard: { writeText: writeMock } } as any;

    const mockRenderer = {
      current: {
        createBlock: vi.fn(),
        createConnection: vi.fn(),
        updateSelectionBox: vi.fn(),
        deleteBlock: deleteMock,
      },
    } as any;

    initHistory({ renderer: mockRenderer as any });

    const m = await import('./history.js');
    const res = await (m as any).cutSelection();
    expect(res).toBe(true);
    expect(writeMock).toHaveBeenCalled();
    expect(deleteMock).toHaveBeenCalledWith('1');
    expect(state.selectedIds.size).toBe(0);
    expect(state.selectedId).toBe(null);
  });
});
//=============================================================================
export {};
//=============================================================================
