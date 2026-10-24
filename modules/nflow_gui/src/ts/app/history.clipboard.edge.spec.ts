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
//=============================================================================
let state: any;
describe('history clipboard mapping edge cases', () => {
  beforeEach(async () => {
    vi.resetModules();
    const stateModule = await import('./state.js');
    state = stateModule.state;
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

  test('pasteSelection ignores connections referencing missing blocks', async () => {
    const payload = {
      __nflow__: true,
      created: Date.now(),
      blocks: [{ id: 1, type: 'ramp', x: 10, y: 10, rotation: 0, params: {} }],
      // connection references a missing block (to:2)
      connections: [{ from: 1, to: 2, fromIndex: 0, toIndex: 0 }],
    } as any;

    // Ensure no clipboard API so copySelection will set the in-memory fallback
    try {
      delete (global as any).navigator;
    } catch (_e) {}

    let nextId = 500;
    const createBlockMock = vi.fn((type: string, x: number, y: number, opts: any) => {
      const id = nextId++;
      const block = { id, type, x, y, params: (opts && opts.params) || {} } as any;
      state.blocks.set(id, block);
      return block;
    });
    const createConnectionMock = vi.fn();

    const mockRenderer = {
      current: {
        createBlock: createBlockMock,
        createConnection: createConnectionMock,
        updateSelectionBox: vi.fn(),
      },
    } as any;

    const m = await import('./history.js');
    m.initHistory({ renderer: mockRenderer });
    // Prepare state so copySelection will include a connection referencing missing block id=2
    state.blocks.clear();
    state.blocks.set(1, { id: 1, type: 'ramp', x: 10, y: 10, rotation: 0, params: {} } as any);
    state.connections = [{ from: 1, to: 2, fromIndex: 0, toIndex: 0 } as any];
    state.selectedId = null;
    state.selectedIds = new Set([1, 2]);

    const copied = await (m as any).copySelection();
    expect(copied).toBe(true);

    const res = await (m as any).pasteSelection();
    expect(res).toBe(true);
    // Block present, but connection should NOT be recreated because '2' is missing
    expect(createBlockMock).toHaveBeenCalledTimes(1);
    expect(createConnectionMock).toHaveBeenCalledTimes(0);
  });

  test('pasteSelection returns false for invalid JSON', async () => {
    (global as any).navigator = { clipboard: { readText: vi.fn(async () => 'not-json') } } as any;

    const createBlockMock = vi.fn();
    const mockRenderer = { current: { createBlock: createBlockMock } } as any;

    const m = await import('./history.js');
    m.initHistory({ renderer: mockRenderer });

    const res = await (m as any).pasteSelection();
    expect(res).toBe(false);
    expect(createBlockMock).not.toHaveBeenCalled();
  });

  test('pasteSelection returns false when payload missing __nflow__', async () => {
    const payload = { created: Date.now(), blocks: [] } as any;
    (global as any).navigator = {
      clipboard: { readText: vi.fn(async () => JSON.stringify(payload)) },
    } as any;

    const createBlockMock = vi.fn();
    const mockRenderer = { current: { createBlock: createBlockMock } } as any;

    const m = await import('./history.js');
    m.initHistory({ renderer: mockRenderer });

    const res = await (m as any).pasteSelection();
    expect(res).toBe(false);
    expect(createBlockMock).not.toHaveBeenCalled();
  });
});
//=============================================================================
// undo/redo tests live in a separate file to allow module mocking.
export {};
//=============================================================================
