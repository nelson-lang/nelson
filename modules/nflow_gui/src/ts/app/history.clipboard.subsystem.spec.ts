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
describe('history clipboard - subsystem payload regression', () => {
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

  test('pasteSelection restores subsystem params and recreates internal wires', async () => {
    const payload = {
      __nflow__: true,
      created: Date.now(),
      blocks: [
        {
          id: 's1',
          type: 'subsystem',
          x: 10,
          y: 10,
          rotation: 0,
          params: {
            name: 'MySub',
            externalInputs: [{ id: 'ext_in', name: 'In1' }],
            externalOutputs: [{ id: 'ext_out', name: 'Out1' }],
          },
        },
        { id: 'b1', type: 'ramp', x: 60, y: 10, rotation: 0, params: {} },
      ],
      connections: [{ from: 'b1', to: 's1', fromIndex: 0, toIndex: 0 }],
    } as any;

    try {
      delete (global as any).navigator;
    } catch (_e) {}
    state.blocks.clear();
    payload.blocks.forEach((b: any) => state.blocks.set(String(b.id), b));
    state.connections = payload.connections as any;
    state.selectedIds = new Set(payload.blocks.map((b: any) => String(b.id)));

    let nextId = 800;
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
    const res = await (m as any).pasteSelection();
    expect(res).toBe(true);

    expect(createBlockMock).toHaveBeenCalledTimes(2);
    expect(createConnectionMock).toHaveBeenCalledTimes(1);

    // one createBlock call carries externalInputs
    const hadSubsystem = createBlockMock.mock.calls.some(
      (c: any) =>
        c[3] &&
        c[3].params &&
        Array.isArray(c[3].params.externalInputs) &&
        c[3].params.externalInputs.length === 1
    );
    expect(hadSubsystem).toBe(true);
  });
});
//=============================================================================
export {};
//=============================================================================
