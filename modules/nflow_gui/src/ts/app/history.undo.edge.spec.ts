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
test('undo/redo restore diagram after paste (edge case)', async () => {
  await vi.doMock('./diagram-handlers', () => ({
    loadDiagram: async (d: any) => {
      const s = await import('./state.js');
      s.state.blocks.clear();
      if (Array.isArray(d.blocks)) {
        for (const b of d.blocks) {
          s.state.blocks.set(b.id, { ...b } as any);
        }
      }
      s.state.connections = Array.isArray(d.connections) ? d.connections.slice() : [];
      s.state.diagramName = String(d.name || s.state.diagramName);
    },
  }));

  const hist = await import('./history.js');
  const s = await import('./state.js');

  let nextId = 2000;
  const fakeRenderer = {
    current: {
      createBlock: (type: string, x: number, y: number, opts: any) => {
        const id = nextId++;
        const block = { id, type, x, y, params: (opts && opts.params) || {} } as any;
        s.state.blocks.set(id, block);
        return block;
      },
      createConnection: (from: any, to: any, toIndex?: number, fromIndex?: number) => {
        s.state.connections.push({ from, to, toIndex, fromIndex });
        return true;
      },
      updateSelectionBox: () => {},
      clearWorkspace: () => {},
    },
  } as any;

  s.state.blocks.clear();
  s.state.blocks.set(1, { id: 1, type: 'constant', x: 10, y: 10, params: {} } as any);
  s.state.diagramName = 'undo-edge-test';
  hist.initHistory({ renderer: fakeRenderer });

  s.state.blocks.set(2, { id: 2, type: 'ramp', x: 20, y: 20, params: {} } as any);
  s.state.selectedId = null;
  s.state.selectedIds = new Set([2]);

  // Ensure no navigator clipboard so copySelection uses in-memory fallback
  try {
    delete (global as any).navigator;
  } catch (_e) {}

  const copied = await hist.copySelection();
  expect(copied).toBe(true);

  const pasted = await hist.pasteSelection();
  expect(pasted).toBe(true);

  // Commit diagram so history records the new state
  window.dispatchEvent(new Event('diagramCommitted'));

  const undone = await hist.undo();
  expect(undone).toBe(true);
  expect(Array.from(s.state.blocks.keys()).sort()).toEqual([1]);

  const redone = await hist.redo();
  expect(redone).toBe(true);
  const keys = Array.from(s.state.blocks.keys()).sort((a, b) => Number(a) - Number(b));
  expect(keys.length).toBeGreaterThanOrEqual(1);
  expect(keys.some((k) => k !== 1)).toBe(true);
});
//=============================================================================
export {};
//=============================================================================
