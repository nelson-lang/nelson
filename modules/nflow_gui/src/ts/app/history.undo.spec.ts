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
describe('history undo/redo integration', () => {
  test('moving a block then undo restores position', async () => {
    // Mock the diagram-handlers module (history imports it) to avoid loading many deps
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

    const fakeRenderer = {
      current: {
        createBlock: (
          type: string,
          x: number,
          y: number,
          opts: Record<string, unknown> | undefined
        ) => {
          const id = (opts && (opts as any).id) || Math.floor(Math.random() * 100000);
          const block = { id, type, x, y, params: (opts && (opts as any).params) || {} };
          s.state.blocks.set(id as number, block as any);
          return block;
        },
        createConnection: () => true,
        updateSelectionBox: () => {},
        clearWorkspace: () => {},
      },
    } as any;

    s.state.blocks.clear();
    s.state.blocks.set(1, { id: 1, type: 'constant', x: 10, y: 10, params: {} } as any);
    s.state.diagramName = 'test';
    hist.initHistory({ renderer: fakeRenderer });

    const before = (s.state.blocks.get(1) as any).x;
    expect(before).toBe(10);

    const b = s.state.blocks.get(1) as any;
    b.x = 50;
    window.dispatchEvent(new Event('diagramChanged'));

    const ok = await hist.undo();
    expect(ok).toBe(true);
    const after = (s.state.blocks.get(1) as any).x;
    expect(after).toBe(10);
  });
});
//=============================================================================
