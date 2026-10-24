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
async function setup() {
  await vi.doMock('./diagram-handlers', () => ({
    loadDiagram: async (d: any) => {
      const s = await import('./state.js');
      s.state.blocks.clear();
      for (const b of d.blocks || []) s.state.blocks.set(b.id, { ...b } as any);
      s.state.connections = Array.isArray(d.connections) ? d.connections.slice() : [];
    },
  }));
  const hist = await import('./history.js');
  const s = await import('./state.js');
  s.state.blocks.clear();
  s.state.blocks.set(1, { id: 1, type: 'constant', x: 10, y: 10, params: {} } as any);
  s.state.diagramName = 'after-apply';
  hist.initHistory({ renderer: { current: { clearWorkspace: () => {} } } as any });
  return { hist, s };
}
//=============================================================================
const commit = () => window.dispatchEvent(new Event('diagramCommitted'));
const xOf = (s: any) => (s.state.blocks.get(1) as any).x;
describe('history edits right after undo/redo', () => {
  test('an edit committed right after an undo is recorded', async () => {
    const { hist, s } = await setup();
    (s.state.blocks.get(1) as any).x = 50;
    commit();

    expect(await hist.undo()).toBe(true);
    expect(xOf(s)).toBe(10);

    // No delay: the edit must not fall into a post-undo blind window.
    (s.state.blocks.get(1) as any).x = 99;
    commit();

    expect(await hist.undo()).toBe(true);
    expect(xOf(s)).toBe(10);
    expect(await hist.redo()).toBe(true);
    expect(xOf(s)).toBe(99);
  });

  test('a change event echoed by the reload does not clear the redo stack', async () => {
    const { hist, s } = await setup();
    (s.state.blocks.get(1) as any).x = 50;
    commit();

    expect(await hist.undo()).toBe(true);
    commit(); // the reload notifies a change without editing anything

    expect(await hist.redo()).toBe(true);
    expect(xOf(s)).toBe(50);
  });
});
//=============================================================================
