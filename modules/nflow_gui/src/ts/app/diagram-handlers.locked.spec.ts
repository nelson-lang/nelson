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
// Ensure library-loader reports no missing block types so loadDiagram proceeds
vi.doMock('./library-loader', () => ({ getMissingBlockTypes: () => [] }));
const dh = await import('./diagram-handlers');
const stateMod = await import('./state');
const state = stateMod.state;
beforeEach(() => {
  document.body.innerHTML = '';
  state.blocks.clear();
  state.connections = [] as any;
  state.subsystemStack = [] as any;
});
//=============================================================================
test('loadDiagram creates blocks even when file locked is true and keeps locked flag', async () => {
  const renderer = {
    current: {
      createBlock: (type: string, x: number, y: number, opts?: any) => {
        const id = (opts && opts.id) || `b${Math.floor(Math.random() * 10000) + 100}`;
        state.blocks.set(id, { id, type, x, y, params: (opts && opts.params) || {} });
        return { id };
      },
      createConnection: (from: any, to: any, toIndex?: number, fromIndex?: number) => {
        const conn: any = {
          from,
          to,
          toIndex: Number(toIndex || 0),
          fromIndex: Number(fromIndex || 0),
        };
        state.connections.push(conn);
        return conn;
      },
      getLastConnectionError: () => null,
    },
  } as any;

  dh.setRendererRef(renderer as any);

  // Start with state locked = true to simulate file produced by a locked UI
  state.locked = true;

  const data = {
    name: 'locked-test',
    locked: true,
    blocks: [{ id: 'b1', type: 'constant', x: 5, y: 5 }],
    connections: [],
  };

  await dh.loadDiagram(data);

  expect(state.blocks.size).toBeGreaterThan(0);
  expect(state.locked).toBe(true);
  expect(document.body.classList.contains('diagram-locked')).toBe(true);
});
//=============================================================================
