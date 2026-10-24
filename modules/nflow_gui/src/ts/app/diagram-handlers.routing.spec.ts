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
test('loadDiagram parses points and attaches to created connections', async () => {
  const renderer = {
    current: {
      createBlock: (type: string, x: number, y: number, opts?: any) => {
        const id = (opts && opts.id) || Math.floor(Math.random() * 10000) + 100;
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

  const data = {
    name: 'test',
    blocks: [{ id: 1, type: 'foo', x: 0, y: 0 }],
    connections: [{ from: 1, to: 1, points: '[[10,20],[30,40]]' }],
    routing: [{ from: 1, to: 1, points: '[[5,6],[7,8]]' }],
  };

  await dh.loadDiagram(data);

  expect(state.connections.length).toBeGreaterThan(0);
  const pts = (state.connections[0] as any).points;
  expect(Array.isArray(pts)).toBe(true);
  expect(pts.length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
