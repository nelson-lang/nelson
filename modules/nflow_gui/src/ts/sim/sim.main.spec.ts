//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { pathToFileURL } from 'url';
//=============================================================================
function installSimRpcBackend(handler: (diagramJson: string) => unknown): void {
  (global as any).window.webui = {
    call: async (name: string, payload: string) => {
      if (name !== 'rpc') throw new Error(`unexpected call: ${name}`);
      const envelope = JSON.parse(payload);
      if (envelope.method !== 'nflow.sim.run') {
        throw new Error(`unexpected method: ${envelope.method}`);
      }
      try {
        return JSON.stringify({
          id: envelope.id,
          result: await handler(String(envelope.params?.diagramJson ?? '')),
        });
      } catch (err) {
        return JSON.stringify({
          id: envelope.id,
          error: { message: (err as Error)?.message || String(err) },
        });
      }
    },
  };
}
//=============================================================================
afterEach(() => {
  delete (global as any).window.webui;
});
//=============================================================================
test('simulate native RPC success calls appropriate renderers and updates status', async () => {
  // Avoid mocking imported helpers - supply numeric params and observe status
  const renderCalls: any = { scope: 0, xy: 0, xyz: 0 };

  installSimRpcBackend(async (_diagramStr: string) =>
    JSON.stringify({
      status: 'done',
      scopes: [
        { id: 'b1', type: 'scope', times: [0], series: [[1, 2]], connected: false },
        { id: 'b2', type: 'xyScope', x: [1], y: [2], connected: true },
        { id: 'b3', type: 'xyzScope', x: [1], y: [2], z: [3], connected: false },
      ],
    })
  );

  const sim = await import('../../../dist/sim/sim.js');

  const blocksMap = new Map();
  blocksMap.set('b1', { id: 'b1', params: { a: '1' } });
  blocksMap.set('b2', { id: 'b2', params: { a: '2' } });
  blocksMap.set('b3', { id: 'b3', params: { a: '3' } });

  const state: any = { blocks: blocksMap, connections: [], variables: { dt: 0.02 } };

  const statusEl: any = { textContent: '' };
  const statusCalls: any[] = [];
  const onStatusUpdate = (_msg: any, _time: any) => {
    statusCalls.push([_msg, _time]);
  };

  const res = await (sim as any).simulate({
    state,
    runtimeInput: { value: '2' },
    statusEl,
    onStatusUpdate,
  });

  expect(res.status).toBe('done');
  expect(statusEl.textContent).toBe('Done');
  expect(statusCalls.length).toBeGreaterThanOrEqual(2);
});
//=============================================================================
test('simulate handles native RPC error envelope and reports error', async () => {
  installSimRpcBackend(async () => {
    throw new Error('cpp-failure');
  });

  const sim = await import('../../../dist/sim/sim.js');
  const state = { blocks: new Map(), connections: [] };
  const statusEl: any = { textContent: '' };

  const res = await (sim as any).simulate({ state, runtimeInput: { value: '0' }, statusEl });

  expect(res.status).toBe('error');
  expect(statusEl.textContent).toMatch(/Error/);
});
//=============================================================================
