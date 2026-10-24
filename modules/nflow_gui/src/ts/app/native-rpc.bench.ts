//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { bench, beforeEach, describe } from 'vitest';
import { nflowRpc } from './native-rpc.js';
//=============================================================================
const g = globalThis as typeof globalThis & { window: Window };
describe('nflow native RPC bridge', () => {
  const largeText = 'x'.repeat(512 * 1024);

  beforeEach(() => {
    g.window.webui = {
      call: async (_name: string, payload: string) => {
        const envelope = JSON.parse(payload);
        let result: unknown = envelope.params.payload ?? 'ok';
        if (envelope.method === 'nflow.file.readText') {
          result = JSON.stringify({ success: true, content: largeText });
        } else if (envelope.method === 'nflow.file.writeText') {
          result = JSON.stringify({ success: true });
        } else if (envelope.method === 'nflow.library.load') {
          result = {
            success: true,
            manifests: [
              {
                id: 'bench.large',
                blocks: Array.from({ length: 500 }, (_, i) => ({ type: `bench.block${i}` })),
              },
            ],
          };
        }
        return JSON.stringify({ id: envelope.id, result });
      },
    };
  });

  bench('routeWires payload envelope', async () => {
    const payload = JSON.stringify({
      snapshot: { blocks: [], connections: [] },
      width: 2000,
      height: 1200,
      timeLimitMs: 80,
    });
    await nflowRpc('nflow.router.routeWires', { payload });
  });

  bench('large simulation payload envelope', async () => {
    const blocks = Array.from({ length: 1000 }, (_, i) => ({
      id: `b${i}`,
      type: 'constant',
      x: i,
      y: i,
      params: { value: i },
    }));
    await nflowRpc('nflow.sim.run', { diagramJson: JSON.stringify({ blocks, connections: [] }) });
  });

  bench('large text read envelope', async () => {
    await nflowRpc('nflow.file.readText', { path: 'large.nflow' });
  });

  bench('large text write envelope', async () => {
    await nflowRpc('nflow.file.writeText', { path: 'large.nflow', content: largeText });
  });

  bench('large library load envelope', async () => {
    await nflowRpc('nflow.library.load', { path: '/tmp/nflow-large-library' });
  });
});
//=============================================================================
