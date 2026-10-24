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
import * as sim from './sim';
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
  delete (global as any).requestAnimationFrame;
  delete (global as any).cancelAnimationFrame;
});
//=============================================================================
describe('simulate behavior (focused)', () => {
  test('simulate handles successful native RPC run and calls drawScope', async () => {
    const state = { blocks: new Map([['b1', { id: 'b1', type: 'scope' }]]) } as any;
    const scopes = [{ id: 'b1', type: 'scope', times: [0], series: [[1]], connected: [] }];
    installSimRpcBackend(() => JSON.stringify({ status: 'done', scopes }));

    const statusEl: any = { textContent: '' };
    const onStatus = vi.fn();

    const res = await sim.simulate({ state, statusEl, onStatusUpdate: onStatus });

    expect(res.status).toBe('done');
    const block = state.blocks.get('b1');
    expect(block.scopeData).toBeDefined();
    expect(block.scopeData.time).toEqual([0]);
    expect(statusEl.textContent).toBe('Done');
    expect(onStatus).toHaveBeenCalledWith('Done', 0);
  });

  test('simulate accepts an already parsed native RPC result object', async () => {
    const state = { blocks: new Map([['b1', { id: 'b1', type: 'scope' }]]) } as any;
    installSimRpcBackend(() => ({
      status: 'done',
      scopes: [{ id: 'b1', type: 'scope', times: [0, 1], series: [[1, 2]], connected: true }],
    }));

    const res = await sim.simulate({ state });

    expect(res.status).toBe('done');
    const block = state.blocks.get('b1');
    expect(block.scopeData).toEqual({ time: [0, 1], series: [[1, 2]], connected: true });
  });

  test('simulate processes async native RPC partial scopes before final result', async () => {
    (global as any).requestAnimationFrame = (cb: FrameRequestCallback) => {
      cb(0);
      return 1;
    };
    (global as any).cancelAnimationFrame = vi.fn();
    const state = { blocks: new Map([['b1', { id: 'b1', type: 'scope' }]]) } as any;
    (global as any).window.webui = {
      call: async (name: string, payload: string) => {
        if (name !== 'rpc') throw new Error(`unexpected call: ${name}`);
        const envelope = JSON.parse(payload);
        return JSON.stringify({
          id: envelope.id,
          result: JSON.stringify({ status: 'started', runId: 'test-run' }),
        });
      },
    };

    const simPromise = sim.simulate({ state });
    await new Promise((resolve) => setTimeout(resolve, 0));
    (global as any).window.__nelsonPush(
      'nflow.sim.partial',
      JSON.stringify({
        progress: 0.4,
        currentTime: 2,
        scopes: [{ id: 'b1', type: 'scope', times: [0, 1], series: [[3, 4]], connected: true }],
      })
    );
    expect(state.blocks.get('b1').scopeData).toEqual({
      time: [0, 1],
      series: [[3, 4]],
      connected: true,
    });

    (global as any).window.__nelsonPush(
      'nflow.sim.final',
      JSON.stringify({
        runId: 'test-run',
        result: {
          status: 'done',
          scopes: [
            { id: 'b1', type: 'scope', times: [0, 1, 2], series: [[3, 4, 5]], connected: true },
          ],
        },
      })
    );
    const res = await simPromise;
    expect(res.status).toBe('done');
    expect(state.blocks.get('b1').scopeData.time).toEqual([0, 1, 2]);
  });

  test('simulate catches native RPC errors and returns error status', async () => {
    const state = { blocks: new Map() } as any;
    installSimRpcBackend(() => {
      throw new Error('boom');
    });
    const statusEl: any = { textContent: '' };
    const res = await sim.simulate({ state, statusEl });
    expect(res.status).toBe('error');
    expect(res.reason).toBe('Error: boom');
    expect(statusEl.textContent).toMatch(/Error/);
  });
});
//=============================================================================
describe('simulate stop handling', () => {
  function installStartedRun(delivery?: 'poll'): void {
    (global as any).window.webui = {
      call: async (name: string, payload: string) => {
        if (name !== 'rpc') throw new Error(`unexpected call: ${name}`);
        const envelope = JSON.parse(payload);
        const result =
          envelope.method === 'nflow.sim.poll'
            ? JSON.stringify({ status: 'running' })
            : JSON.stringify({ status: 'started', runId: 'stop-run', delivery });
        return JSON.stringify({ id: envelope.id, result });
      },
    };
  }

  test('a stopped run whose final result never arrives settles as stopped', async () => {
    installStartedRun();
    const controller = new AbortController();
    const state = { blocks: new Map() } as any;

    const running = sim.simulate({ state, signal: controller.signal, stopGraceMs: 20 });
    await new Promise((resolve) => setTimeout(resolve, 0));
    controller.abort();

    await expect(running).resolves.toMatchObject({ status: 'stopped' });
  });

  test('a stopped run keeps the final result delivered within the grace period', async () => {
    installStartedRun();
    const controller = new AbortController();
    const state = { blocks: new Map([['b1', { id: 'b1', type: 'scope' }]]) } as any;

    const running = sim.simulate({ state, signal: controller.signal, stopGraceMs: 5000 });
    await new Promise((resolve) => setTimeout(resolve, 0));
    controller.abort();
    (global as any).window.__nelsonPush(
      'nflow.sim.final',
      JSON.stringify({
        runId: 'stop-run',
        result: {
          status: 'done',
          scopes: [{ id: 'b1', type: 'scope', times: [0, 1], series: [[7, 8]], connected: true }],
        },
      })
    );

    await expect(running).resolves.toMatchObject({ status: 'done' });
    expect(state.blocks.get('b1').scopeData.time).toEqual([0, 1]);
  });

  test('a stopped polled run stops polling after the grace period', async () => {
    installStartedRun('poll');
    const controller = new AbortController();
    const state = { blocks: new Map() } as any;

    const running = sim.simulate({ state, signal: controller.signal, stopGraceMs: 150 });
    await new Promise((resolve) => setTimeout(resolve, 0));
    controller.abort();

    await expect(running).resolves.toMatchObject({ status: 'stopped' });
  });
});
//=============================================================================
