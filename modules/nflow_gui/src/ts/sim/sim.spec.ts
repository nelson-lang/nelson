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
const simModuleUrl = '../../../dist/sim/sim.js';
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
beforeEach(() => {
  (global as any).requestAnimationFrame = (cb: any) => {
    cb();
    return 1;
  };
  (global as any).cancelAnimationFrame = () => {};
});
//=============================================================================
afterEach(() => {
  delete (global as any).requestAnimationFrame;
  delete (global as any).cancelAnimationFrame;
  delete (global as any).window.webui;
});
//=============================================================================
test('drawScope / drawXYScope / drawXYZScope set data and return without error', async () => {
  const mod = await import(simModuleUrl);
  const scopeBlock: any = { id: 's1' };
  (mod as any).drawScope(scopeBlock, [1, 2, 3], [[1, 2]], true);
  expect(scopeBlock.scopeData).toEqual({ time: [1, 2, 3], series: [[1, 2]], connected: true });

  const xy = { x: [1], y: [2] };
  const xyBlock: any = { id: 's2' };
  (mod as any).drawXYScope(xyBlock, xy, false);
  expect(xyBlock.xyScopeData).toEqual({ series: xy, connected: false });

  const xyz = { x: [1], y: [2], z: [3] };
  const xyzBlock: any = { id: 's3' };
  (mod as any).drawXYZScope(xyzBlock, xyz, true);
  expect(xyzBlock.xyzScopeData).toEqual({ series: xyz, connected: true });
});
//=============================================================================
test('simulate success updates statusEl and returns done', async () => {
  installSimRpcBackend(async () => JSON.stringify({ status: 'done', scopes: [] }));
  const mod = await import(simModuleUrl);
  const state = { blocks: new Map() };
  const statusEl: any = { textContent: '' };
  const statusRecords: any[] = [];
  const onStatusUpdate = (_msg: any, _time: any) => {
    statusRecords.push([_msg, _time]);
  };

  const res = await (mod as any).simulate({
    state,
    runtimeInput: { value: '5' },
    statusEl,
    onStatusUpdate,
  });
  expect(res.status).toBe('done');
  expect(statusEl.textContent).toBe('Done');
  expect(statusRecords).toContainEqual(['Done', 0]);
});
//=============================================================================
test('embedded simulation polls until the native result is complete', async () => {
  let pollCount = 0;
  (global as any).window.webui = {
    call: async (name: string, payload: string) => {
      if (name !== 'rpc') throw new Error(`unexpected call: ${name}`);
      const envelope = JSON.parse(payload);
      if (envelope.method === 'nflow.sim.run') {
        return JSON.stringify({
          id: envelope.id,
          result: JSON.stringify({ status: 'started', runId: 'sim-1', delivery: 'poll' }),
        });
      }
      if (envelope.method === 'nflow.sim.poll') {
        pollCount += 1;
        if (pollCount === 1) {
          throw new SyntaxError("JSON Parse error: Expected '}'");
        }
        const result =
          pollCount === 2
            ? {
                status: 'pending',
                runId: 'sim-1',
                partials: [{ currentTime: 0.5, scopes: [] }],
              }
            : { status: 'complete', runId: 'sim-1', result: { status: 'done', scopes: [] } };
        return JSON.stringify({ id: envelope.id, result: JSON.stringify(result) });
      }
      throw new Error(`unexpected method: ${envelope.method}`);
    },
  };

  const mod = await import(simModuleUrl);
  const statusEl: any = { textContent: '' };
  const res = await (mod as any).simulate({
    state: { blocks: new Map() },
    runtimeInput: { value: '1' },
    statusEl,
  });

  expect(res.status).toBe('done');
  expect(statusEl.textContent).toBe('Done');
  expect(pollCount).toBe(3);
});
//=============================================================================
test('simulate when native RPC returns an error envelope sets status', async () => {
  installSimRpcBackend(async () => {
    throw new Error('boom');
  });
  const mod = await import(simModuleUrl);
  const state = { blocks: new Map() };
  const statusEl: any = { textContent: '' };

  const res = await (mod as any).simulate({ state, runtimeInput: { value: '1' }, statusEl });
  expect(res.status).toBe('error');
  expect(res.reason).toBe('Error: boom');
  expect(statusEl.textContent).toMatch(/Error: boom/);
});
//=============================================================================
test('simulate parse error from native RPC returns parse_error', async () => {
  installSimRpcBackend(async () => 'not a json');
  const mod = await import(simModuleUrl);
  const state = { blocks: new Map() };
  const statusEl: any = { textContent: '' };

  const res = await (mod as any).simulate({ state, runtimeInput: { value: '1' }, statusEl });
  expect(res.status).toBe('error');
  expect(res.reason).toBe('parse_error');
  expect(statusEl.textContent).toBe('Error: invalid response from C++ engine');
});
//=============================================================================
test('simulate reports unsupported_block errors directly (no retry)', async () => {
  let callCount = 0;
  installSimRpcBackend(async () => {
    callCount += 1;
    return JSON.stringify({ status: 'error', reason: 'unsupported_block: foo' });
  });

  const mod = await import(simModuleUrl);
  const state = { blocks: new Map() };
  const statusEl: any = { textContent: '' };

  const res = await (mod as any).simulate({ state, runtimeInput: { value: '1' }, statusEl });
  // The engine is deterministic: retrying the same diagram is pointless.
  expect(callCount).toBe(1);
  expect(res.status).toBe('error');
  expect(res.reason).toBe('unsupported_block: foo');
  expect(statusEl.textContent).toBe('Error: unsupported_block: foo');
});
//=============================================================================
test('engine errorBlocks link the console entry and highlight blocks', async () => {
  installSimRpcBackend(async () =>
    JSON.stringify({
      status: 'error',
      reason: 'unsupported_block: foo',
      errorBlocks: ['b7'],
    })
  );

  const mod = await import(simModuleUrl);
  const state = { blocks: new Map() };
  const statusEl: any = { textContent: '' };
  const highlighted: any[] = [];
  const onHighlight = (ev: Event) => highlighted.push((ev as CustomEvent).detail?.ids ?? null);
  document.addEventListener('nf-error-blocks', onHighlight);

  const res = await (mod as any).simulate({ state, runtimeInput: { value: '1' }, statusEl });
  document.removeEventListener('nf-error-blocks', onHighlight);
  expect(res.status).toBe('error');
  // First event clears previous highlights, the second carries the ids.
  expect(highlighted[highlighted.length - 1]).toEqual(['b7']);
});
//=============================================================================
test('partial scopes are queued and flushed via requestAnimationFrame', async () => {
  let resolveRun: any;
  installSimRpcBackend(
    () =>
      new Promise((res) => {
        resolveRun = res;
      })
  );

  const modPromise = import(simModuleUrl);
  const mod = await modPromise;

  const statusEl: any = { textContent: '' };
  const statusRecords: any[] = [];
  const onStatusUpdate = (_msg: any, _time: any) => {
    statusRecords.push([_msg, _time]);
  };
  const state = { blocks: new Map([['b1', { id: 'b1' }]]) };

  const p = (mod as any).simulate({
    state,
    runtimeInput: { value: '1' },
    statusEl,
    onStatusUpdate,
  });

  (global as any).window.__nelsonPush(
    'nflow.sim.partial',
    JSON.stringify({
      progress: 0.5,
      currentTime: 0.123,
      scopes: [{ id: 'b1', type: 'scope', times: [0], series: [[1]], connected: true }],
    })
  );

  await vi.waitFor(() => expect(resolveRun).toBeTypeOf('function'));
  resolveRun(JSON.stringify({ status: 'done', scopes: [] }));
  const res = await p;
  expect(res.status).toBe('done');
  expect(statusRecords.length).toBeGreaterThan(0);
});
//=============================================================================
