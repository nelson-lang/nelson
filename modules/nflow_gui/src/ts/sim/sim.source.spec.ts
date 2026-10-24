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
/* eslint-disable @typescript-eslint/no-explicit-any */
function installSimRpcBackend(handler: (diagramJson: string) => unknown): ReturnType<typeof vi.fn> {
  const backend = vi.fn(handler);
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
          result: await backend(String(envelope.params?.diagramJson ?? '')),
        });
      } catch (err) {
        return JSON.stringify({
          id: envelope.id,
          error: { message: (err as Error)?.message || String(err) },
        });
      }
    },
  };
  return backend;
}
//=============================================================================
describe('simulate and drawing', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
    vi.resetAllMocks();
    delete (global as any).window.webui;
  });

  test('simulate calls native RPC and updates status', async () => {
    const statusEl = document.createElement('div');
    const onStatus = vi.fn();
    installSimRpcBackend(() => JSON.stringify({ status: 'done', scopes: [] }));

    const state = { blocks: new Map(), connections: [], variables: {} };
    const res = await simulate({ state, statusEl, onStatusUpdate: onStatus });
    expect(res.status).toBe('done');
    expect(statusEl.textContent).toBe('Done');
    expect(onStatus).toHaveBeenCalled();
  });

  test('simulate reports unsupported_block errors without retrying', async () => {
    const statusEl = document.createElement('div');
    const calls: string[] = [];
    const backend = installSimRpcBackend(() =>
      JSON.stringify({ status: 'error', reason: 'unsupported_block: foo' })
    );

    const state = { blocks: new Map(), connections: [], variables: {} };
    const res = await simulate({ state, statusEl, onStatusUpdate: (m: string) => calls.push(m) });
    expect(res.status).toBe('error');
    expect(backend).toHaveBeenCalledTimes(1);
    expect(calls.some((c) => c.startsWith('Running'))).toBe(true);
  });

  test('drawScope stores scopeData on block', () => {
    const block = { id: 's1' } as any;
    drawScope(block, [0, 1], [[1, 2]], true);
    expect(block.scopeData).toBeDefined();
    expect(block.scopeData.time).toEqual([0, 1]);
  });

  test('drawXYScope and drawXYZScope store data on block', () => {
    const b1 = { id: 'xy' } as any;
    drawXYScope(b1, { x: [1], y: [2] }, true);
    expect(b1.xyScopeData).toBeDefined();
    expect(b1.xyScopeData.series.x).toEqual([1]);

    const b2 = { id: 'xyz' } as any;
    drawXYZScope(b2, { x: [1], y: [2], z: [3] }, false);
    expect(b2.xyzScopeData).toBeDefined();
    expect(b2.xyzScopeData.series.z).toEqual([3]);
  });
});
//=============================================================================
// Mock the render modules before importing the module under test (ESM namespaces are read-only)
const renderScopeMock = { renderScope: vi.fn() };
const renderXYMock = { renderXYScope: vi.fn() };
const renderXYZMock = { renderXYZScope: vi.fn() };
const renderDisplayMock = { renderDisplay: vi.fn() };
vi.doMock('./render-scope.js', () => renderScopeMock as any);
vi.doMock('./render-xy-scope.js', () => renderXYMock as any);
vi.doMock('./render-xyz-scope.js', () => renderXYZMock as any);
vi.doMock('./render-display.js', () => renderDisplayMock as any);
let simulate: any;
let drawScope: any;
let drawXYScope: any;
let drawXYZScope: any;
let processPartialScopes: any;
beforeAll(async () => {
  const mod = await import('./sim.js');
  simulate = mod.simulate;
  drawScope = mod.drawScope;
  drawXYScope = mod.drawXYScope;
  drawXYZScope = mod.drawXYZScope;
  processPartialScopes = mod.processPartialScopes;
});
//=============================================================================
beforeEach(() => {
  vi.resetAllMocks();
  delete (global as any).window.webui;
  (global as any).requestAnimationFrame = (cb: any) => {
    cb();
    return 1;
  };
  (global as any).cancelAnimationFrame = () => {
    /* no-op */
  };
});
//=============================================================================
describe('simulate', () => {
  test('returns done when native RPC returns done', async () => {
    installSimRpcBackend(() => JSON.stringify({ status: 'done', scopes: [] }));
    const res = await simulate({
      state: { blocks: new Map(), connections: [], variables: {} } as any,
    });
    expect(res.status).toBe('done');
  });

  test('handles native RPC error envelopes', async () => {
    installSimRpcBackend(() => {
      throw new Error('boom');
    });
    const res = await simulate({
      state: { blocks: new Map(), connections: [], variables: {} } as any,
    });
    expect(res.status).toBe('error');
  });

  test('handles invalid JSON response', async () => {
    installSimRpcBackend(() => 'not-json');
    const res = await simulate({
      state: { blocks: new Map(), connections: [], variables: {} } as any,
    });
    expect(res.status).toBe('error');
    expect(res.reason).toBe('parse_error');
  });

  test('unsupported_block error is surfaced with its reason', async () => {
    const backend = installSimRpcBackend(() =>
      JSON.stringify({ status: 'error', reason: 'unsupported_block: foo' })
    );
    const res = await simulate({
      state: { blocks: new Map(), connections: [], variables: {} } as any,
    });
    expect(res.status).toBe('error');
    expect((res as any).reason).toBe('unsupported_block: foo');
    expect(backend).toHaveBeenCalledTimes(1);
  });

  test('flushes partial scopes via nflow push', async () => {
    const block = { id: 'b1' };
    installSimRpcBackend(
      () =>
        new Promise((res) => {
          setTimeout(() => res(JSON.stringify({ status: 'done', scopes: [] })), 10);
        })
    );
    const state = { blocks: new Map([[block.id, block]]), connections: [], variables: {} } as any;
    const simPromise = simulate({ state });
    // allow simulate to register the partial callback
    await new Promise((r) => setTimeout(r, 0));
    (global as any).window.__nelsonPush(
      'nflow.sim.partial',
      JSON.stringify({
        progress: 0.5,
        currentTime: 1,
        scopes: [{ id: 'b1', type: 'scope', times: [0], series: [[1]], connected: false }],
      })
    );
    // requestAnimationFrame is mocked to flush immediately in beforeEach, so renderScope should be called
    expect(renderScopeMock.renderScope).toHaveBeenCalled();
    const res = await simPromise;
    expect(res.status).toBe('done');
  });

  test('partial sink updates are dispatched generically', () => {
    const blocks = new Map<string, any>([
      ['scope1', { id: 'scope1' }],
      ['xy1', { id: 'xy1' }],
      ['xyz1', { id: 'xyz1' }],
      ['display1', { id: 'display1' }],
    ]);

    processPartialScopes(
      {
        progress: 0.25,
        currentTime: 0.5,
        scopes: [
          { id: 'scope1', type: 'scope', times: [0], series: [[1]], connected: true },
          { id: 'xy1', type: 'xyScope', x: [1], y: [2], connected: true },
          { id: 'xyz1', type: 'xyzScope', x: [1], y: [2], z: [3], connected: true },
          { id: 'display1', type: 'display', value: 42 },
        ],
      },
      blocks
    );

    expect(blocks.get('scope1').scopeData.time).toEqual([0]);
    expect(blocks.get('xy1').xyScopeData.series).toEqual({ x: [1], y: [2] });
    expect(blocks.get('xyz1').xyzScopeData.series).toEqual({ x: [1], y: [2], z: [3] });
    expect(blocks.get('display1').displayData).toEqual({ value: 42 });
    expect(renderScopeMock.renderScope).toHaveBeenCalledWith(blocks.get('scope1'));
    expect(renderXYMock.renderXYScope).toHaveBeenCalledWith(blocks.get('xy1'));
    expect(renderXYZMock.renderXYZScope).toHaveBeenCalledWith(blocks.get('xyz1'));
    expect(renderDisplayMock.renderDisplay).toHaveBeenCalledWith(blocks.get('display1'));
  });
});
//=============================================================================
describe('draw helpers', () => {
  test('drawScope sets scopeData and calls renderScope', () => {
    const block: any = { id: 'b1' };
    drawScope(block, [0, 1], [[1, 2]], false);
    expect(block.scopeData).toBeDefined();
    expect(renderScopeMock.renderScope).toHaveBeenCalledWith(block);
  });

  test('drawXYScope sets xyScopeData and calls renderXYScope', () => {
    const block: any = { id: 'b2' };
    drawXYScope(block, { x: [1], y: [2] }, false);
    expect(block.xyScopeData).toBeDefined();
    expect(renderXYMock.renderXYScope).toHaveBeenCalledWith(block);
  });

  test('drawXYZScope sets xyzScopeData and calls renderXYZScope', () => {
    const block: any = { id: 'b3' };
    drawXYZScope(block, { x: [1], y: [2], z: [3] }, false);
    expect(block.xyzScopeData).toBeDefined();
    expect(renderXYZMock.renderXYZScope).toHaveBeenCalledWith(block);
  });
});
//=============================================================================
