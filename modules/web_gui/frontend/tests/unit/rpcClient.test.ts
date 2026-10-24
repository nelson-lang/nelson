//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, describe, expect, it, vi } from 'vitest';
import {
  emitLocalPushValue,
  installLocalRpcBackend,
  isBackendConnected,
  onPush,
  rpc,
} from '../../src/rpc/client';
//=============================================================================
type Call = (name: string, payload: string) => Promise<string>;
const g = window as unknown as { webui?: { call: Call } };
function reply(result: unknown): Call {
  return async (_name, payload) => JSON.stringify({ id: JSON.parse(payload).id, result });
}
//=============================================================================
afterEach(() => {
  vi.useRealTimers();
  delete g.webui;
});
//=============================================================================
describe('rpc client', () => {
  it('sends one envelope on the rpc binding and unwraps the result', async () => {
    const call = vi.fn(reply({ ok: true }));
    g.webui = { call };

    await expect(rpc('figure.list', { a: 1 })).resolves.toEqual({ ok: true });
    expect(call).toHaveBeenCalledTimes(1);
    const [name, payload] = call.mock.calls[0];
    expect(name).toBe('rpc');
    expect(JSON.parse(payload)).toMatchObject({ method: 'figure.list', params: { a: 1 } });
  });

  it('turns an error envelope into a rejection naming the method', async () => {
    g.webui = {
      call: async (_n, p) => JSON.stringify({ id: JSON.parse(p).id, error: { message: 'bad' } }),
    };
    await expect(rpc('var.get')).rejects.toThrow('nelson-webview RPC "var.get": bad');
  });

  it('retries an idempotent read after a transport failure', async () => {
    vi.useFakeTimers();
    const call = vi
      .fn<Call>()
      .mockRejectedValueOnce(new Error('WebSocket is not connected'))
      .mockImplementation(reply('ok'));
    g.webui = { call };

    const result = rpc('figure.list');
    await vi.advanceTimersByTimeAsync(200);
    await expect(result).resolves.toBe('ok');
    expect(call).toHaveBeenCalledTimes(2);
  });

  it('never resends a side-effectful command', async () => {
    const call = vi.fn<Call>().mockRejectedValue(new Error('transport down'));
    g.webui = { call };

    await expect(rpc('term.input', { line: 'x = 1' })).rejects.toThrow('transport down');
    expect(call).toHaveBeenCalledTimes(1);
  });

  it('never resends examples actions', async () => {
    const call = vi.fn<Call>().mockRejectedValue(new Error('transport down'));
    g.webui = { call };

    await expect(
      rpc('examples.run', { path: '/modules/graphics/examples/cube.m' })
    ).rejects.toThrow('transport down');
    await expect(
      rpc('examples.open', { path: '/modules/graphics/examples/cube.m' })
    ).rejects.toThrow('transport down');
    expect(call).toHaveBeenCalledTimes(2);
  });

  it('timeoutMs rejects and stops the retry loop', async () => {
    vi.useFakeTimers();
    const call = vi.fn<Call>().mockRejectedValue(new Error('cold websocket'));
    g.webui = { call };

    const result = rpc('figure.list', undefined, { timeoutMs: 500 });
    const assertion = expect(result).rejects.toMatchObject({ name: 'TimeoutError' });
    await vi.advanceTimersByTimeAsync(500);
    await assertion;
    const calls = call.mock.calls.length;
    await vi.advanceTimersByTimeAsync(10_000);
    expect(call).toHaveBeenCalledTimes(calls);
  });

  it('an aborted signal rejects a pending call', async () => {
    g.webui = { call: () => new Promise<string>(() => undefined) };
    const controller = new AbortController();

    const result = rpc('debugger.state', undefined, { signal: controller.signal });
    controller.abort();
    await expect(result).rejects.toMatchObject({ name: 'AbortError' });
  });

  it('routes backend pushes to channel subscribers', () => {
    const handler = vi.fn();
    const off = onPush('test.channel', handler);
    window.__nelsonPush?.('test.channel', JSON.stringify({ n: 1 }));
    window.__nelsonPush?.('other.channel', '{}');
    off();
    window.__nelsonPush?.('test.channel', '{}');
    expect(handler).toHaveBeenCalledTimes(1);
    expect(handler).toHaveBeenCalledWith({ n: 1 });
  });

  it('routes structured local pushes without JSON serialization', () => {
    const handler = vi.fn();
    const payload = { frame: new Uint8Array([1, 2, 3]) };
    const off = onPush('test.local-frame', handler);
    emitLocalPushValue('test.local-frame', payload);
    off();
    expect(handler).toHaveBeenCalledWith(payload);
  });

  it('hydrates unchanged native animation images before notifying subscribers', () => {
    const handler = vi.fn();
    const off = onPush('figure.frame', handler);
    const image = { kind: 'image', imageId: 'native-static-image', x: 0, y: 0, w: 4, h: 4 };
    const pixels = { rgba: '/wAA/w==', imageWidth: 1, imageHeight: 1 };
    window.__nelsonPush?.(
      'figure.frame',
      JSON.stringify({
        handle: 1,
        displayList: { width: 4, height: 4, primitives: [{ ...image, ...pixels }] },
      })
    );
    window.__nelsonPush?.(
      'figure.frame',
      JSON.stringify({
        handle: 1,
        displayList: { width: 4, height: 4, primitives: [image] },
      })
    );
    off();
    expect(handler.mock.calls[1][0].displayList.primitives[0]).toEqual({ ...image, ...pixels });
  });

  it('seeds native streaming image references from an initial snapshot', async () => {
    const image = { kind: 'image', imageId: 'native-snapshot-image', x: 0, y: 0, w: 4, h: 4 };
    g.webui = {
      call: reply({ width: 4, height: 4, primitives: [{ ...image, image: 'data:png' }] }),
    };
    await rpc('figure.getDisplayList', { handle: 1 });
    const handler = vi.fn();
    const off = onPush('figure.frame', handler);
    window.__nelsonPush?.(
      'figure.frame',
      JSON.stringify({
        handle: 1,
        displayList: { width: 4, height: 4, primitives: [image] },
      })
    );
    off();
    expect(handler.mock.calls[0][0].displayList.primitives[0]).toMatchObject({
      ...image,
      image: 'data:png',
    });
  });

  it('routes calls to an installed local backend', async () => {
    const handle = vi.fn(async (method: string, params: Record<string, unknown>) => ({
      method,
      params,
    }));
    const dispose = vi.fn();
    const remove = installLocalRpcBackend({ handle, dispose });

    expect(isBackendConnected()).toBe(true);
    await expect(rpc('term.sync', { ready: true })).resolves.toEqual({
      method: 'term.sync',
      params: { ready: true },
    });
    remove();
    expect(dispose).toHaveBeenCalledTimes(1);
  });
});
//=============================================================================
