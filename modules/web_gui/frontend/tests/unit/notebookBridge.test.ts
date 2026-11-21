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
  BRIDGE_CHANNEL,
  NotebookFigureBridge,
  type RendererMessaging,
} from '../../src/replayer/notebookBridge';
import { onPush, rpc } from '../../src/rpc/client';
//=============================================================================
interface Envelope {
  channel: string;
  outputId: string | null;
  handle: number;
  body: Record<string, unknown>;
}
//=============================================================================
// A messaging double that records what the bridge posts and lets a test push
// messages back the way the extension would.
function fakeMessaging() {
  const posted: Envelope[] = [];
  let listener: ((message: unknown) => void) | null = null;
  const messaging: RendererMessaging = {
    postMessage(message) {
      posted.push(message as Envelope);
    },
    onDidReceiveMessage(cb) {
      listener = cb;
      return { dispose: () => (listener = null) };
    },
  };
  return {
    messaging,
    posted,
    receive: (message: unknown) => listener?.(message),
  };
}
//=============================================================================
describe('NotebookFigureBridge', () => {
  let bridge: NotebookFigureBridge | null = null;

  afterEach(() => {
    bridge?.dispose();
    bridge = null;
  });

  it('announces a live-capable output and resolves on init', async () => {
    const host = fakeMessaging();
    bridge = new NotebookFigureBridge(host.messaging);
    expect(bridge.canGoLive()).toBe(true);

    const pending = bridge.requestLive('out-1', 7, () => undefined);
    const ready = host.posted.at(-1)!;
    expect(ready.channel).toBe(BRIDGE_CHANNEL);
    expect(ready.outputId).toBe('out-1');
    expect(ready.handle).toBe(7);
    expect(ready.body).toMatchObject({ control: 'ready', protocol: 1 });

    host.receive({
      channel: BRIDGE_CHANNEL,
      outputId: 'out-1',
      handle: 7,
      body: { control: 'init', live: true, capabilities: { figures: true } },
    });
    await expect(pending).resolves.toEqual({ live: true, capabilities: { figures: true } });
  });

  it('wraps an rpc request and resolves it from the matching response', async () => {
    const host = fakeMessaging();
    bridge = new NotebookFigureBridge(host.messaging);

    const call = rpc('figure.setView', { handle: 3, azimuth: 30 });
    const request = host.posted.at(-1)!;
    expect(request.channel).toBe(BRIDGE_CHANNEL);
    expect(request.handle).toBe(3);
    const body = request.body as { rpc: { id: number; method: string; params: unknown } };
    expect(body.rpc.method).toBe('figure.setView');
    expect(body.rpc.params).toEqual({ handle: 3, azimuth: 30 });

    host.receive({
      channel: BRIDGE_CHANNEL,
      outputId: 'out-1',
      handle: 3,
      body: { rpc: { id: body.rpc.id, result: { ok: true } } },
    });
    await expect(call).resolves.toEqual({ ok: true });
  });

  it('rejects an rpc request from an error response', async () => {
    const host = fakeMessaging();
    bridge = new NotebookFigureBridge(host.messaging);

    const call = rpc('figure.getDisplayList', { handle: 1 });
    const body = host.posted.at(-1)!.body as { rpc: { id: number } };
    host.receive({
      channel: BRIDGE_CHANNEL,
      outputId: 'out-1',
      handle: 1,
      body: { rpc: { id: body.rpc.id, error: { message: 'no kernel' } } },
    });
    await expect(call).rejects.toThrow('no kernel');
  });

  it('re-emits a figure.frame push on the shared bus', () => {
    const host = fakeMessaging();
    bridge = new NotebookFigureBridge(host.messaging);
    const frames: { handle?: number; displayList?: { width?: number } }[] = [];
    const off = onPush('figure.frame', (payload) =>
      frames.push(payload as { handle?: number; displayList?: { width?: number } })
    );

    host.receive({
      channel: BRIDGE_CHANNEL,
      outputId: null,
      handle: 5,
      body: {
        rpc: {
          channel: 'figure.frame',
          payload: { handle: 5, displayList: { width: 10, height: 8, primitives: [] } },
        },
      },
    });
    off();
    expect(frames).toHaveLength(1);
    expect(frames[0].handle).toBe(5);
    expect(frames[0].displayList?.width).toBe(10);
  });

  it('drops a closed figure back to static and posts bye on dispose', () => {
    const host = fakeMessaging();
    bridge = new NotebookFigureBridge(host.messaging);
    const onClosed = vi.fn();
    void bridge.requestLive('out-9', 4, onClosed);

    host.receive({
      channel: BRIDGE_CHANNEL,
      outputId: null,
      handle: 4,
      body: { rpc: { channel: 'figure.closed', payload: { handle: 4 } } },
    });
    expect(onClosed).toHaveBeenCalledTimes(1);

    bridge.bye('out-9');
    expect(host.posted.at(-1)).toMatchObject({ outputId: 'out-9', body: { control: 'bye' } });
  });

  it('settles a pending live request when the output is replaced (bye)', async () => {
    const host = fakeMessaging();
    bridge = new NotebookFigureBridge(host.messaging);
    // A replaced output (slider re-run) byes before init ever arrives.
    const pending = bridge.requestLive('out-7', 2, () => undefined);
    bridge.bye('out-7');
    await expect(pending).resolves.toEqual({ live: false, capabilities: {} });
    expect(host.posted.at(-1)).toMatchObject({ outputId: 'out-7', body: { control: 'bye' } });
  });

  it('ignores a message on another channel', async () => {
    const host = fakeMessaging();
    bridge = new NotebookFigureBridge(host.messaging);
    const call = rpc('figure.getDisplayList', { handle: 2 });
    const body = host.posted.at(-1)!.body as { rpc: { id: number } };
    host.receive({ channel: 'other', outputId: 'out-1', handle: 2, body: { rpc: { id: body.rpc.id, result: 1 } } });
    const settled = await Promise.race([call.then(() => 'done'), Promise.resolve('pending')]);
    expect(settled).toBe('pending');
  });
});
//=============================================================================
