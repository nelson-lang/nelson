//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, expect, test, vi } from 'vitest';
import {
  canUseNFlowRpc,
  emitNFlowPush,
  installNFlowPushBridge,
  installWasmWorkerHost,
  nflowRpc,
  onNFlowPush,
} from './native-rpc.js';
//=============================================================================
const g = globalThis as any;
afterEach(() => {
  vi.useRealTimers();
  delete g.window.webui;
  delete g.window.nflow_rpc;
  delete g.window.rpc;
  delete g.window.__nflowPushHandlers;
  installNFlowPushBridge();
});
//=============================================================================
test('nflowRpc sends one WebUI nflow_rpc envelope and unwraps result', async () => {
  let envelope: any = null;
  g.window.webui = {
    call: vi.fn(async (name: string, payload: string) => {
      expect(name).toBe('nflow_rpc');
      envelope = JSON.parse(payload);
      return JSON.stringify({ id: envelope.id, result: { ok: true } });
    }),
  };

  await expect(nflowRpc('nflow.ping')).resolves.toEqual({ ok: true });
  expect(envelope.method).toBe('nflow.ping');
  expect(envelope.params).toEqual({});
});
//=============================================================================
test('nflowRpc falls back to legacy rpc binding when nflow_rpc is unavailable', async () => {
  let envelope: any = null;
  const call = vi.fn(async (name: string, payload: string) => {
    if (name === 'nflow_rpc') throw new ReferenceError('No binding was found for "nflow_rpc"');
    expect(name).toBe('rpc');
    envelope = JSON.parse(payload);
    return JSON.stringify({ id: envelope.id, result: { ok: true } });
  });
  g.window.webui = { call };

  await expect(nflowRpc('nflow.ping')).resolves.toEqual({ ok: true });
  expect(envelope.method).toBe('nflow.ping');
  expect(call).toHaveBeenCalledTimes(2);
});
//=============================================================================
test('nflowRpc can use generated global nflow_rpc function when webui object is hidden', async () => {
  let envelope: any = null;
  g.window.nflow_rpc = vi.fn(async (payload: string) => {
    envelope = JSON.parse(payload);
    return JSON.stringify({ id: envelope.id, result: { ok: true } });
  });

  await expect(nflowRpc('nflow.ping')).resolves.toEqual({ ok: true });
  expect(envelope.method).toBe('nflow.ping');
  expect(canUseNFlowRpc('nflow.ping')).toBe(true);
});
//=============================================================================
test('canUseNFlowRpc depends on the rpc transport', () => {
  expect(canUseNFlowRpc('nflow.sim.run')).toBe(false);
  g.window.webui = { call: vi.fn() };
  expect(canUseNFlowRpc('nflow.sim.run')).toBe(true);
});
//=============================================================================
test('nflowRpc rejects when the rpc transport is unavailable', async () => {
  vi.useFakeTimers();

  const result = nflowRpc('nflow.sim.run', { diagramJson: '{}' });
  const assertion = expect(result).rejects.toThrow(
    'nflow RPC backend is not available for "nflow.sim.run"'
  );
  await vi.advanceTimersByTimeAsync(10_000);
  await assertion;
});
//=============================================================================
test('nflow push bridge routes payloads by channel', () => {
  const handler = vi.fn();
  const unsubscribe = onNFlowPush('nflow.sim.partial', handler);

  emitNFlowPush('nflow.sim.partial', { currentTime: 1.25 });
  unsubscribe();
  emitNFlowPush('nflow.sim.partial', { currentTime: 2 });

  expect(handler).toHaveBeenCalledTimes(1);
  expect(handler).toHaveBeenCalledWith({ currentTime: 1.25 });
});
//=============================================================================
test('nflow push handlers survive bridge reinstall', () => {
  const handler = vi.fn();
  const unsubscribe = onNFlowPush('nflow.sim.partial', handler);

  installNFlowPushBridge();
  emitNFlowPush('nflow.sim.partial', { currentTime: 3 });
  unsubscribe();

  expect(handler).toHaveBeenCalledWith({ currentTime: 3 });
});
//=============================================================================
test('nflowRpc rejects native error envelopes including unknown methods', async () => {
  g.window.webui = {
    call: vi.fn(async (_name: string, payload: string) => {
      const envelope = JSON.parse(payload);
      return JSON.stringify({
        id: envelope.id,
        error: { message: `unknown nflow RPC method: ${envelope.method}` },
      });
    }),
  };

  await expect(nflowRpc('nflow.nope')).rejects.toThrow('unknown nflow RPC method: nflow.nope');
});
//=============================================================================
test('nflowRpc reports the method and response size for malformed native JSON', async () => {
  g.window.webui = { call: vi.fn(async () => '{"id":1,"result":[') };

  await expect(nflowRpc('nflow.sim.run', { diagramJson: '{}' })).rejects.toThrow(
    /RPC nflow\.sim\.run, 18 bytes/
  );
});
//=============================================================================
test('nflowRpc resolves blob result envelopes by reading chunks then releasing', async () => {
  const call = vi.fn(async (_name: string, payload: string) => {
    const envelope = JSON.parse(payload);
    if (envelope.method === 'nflow.blob.readChunk') {
      const offset = Number(envelope.params.offset || 0);
      const data = offset === 0 ? 'ab' : 'cd';
      return JSON.stringify({
        id: envelope.id,
        result: { data, nextOffset: offset + data.length },
      });
    }
    if (envelope.method === 'nflow.blob.release') {
      return JSON.stringify({ id: envelope.id, result: { success: true } });
    }
    return JSON.stringify({
      id: envelope.id,
      result: { __nflowBlob: true, blobId: 'b1', size: 4, mime: 'text/plain' },
    });
  });
  g.window.webui = { call };

  await expect(nflowRpc('nflow.file.readText', { path: 'large.txt' })).resolves.toBe('abcd');
  expect(call).toHaveBeenCalledTimes(4);
});
//=============================================================================
test('idempotent methods retry transient transport errors', async () => {
  vi.useFakeTimers();
  const call = vi
    .fn()
    .mockRejectedValueOnce(new Error('cold websocket'))
    .mockResolvedValueOnce(JSON.stringify({ id: 1, result: 'ok' }));
  g.window.webui = { call };

  const result = nflowRpc('nflow.router.routeWires', { payload: '{}' });
  await vi.advanceTimersByTimeAsync(200);

  await expect(result).resolves.toBe('ok');
  expect(call).toHaveBeenCalledTimes(2);
});
//=============================================================================
test('non-idempotent methods are not retried', async () => {
  const call = vi.fn().mockRejectedValue(new Error('transport down'));
  g.window.webui = { call };

  await expect(nflowRpc('nflow.sim.run', { diagramJson: '{}' })).rejects.toThrow('transport down');
  expect(call).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('file writes are not retried', async () => {
  const call = vi.fn().mockRejectedValue(new Error('write failed'));
  g.window.webui = { call };

  await expect(
    nflowRpc('nflow.file.writeText', { path: 'x.nflow', content: '{}' })
  ).rejects.toThrow('write failed');
  expect(call).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('nflowRpc timeout stops the retry loop of an idempotent method', async () => {
  vi.useFakeTimers();
  const call = vi.fn().mockRejectedValue(new Error('cold websocket'));
  g.window.webui = { call };

  const result = nflowRpc('nflow.router.routeWires', { payload: '{}' }, { timeoutMs: 500 });
  const assertion = expect(result).rejects.toMatchObject({ name: 'TimeoutError' });
  await vi.advanceTimersByTimeAsync(500);
  await assertion;
  const callsAtTimeout = call.mock.calls.length;
  expect(callsAtTimeout).toBeLessThanOrEqual(4);

  await vi.advanceTimersByTimeAsync(10_000);
  expect(call).toHaveBeenCalledTimes(callsAtTimeout);
});
//=============================================================================
test('nflowRpc rejects with an AbortError when its signal aborts', async () => {
  g.window.webui = { call: vi.fn(() => new Promise<string>(() => undefined)) };
  const controller = new AbortController();

  const result = nflowRpc('nflow.sim.stop', undefined, { signal: controller.signal });
  controller.abort();

  await expect(result).rejects.toMatchObject({ name: 'AbortError' });
});
//=============================================================================
test('nflowRpc timeout bounds the wait for a missing transport', async () => {
  vi.useFakeTimers();

  const result = nflowRpc('nflow.sim.run', { diagramJson: '{}' }, { timeoutMs: 1000 });
  const assertion = expect(result).rejects.toMatchObject({ name: 'TimeoutError' });
  await vi.advanceTimersByTimeAsync(1000);
  await assertion;
});
//=============================================================================
test('WasmWorkerHost exposes simulation without pretending native features exist', async () => {
  const simulateNFlow = vi.fn(async (diagramJson: string) => ({
    status: 'done',
    source: JSON.parse(diagramJson),
  }));
  const interrupt = vi.fn(async () => ({ capabilities: ['nflow'] }));
  const cancelNFlow = vi.fn(async () => ({ cancelled: true, mode: 'cooperative' }));
  const restore = installWasmWorkerHost({ simulateNFlow, interrupt, cancelNFlow });
  try {
    expect(canUseNFlowRpc('nflow.sim.run')).toBe(true);
    expect(canUseNFlowRpc('nflow.file.readText')).toBe(false);
    await expect(nflowRpc('nflow.sim.run', { diagramJson: '{"blocks":[]}' })).resolves.toEqual({
      status: 'done',
      source: { blocks: [] },
    });
    await expect(nflowRpc('nflow.host.mode')).resolves.toEqual({ mode: 'wasm' });
    await expect(nflowRpc('nflow.sim.stop')).resolves.toEqual({ stopped: true });
    expect(simulateNFlow).toHaveBeenCalledWith('{"blocks":[]}');
    expect(cancelNFlow).toHaveBeenCalledOnce();
    expect(interrupt).not.toHaveBeenCalled();
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost restarts an older Worker client when cooperative stop is unavailable', async () => {
  const interrupt = vi.fn(async () => ({ capabilities: ['nflow'] }));
  const restore = installWasmWorkerHost({ simulateNFlow: vi.fn(), interrupt });
  try {
    await expect(nflowRpc('nflow.sim.stop')).resolves.toEqual({ stopped: true });
    expect(interrupt).toHaveBeenCalledOnce();
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost forwards streamed simulation partials to existing push handlers', () => {
  let partialHandler: ((partial: unknown) => void) | undefined;
  const removePartial = vi.fn();
  const client = {
    simulateNFlow: vi.fn(),
    interrupt: vi.fn(),
    onNFlowPartial: vi.fn((handler: (partial: unknown) => void) => {
      partialHandler = handler;
      return removePartial;
    }),
  };
  const received: unknown[] = [];
  const unsubscribe = onNFlowPush('nflow.sim.partial', (partial) => received.push(partial));
  const restore = installWasmWorkerHost(client);
  try {
    partialHandler?.({ status: 'running', sequence: 1, progress: 0.5 });
    expect(received).toEqual([{ status: 'running', sequence: 1, progress: 0.5 }]);
  } finally {
    restore();
    unsubscribe();
  }
  expect(removePartial).toHaveBeenCalledOnce();
});
//=============================================================================
test('WasmWorkerHost routes live nflow.sim.event to the worker client', async () => {
  const sendNFlowSimEvent = vi.fn(() => ({ delivered: true }));
  const client = { simulateNFlow: vi.fn(), interrupt: vi.fn(), sendNFlowSimEvent };
  const restore = installWasmWorkerHost(client);
  try {
    expect(canUseNFlowRpc('nflow.sim.event')).toBe(true);
    const data = JSON.stringify({ type: 'setDashboardValue', id: 'sl_dP', value: 30 });
    await nflowRpc('nflow.sim.event', { name: 'DashboardControl', data });
    expect(sendNFlowSimEvent).toHaveBeenCalledWith('DashboardControl', data);
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost routes nflow.callback.invoke to the worker client', async () => {
  const sendNFlowSimCallback = vi.fn(() => ({ delivered: true }));
  const client = { simulateNFlow: vi.fn(), interrupt: vi.fn(), sendNFlowSimCallback };
  const restore = installWasmWorkerHost(client);
  try {
    expect(canUseNFlowRpc('nflow.callback.invoke')).toBe(true);
    await nflowRpc('nflow.callback.invoke', { code: 'x = 1;', blockId: 'cb1' });
    expect(sendNFlowSimCallback).toHaveBeenCalledWith('x = 1;', 'cb1');
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost runs a gallery companion script on the engine and opens the model it asks for', async () => {
  const evaluate = vi.fn(async () => ({
    stdout: '',
    uiActions: [
      {
        type: 'open-nflow',
        path: '/modules/nflow_blocks/examples/workspace/From_Workspace_Demo.nflow',
      },
    ],
  }));
  const sendNFlowSimCallback = vi.fn(() => ({ delivered: true }));
  const client = { simulateNFlow: vi.fn(), interrupt: vi.fn(), evaluate, sendNFlowSimCallback };
  const examples = {
    listExamples: vi.fn(async () => ({
      groups: [
        {
          examples: [
            { kind: 'script', file: 'nflow-example:/workspace/From_Workspace_Demo.m' },
            { kind: 'model', file: 'nflow-example:/workspace/From_Workspace_Demo.nflow' },
          ],
        },
      ],
    })),
    readText: vi.fn(async () => "simin = [0 1];\nopen_system('x');\n"),
  };
  const openModel = vi.fn();
  const restore = installWasmWorkerHost(client, examples, undefined, undefined, openModel);
  try {
    await nflowRpc('nflow.callback.invoke', {
      code: "run('nflow-example:/workspace/From_Workspace_Demo.m')",
      blockId: '',
    });
    expect(examples.readText).toHaveBeenCalledWith(
      'nflow-example:/workspace/From_Workspace_Demo.m'
    );
    expect(evaluate).toHaveBeenCalledTimes(1);
    expect(String(evaluate.mock.calls[0][0])).toContain('simin = [0 1];');
    expect(openModel).toHaveBeenCalledWith('nflow-example:/workspace/From_Workspace_Demo.nflow');
    expect(sendNFlowSimCallback).not.toHaveBeenCalled();
    // Ordinary dashboard callbacks keep the live-simulation path.
    await nflowRpc('nflow.callback.invoke', { code: 'x = 1;', blockId: 'cb1' });
    expect(sendNFlowSimCallback).toHaveBeenCalledWith('x = 1;', 'cb1');
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost serves the editor file dialogs from the engine filesystem', async () => {
  const files = new Map<string, Uint8Array>([
    ['/workspace/models/plant.nflow', new TextEncoder().encode('{"name":"plant"}')],
    ['/workspace/notes.txt', new TextEncoder().encode('hi')],
  ]);
  const client = {
    simulateNFlow: vi.fn(),
    interrupt: vi.fn(),
    readFile: vi.fn(async (path: string) => {
      const bytes = files.get(path);
      if (!bytes) throw new Error(`Virtual file does not exist: ${path}`);
      return bytes;
    }),
    writeFile: vi.fn(async (path: string, data: Uint8Array) => {
      files.set(path, data);
      return { path, bytes: data.byteLength };
    }),
    listFiles: vi.fn(async () => [...files.keys()]),
    listDirectories: vi.fn(async () => ['/workspace', '/workspace/models']),
  };
  const restore = installWasmWorkerHost(client);
  try {
    for (const method of [
      'nflow.file.readText',
      'nflow.file.writeText',
      'nflow.fs.list',
      'nflow.fs.dialogStart',
      'nflow.fs.dialogAccept',
    ]) {
      expect(canUseNFlowRpc(method)).toBe(true);
    }
    const listed = JSON.parse(await nflowRpc<string>('nflow.fs.list', { path: '/workspace' }));
    expect(listed).toEqual({
      cwd: '/workspace',
      entries: [
        { name: 'models', path: '/workspace/models', isDir: true, size: 0 },
        { name: 'notes.txt', path: '/workspace/notes.txt', isDir: false, size: 0 },
      ],
    });
    // Paths outside the engine roots fall back to /workspace instead of leaking.
    expect(JSON.parse(await nflowRpc<string>('nflow.fs.list', { path: '/etc' })).cwd).toBe(
      '/workspace'
    );
    const read = JSON.parse(
      await nflowRpc<string>('nflow.file.readText', { path: '/workspace/models/plant.nflow' })
    );
    expect(read).toEqual({ success: true, content: '{"name":"plant"}' });
    const missing = JSON.parse(
      await nflowRpc<string>('nflow.file.readText', { path: '/workspace/nope.nflow' })
    );
    expect(missing.success).toBe(false);

    const written = JSON.parse(
      await nflowRpc<string>('nflow.file.writeText', {
        path: 'saved/new.nflow',
        content: '{"name":"new"}',
      })
    );
    expect(written).toEqual({ success: true });
    expect(client.writeFile).toHaveBeenCalledWith('/workspace/saved/new.nflow', expect.anything());
    expect(new TextDecoder().decode(files.get('/workspace/saved/new.nflow'))).toBe(
      '{"name":"new"}'
    );
    const binary = JSON.parse(
      await nflowRpc<string>('nflow.file.writeText', {
        path: '/workspace/shot.png',
        content: `base64:${btoa('PNG')}`,
      })
    );
    expect(binary).toEqual({ success: true });
    expect(new TextDecoder().decode(files.get('/workspace/shot.png'))).toBe('PNG');
    const refused = JSON.parse(
      await nflowRpc<string>('nflow.file.writeText', { path: '/modules/x.nflow', content: '{}' })
    );
    expect(refused.success).toBe(false);

    expect(JSON.parse(await nflowRpc<string>('nflow.fs.dialogStart', { mode: 'save' }))).toEqual({
      directory: '/workspace',
    });
    expect(
      JSON.parse(
        await nflowRpc<string>('nflow.fs.dialogAccept', {
          mode: 'save',
          path: '/workspace/models/plant.nflow',
        })
      )
    ).toEqual({ ok: true });
    expect(JSON.parse(await nflowRpc<string>('nflow.fs.dialogStart', { mode: 'save' }))).toEqual({
      directory: '/workspace/models',
    });
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost maps example paths used by scripts to their packaged copies', async () => {
  const client = {
    simulateNFlow: vi.fn(),
    interrupt: vi.fn(),
    readFile: vi.fn(async () => {
      throw new Error('not mounted');
    }),
    writeFile: vi.fn(),
    listFiles: vi.fn(async () => []),
    listDirectories: vi.fn(async () => []),
  };
  const examples = {
    listExamples: vi.fn(async () => ({
      groups: [
        {
          examples: [{ kind: 'model', file: 'nflow-example:/workspace/From_Workspace_Demo.nflow' }],
        },
      ],
    })),
    readText: vi.fn(async () => '{"name":"From Workspace Demo"}'),
  };
  const restore = installWasmWorkerHost(client, examples);
  try {
    const read = JSON.parse(
      await nflowRpc<string>('nflow.file.readText', {
        path: '/modules/nflow_blocks/examples/workspace/From_Workspace_Demo.nflow',
      })
    );
    expect(read).toEqual({ success: true, content: '{"name":"From Workspace Demo"}' });
    expect(examples.readText).toHaveBeenCalledWith(
      'nflow-example:/workspace/From_Workspace_Demo.nflow'
    );
    expect(client.readFile).not.toHaveBeenCalled();
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost generates code on the shared engine below /workspace', async () => {
  const written = new Map<string, string>();
  const evaluate = vi.fn(
    async (code: string, onOutput?: (o: { stream?: string; text?: string }) => void) => {
      expect(code).toContain(
        "nflow_codegenerate('/workspace/.nflow-codegen/My_Plant.nflow', '/workspace/out', true, 'C');"
      );
      onOutput?.({ stream: 'stdout', text: '__NFLOW_CODEGEN_WARNING__RK4 downgraded to Euler\n' });
      return { stdout: '' };
    }
  );
  const client = {
    simulateNFlow: vi.fn(),
    interrupt: vi.fn(),
    evaluate,
    readFile: vi.fn(async () => new Uint8Array()),
    writeFile: vi.fn(async (path: string, data: Uint8Array) => {
      written.set(path, new TextDecoder().decode(data));
      return { path, bytes: data.byteLength };
    }),
    listFiles: vi.fn(async () => []),
    listDirectories: vi.fn(async () => []),
  };
  const restore = installWasmWorkerHost(client);
  try {
    expect(canUseNFlowRpc('nflow.codegen.generateToDir')).toBe(true);
    const diagramJson = JSON.stringify({ name: 'My Plant', blocks: [], connections: [] });
    const raw = await nflowRpc<string>('nflow.codegen.generateToDir', {
      diagramJson,
      optionsJson: JSON.stringify({ lang: 'c', includeMain: true, build: false }),
      destinationDirectory: '/workspace/out',
    });
    expect(JSON.parse(raw)).toEqual({
      success: true,
      dir: '/workspace/out/My_Plant',
      warnings: ['RK4 downgraded to Euler'],
    });
    expect(written.get('/workspace/.nflow-codegen/My_Plant.nflow')).toBe(diagramJson);

    const withBuild = JSON.parse(
      await nflowRpc<string>('nflow.codegen.generateToDir', {
        diagramJson,
        optionsJson: JSON.stringify({ lang: 'c', includeMain: true, build: true }),
        destinationDirectory: '/workspace/out',
      })
    );
    // Building is not offered in the browser: a stale `build` flag is ignored.
    expect(withBuild).toEqual({
      success: true,
      dir: '/workspace/out/My_Plant',
      warnings: ['RK4 downgraded to Euler'],
    });

    evaluate.mockRejectedValueOnce(new Error('Code generation: unsupported block'));
    const failed = JSON.parse(
      await nflowRpc<string>('nflow.codegen.generateToDir', {
        diagramJson,
        optionsJson: '{}',
        destinationDirectory: '/workspace/out',
      })
    );
    expect(failed).toEqual({ success: false, error: 'Code generation: unsupported block' });
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost opens block documentation through the shared doc command', async () => {
  const evaluate = vi.fn(async () => ({
    stdout: 'https://nelson.example/docs/en_US/v1.0.0/index.html?open=block%3Again.html\n',
  }));
  const openHelp = vi.fn();
  const restore = installWasmWorkerHost(
    { simulateNFlow: vi.fn(), interrupt: vi.fn(), evaluate },
    undefined,
    openHelp
  );
  try {
    expect(canUseNFlowRpc('nflow.help.open')).toBe(true);
    await expect(nflowRpc('nflow.help.open', { topic: 'block:gain' })).resolves.toMatchObject({
      success: true,
      topic: 'block:gain',
    });
    expect(evaluate).toHaveBeenCalledTimes(2);
    expect(evaluate.mock.calls[0][0]).toContain("doc('block:gain');");
    expect(openHelp).toHaveBeenCalledWith(
      'https://nelson.example/docs/en_US/v1.0.0/index.html?open=block%3Again.html'
    );
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost preserves the canonical block page while calling doc', async () => {
  const evaluate = vi.fn(async () => ({
    stdout:
      'https://nelson.example/docs/en_US/v1.0.0/index.html?open=.%2Fnflow_blocks%2Fsource%2Fgain.html\n',
  }));
  const openHelp = vi.fn();
  const restore = installWasmWorkerHost(
    { simulateNFlow: vi.fn(), interrupt: vi.fn(), evaluate },
    undefined,
    openHelp
  );
  try {
    await expect(
      nflowRpc('nflow.help.open', {
        topic: 'block:gain',
        page: './nflow_blocks/source/gain.html',
      })
    ).resolves.toMatchObject({ success: true });
    expect(evaluate.mock.calls[0][0]).toContain(
      "setenv('NELSON_DOC_BLOCK_PAGE', './nflow_blocks/source/gain.html');"
    );
    expect(evaluate.mock.calls[0][0]).toContain(
      "setenv('NELSON_DOC_BLOCK_PAGE', nflowWasmPreviousDocPage);"
    );
    expect(openHelp).toHaveBeenCalledWith(
      'https://nelson.example/docs/en_US/v1.0.0/index.html?open=.%2Fnflow_blocks%2Fsource%2Fgain.html'
    );
  } finally {
    restore();
  }
});
//=============================================================================
test('WasmWorkerHost exposes packaged examples as read-only files', async () => {
  const client = { simulateNFlow: vi.fn(), interrupt: vi.fn() };
  const examples = {
    listExamples: vi.fn(async () => ({
      examples: [{ path: 'nflow-example:/causal/Gain_Demo.nflow', name: 'Gain demo' }],
    })),
    readText: vi.fn(async () => '{"name":"Gain demo","blocks":[]}'),
  };
  const restore = installWasmWorkerHost(client, examples);
  try {
    expect(canUseNFlowRpc('nflow.examples.list')).toBe(true);
    expect(canUseNFlowRpc('nflow.file.readText')).toBe(true);
    await expect(nflowRpc('nflow.examples.list')).resolves.toEqual({
      examples: [{ path: 'nflow-example:/causal/Gain_Demo.nflow', name: 'Gain demo' }],
    });
    const raw = await nflowRpc<string>('nflow.file.readText', {
      path: 'nflow-example:/causal/Gain_Demo.nflow',
    });
    expect(JSON.parse(raw)).toEqual({
      success: true,
      content: '{"name":"Gain demo","blocks":[]}',
    });
    expect(examples.readText).toHaveBeenCalledWith('nflow-example:/causal/Gain_Demo.nflow');
  } finally {
    restore();
  }
});
//=============================================================================
test('removing WasmWorkerHost restores native transport discovery', () => {
  const restore = installWasmWorkerHost({ simulateNFlow: vi.fn(), interrupt: vi.fn() });
  expect(canUseNFlowRpc('nflow.sim.run')).toBe(true);
  restore();
  expect(canUseNFlowRpc('nflow.sim.run')).toBe(false);
  g.window.webui = { call: vi.fn() };
  expect(canUseNFlowRpc('nflow.sim.run')).toBe(true);
});
//=============================================================================
