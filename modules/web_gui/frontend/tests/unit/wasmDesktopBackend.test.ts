//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import type {
  NelsonEvaluationResult,
  NelsonFigureFrame,
  NelsonWorkerClientApi,
} from '@nelson/wasm-client';
import { WasmDesktopBackend } from '../../src/wasm/WasmDesktopBackend';
import { onPush } from '../../src/rpc/client';
import type { RuntimeCapabilityManifest } from '../../src/wasm/runtimeManifest';
import { getWasmFigureProfile, resetWasmFigureProfile } from '../../src/wasm/wasmFigureProfile';
//=============================================================================
function runtimeCapabilities(maxVirtualFileBytes = 32 * 1024 * 1024): RuntimeCapabilityManifest {
  return {
    formatVersion: 1,
    runtime: 'webassembly',
    profile: 'portable-nflow',
    modules: ['core', 'nflow_engine'],
    capabilities: ['nflow', 'package-manager-core', 'worker'],
    features: {
      blas: true,
      controlSystem: true,
      lapack: true,
      nativeWasmExceptions: false,
      nflow: true,
      odeSolvers: true,
      packageManager: true,
      plots: true,
      slicot: true,
      threadedWasm: false,
      virtualFilesystem: true,
      worker: true,
    },
    limits: {
      maxNflowJsonBytes: 8 * 1024 * 1024,
      maxSourceBytes: 4 * 1024 * 1024,
      maxVariableBlockCells: 1_000_000,
      maxVirtualFileBytes,
      maxWasmBytes: 128 * 1024 * 1024,
    },
    unavailable: [],
  };
}
//=============================================================================
function runtimeResourceBundle(entries: Record<string, string>): Uint8Array {
  const encoder = new TextEncoder();
  const chunks: Uint8Array[] = [];
  const files: Array<{ file: string; offset: number; bytes: number }> = [];
  let offset = 0;
  for (const [file, source] of Object.entries(entries)) {
    const bytes = encoder.encode(source);
    chunks.push(bytes);
    files.push({ file, offset, bytes: bytes.byteLength });
    offset += bytes.byteLength;
  }
  const header = encoder.encode(JSON.stringify({ formatVersion: 1, files }));
  const output = new Uint8Array(12 + header.byteLength + offset);
  output.set(encoder.encode('NLRPACK1'), 0);
  new DataView(output.buffer).setUint32(8, header.byteLength, true);
  output.set(header, 12);
  let payloadOffset = 12 + header.byteLength;
  for (const chunk of chunks) {
    output.set(chunk, payloadOffset);
    payloadOffset += chunk.byteLength;
  }
  return output;
}
//=============================================================================
function clientFixture(): {
  client: NelsonWorkerClientApi;
  sendFigure: (frame: NelsonFigureFrame) => void;
  sendDebuggerState: (state: {
    running: boolean;
    currentFile: string | null;
    currentLine: number | null;
    stack: Array<{ file: string; line: number; function: string }>;
  }) => void;
} {
  let figureHandler: (frame: NelsonFigureFrame) => void = () => undefined;
  let debuggerStateHandler: (state: {
    running: boolean;
    currentFile: string | null;
    currentLine: number | null;
    stack: Array<{ file: string; line: number; function: string }>;
  }) => void = () => undefined;
  const files = new Map<string, Uint8Array>();
  const writeFile = vi.fn(async (path: string, data: Uint8Array) => {
    files.set(path, data);
    return { path, bytes: data.byteLength };
  });
  return {
    client: {
      evaluate: vi.fn(async () => ({
        exitCode: 0,
        stdout: '3',
        stderr: '',
        figures: [],
      })),
      queueCommand: vi.fn(() => ({ queued: true as const })),
      simulateNFlow: vi.fn(),
      writeFile,
      writeFiles: vi.fn(async (entries: Array<{ path: string; data: Uint8Array }>) => {
        for (const entry of entries) {
          await writeFile(entry.path, entry.data);
        }
        return {
          files: entries.length,
          bytes: entries.reduce((sum, entry) => sum + entry.data.byteLength, 0),
        };
      }),
      readFile: vi.fn(async (path: string) => files.get(path) ?? new Uint8Array()),
      listFiles: vi.fn(async () => [...files.keys()]),
      listDirectories: vi.fn(async () => []),
      createDirectory: vi.fn(async (path: string) => ({ ok: true as const, path })),
      renamePath: vi.fn(async (from: string, to: string) => {
        const data = files.get(from);
        if (data) {
          files.delete(from);
          files.set(to, data);
        }
        return { ok: true as const, path: to };
      }),
      deletePath: vi.fn(async (path: string) => {
        files.delete(path);
        return { ok: true as const };
      }),
      listWorkspace: vi.fn(async () => [
        { name: 'answer', class: 'double', size: [1, 1], bytes: 8, sparse: false },
      ]),
      openVariable: vi.fn(async (name: string) => ({
        name,
        className: 'double',
        rows: 1,
        cols: 1,
        columns: ['1'],
        editable: true,
        kind: 'matrix',
      })),
      getVariableBlock: vi.fn(async () => ({ row0: 0, col0: 0, cells: [[42]] })),
      replaceVariable: vi.fn(async () => ({ ok: true as const })),
      setNestedVariable: vi.fn(async () => ({ ok: true as const })),
      complete: vi.fn(async () => ({
        prefix: 'an',
        builtin: ['and'],
        macros: [],
        variables: ['answer'],
        files: [],
        fields: [],
        properties: ['Angle'],
        methods: [],
      })),
      analyzeCode: vi.fn(async () => ({
        diagnostics: [
          {
            severity: 'warning',
            message: 'Example issue',
            ruleName: 'example-rule',
            checkId: 'L001',
            category: 'style',
            helpUri: '',
            lineStart: 1,
            lineEnd: 1,
            columnStart: 1,
            columnEnd: 2,
            fixes: [],
          },
        ],
      })),
      debuggerState: vi.fn(async () => ({
        running: false,
        currentFile: null,
        currentLine: null,
        stack: [],
      })),
      debuggerGetBreakpoints: vi.fn(async (file: string) => ({ file, lines: [3] })),
      debuggerToggleBreakpoint: vi.fn(async (file: string, line: number) => ({
        file,
        lines: [line],
      })),
      setFigureView: vi.fn(async (handle: number, azimuth: number, elevation: number) => ({
        handle,
        width: 640,
        height: 480,
        primitives: [],
        is3d: true,
        view: [azimuth, elevation],
      })),
      figureAction: vi.fn(async () => ({ state: { handle: 0, axes: [] } })),
      setFigureSize: vi.fn(async () => ({ ok: true, figures: [] })),
      panFigure: vi.fn(async () => ({ ok: true, figures: [] })),
      closeFigure: vi.fn(async () => ({ ok: true, figures: [] })),
      getFigureImage: vi.fn(async (handle: number) => ({
        handle,
        image: 'data:image/png;base64,iVBORw0KGgo=',
      })),
      saveFigure: vi.fn(async (_handle: number, path: string) => ({
        ok: true as const,
        path,
      })),
      sendFigureKeyEvent: vi.fn(async () => ({ ok: true, figures: [] })),
      sendFigureMouseEvent: vi.fn(async () => ({ ok: true, motion: true, figures: [] })),
      sendUIControlAction: vi.fn(async () => ({ ok: true, figures: [] })),
      reset: vi.fn(async () => ({ reset: true as const })),
      interrupt: vi.fn(async () => ({ capabilities: [] })),
      onFigure: vi.fn((handler: (frame: NelsonFigureFrame) => void) => {
        figureHandler = handler;
        return () => undefined;
      }),
      onAudioAction: vi.fn(() => () => undefined),
      onNFlowPartial: vi.fn(() => () => undefined),
      onDebuggerState: vi.fn((handler) => {
        debuggerStateHandler = handler;
        return () => undefined;
      }),
      onInputRequest: vi.fn(() => () => undefined),
      onProgress: vi.fn(() => () => undefined),
      terminate: vi.fn(),
    },
    sendFigure: (frame) => figureHandler(frame),
    sendDebuggerState: (state) => debuggerStateHandler(state),
  };
}
//=============================================================================
const signal = new AbortController().signal;
describe('WebAssembly desktop backend', () => {
  it('uses the portable shared code analyzer for editor buffers', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);

    const result = await backend.handle(
      'code.analyze',
      { path: '/workspace/demo.m', source: 'x=1' },
      signal
    );

    expect(fixture.client.analyzeCode).toHaveBeenCalledWith('/workspace/demo.m', 'x=1');
    expect(result).toMatchObject({ diagnostics: [{ checkId: 'L001' }] });
    backend.dispose();
  });

  it('uses the portable debugger bridge for editor breakpoints', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(
      backend.handle('debugger.getBreakpoints', { file: '/workspace/demo.m' }, signal)
    ).resolves.toEqual({ file: '/workspace/demo.m', lines: [3] });
    await expect(
      backend.handle('debugger.toggleBreakpoint', { file: '/workspace/demo.m', line: 7 }, signal)
    ).resolves.toEqual({ file: '/workspace/demo.m', lines: [7] });
    await expect(backend.handle('debugger.state', {}, signal)).resolves.toMatchObject({
      running: false,
    });

    expect(fixture.client.debuggerGetBreakpoints).toHaveBeenCalledWith('/workspace/demo.m');
    expect(fixture.client.debuggerToggleBreakpoint).toHaveBeenCalledWith('/workspace/demo.m', 7);
    expect(fixture.client.debuggerState).toHaveBeenCalled();
    backend.dispose();
  });

  it('publishes debugger state pushed by the WebAssembly worker', () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    const states: unknown[] = [];
    const off = onPush('debugger.state', (state) => states.push(state));

    fixture.sendDebuggerState({
      running: true,
      currentFile: '/workspace/demo.m',
      currentLine: 4,
      stack: [{ file: '/workspace/demo.m', line: 4, function: 'demo' }],
    });

    expect(states).toEqual([
      {
        running: true,
        currentFile: '/workspace/demo.m',
        currentLine: 4,
        stack: [{ file: '/workspace/demo.m', line: 4, function: 'demo' }],
      },
    ]);
    off();
    backend.dispose();
  });

  it('evaluates commands and publishes terminal output and a prompt', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    const backend = new WasmDesktopBackend(fixture.client);

    await backend.handle('term.input', { line: 'disp(1 + 2);' }, signal);

    expect(fixture.client.evaluate).toHaveBeenCalledWith('disp(1 + 2);', expect.any(Function));
    expect(pushes).toContainEqual({
      channel: 'term.busy',
      payload: expect.objectContaining({ token: expect.any(Number) }),
    });
    expect(pushes).toContainEqual({ channel: 'term.output', payload: { text: '3\n' } });
    expect(pushes.at(-1)).toMatchObject({
      channel: 'term.prompt',
      payload: { prompt: '>> ', token: expect.any(Number) },
    });
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('publishes evaluator output before the final result', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(async (_code, onOutput) => {
      onOutput?.({ stream: 'stdout', text: 'first\n' });
      onOutput?.({ stream: 'stdout', text: 'second\n' });
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    });
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    await backend.handle('term.input', { line: "disp('first'); disp('second');" }, signal);

    const outputIndexes = pushes
      .map((push, index) => ({ ...push, index }))
      .filter((push) => push.channel === 'term.output');
    const promptIndex = pushes.findIndex((push) => push.channel === 'term.prompt');
    expect(outputIndexes.map((push) => push.payload)).toEqual([
      { text: 'first\n' },
      { text: 'second\n' },
    ]);
    expect(outputIndexes.every((push) => push.index < promptIndex)).toBe(true);
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('publishes terminal clear events from the portable evaluator', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(async (_code, onOutput) => {
      onOutput?.({ stream: 'stdout', text: 'before\n' });
      onOutput?.({ stream: 'control', clear: true });
      onOutput?.({ stream: 'stdout', text: 'after\n' });
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    });
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    await backend.handle('term.input', { line: 'disp(1); clc; disp(2);' }, signal);

    expect(pushes.map((push) => push.channel)).toContain('term.clear');
    expect(pushes).toContainEqual({ channel: 'term.output', payload: { text: 'before\n' } });
    expect(pushes).toContainEqual({ channel: 'term.output', payload: { text: 'after\n' } });
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('publishes rich terminal events from the portable evaluator', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(async (_code, onOutput) => {
      onOutput?.({ stream: 'control', html: '<b>html</b>' });
      onOutput?.({ stream: 'control', expandable: { id: 9, text: 'expanded text' } });
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    });
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    await backend.handle('term.input', { line: 'rich output' }, signal);

    expect(pushes).toContainEqual({ channel: 'html.show', payload: { html: '<b>html</b>' } });
    expect(pushes).toContainEqual({
      channel: 'term.expandable',
      payload: { id: 9, text: 'expanded text' },
    });
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('routes a command submitted during an evaluation through the engine command queue', async () => {
    const fixture = clientFixture();
    let finishEvaluation: (() => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise((resolve) => {
          finishEvaluation = () => resolve({ exitCode: 0, stdout: '', stderr: '', figures: [] });
        })
    );
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    const running = backend.handle('term.input', { line: 'animated_demo()' }, signal);
    await Promise.resolve();
    await expect(backend.handle('term.input', { line: 'surf(peaks)' }, signal)).resolves.toEqual({
      accepted: true,
    });

    expect(fixture.client.queueCommand).toHaveBeenCalledWith('surf(peaks)');
    expect(fixture.client.evaluate).toHaveBeenCalledTimes(1);
    finishEvaluation?.();
    await running;
    backend.dispose();
  });

  it('does not synchronize a fresh prompt while the evaluator is busy', async () => {
    const fixture = clientFixture();
    let finishEvaluation: (() => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise((resolve) => {
          finishEvaluation = () => resolve({ exitCode: 0, stdout: '', stderr: '', figures: [] });
        })
    );
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    const running = backend.handle('term.input', { line: 'animated_demo()' }, signal);
    await Promise.resolve();
    await expect(backend.handle('term.sync', {}, signal)).resolves.toEqual({ ok: true });

    finishEvaluation?.();
    await running;
    backend.dispose();
  });

  it('does not synchronize a fresh prompt while an external run is being acknowledged', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });
    (
      backend as unknown as {
        exampleRunAcknowledgement: (() => void) | null;
      }
    ).exampleRunAcknowledgement = () => undefined;

    await expect(backend.handle('term.sync', {}, signal)).resolves.toEqual({ ok: true });

    (
      backend as unknown as {
        exampleRunAcknowledgement: (() => void) | null;
      }
    ).exampleRunAcknowledgement = null;
    backend.dispose();
  });

  it('hides the prompt as soon as an examples run is requested', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    let resolveFetch: ((response: Response) => void) | undefined;
    vi.stubGlobal(
      'fetch',
      vi.fn(
        () =>
          new Promise<Response>((resolve) => {
            resolveFetch = resolve;
          })
      )
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html',
      historyStorage: null,
    });
    const dispatch = (
      backend as unknown as {
        dispatchExamplesRequest(method: string, params: Record<string, unknown>): Promise<unknown>;
      }
    ).dispatchExamplesRequest.bind(backend);

    const run = dispatch('run', { command: 'animated_demo()' });
    await vi.waitFor(() => expect(pushes.some((push) => push.channel === 'term.busy')).toBe(true));
    await expect(backend.handle('term.sync', {}, signal)).resolves.toEqual({ ok: true });
    expect(fixture.client.evaluate).not.toHaveBeenCalled();

    resolveFetch?.(
      new Response(
        JSON.stringify({
          groups: [{ examples: [{ kind: 'command', command: 'animated_demo()' }] }],
          diagnostics: [],
          locale: 'en_US',
        })
      )
    );
    await expect(run).resolves.toEqual({ accepted: true });
    expect(fixture.client.evaluate).toHaveBeenCalledWith('animated_demo()', expect.any(Function));
    backend.dispose();
    window.__nelsonPush = previousPush;
    vi.unstubAllGlobals();
  });

  it('keeps an examples run busy until Ctrl+C stops the shared interpreter', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async () =>
          new Response(
            JSON.stringify({
              groups: [{ examples: [{ kind: 'command', command: 'animated_demo()' }] }],
              diagnostics: [],
              locale: 'en_US',
            })
          )
      )
    );
    let rejectEvaluation: ((error: Error) => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise((_, reject) => {
          rejectEvaluation = reject;
        })
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html',
      historyStorage: null,
    });
    const dispatch = (
      backend as unknown as {
        dispatchExamplesRequest(method: string, params: Record<string, unknown>): Promise<unknown>;
      }
    ).dispatchExamplesRequest.bind(backend);

    const run = dispatch('run', { command: 'animated_demo()' });
    await vi.waitFor(() =>
      expect(fixture.client.evaluate).toHaveBeenCalledWith('animated_demo()', expect.any(Function))
    );
    await expect(backend.handle('term.sync', {}, signal)).resolves.toEqual({ ok: true });
    expect(pushes.filter((push) => push.channel === 'term.prompt')).toHaveLength(0);

    await backend.handle('term.interrupt', { busy: true }, signal);
    expect(fixture.client.interrupt).toHaveBeenCalledTimes(1);
    expect(pushes.filter((push) => push.channel === 'term.prompt')).toHaveLength(1);
    expect(pushes).toContainEqual({
      channel: 'term.output',
      payload: { text: '\u001b[31mNelson evaluation interrupted\u001b[0m\n' },
    });

    rejectEvaluation?.(new Error('Nelson evaluation interrupted'));
    await expect(run).resolves.toEqual({ accepted: true });
    expect(pushes.filter((push) => push.channel === 'term.prompt')).toHaveLength(1);
    backend.dispose();
    window.__nelsonPush = previousPush;
    vi.unstubAllGlobals();
  });

  it('queues uicontrol actions into the shared interpreter while an animation is running', async () => {
    const fixture = clientFixture();
    let finishEvaluation: (() => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise((resolve) => {
          finishEvaluation = () => resolve({ exitCode: 0, stdout: '', stderr: '', figures: [] });
        })
    );
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    const running = backend.handle('term.input', { line: 'fluid_simulation_2d' }, signal);
    await Promise.resolve();
    await expect(
      backend.handle('uicontrol.action', { handle: 42, value: 1, text: 'start' }, signal)
    ).resolves.toEqual({ ok: true, figures: [] });

    expect(fixture.client.queueCommand).toHaveBeenCalledWith(
      "__web_uicontrol_action__(42, 1, 'start');"
    );
    expect(fixture.client.sendUIControlAction).not.toHaveBeenCalled();

    finishEvaluation?.();
    await running;
    backend.dispose();
  });

  it('publishes exactly one prompt from the interrupt path, not the stale evaluation path', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    let rejectEvaluation: ((error: Error) => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise((_, reject) => {
          rejectEvaluation = reject;
        })
    );
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    const running = backend.handle('term.input', { line: 'animated_demo()' }, signal);
    await Promise.resolve();
    await backend.handle('term.interrupt', {}, signal);

    expect(pushes.filter((push) => push.channel === 'term.prompt')).toHaveLength(1);
    expect(pushes).toContainEqual({
      channel: 'term.output',
      payload: { text: '\u001b[31mNelson evaluation interrupted\u001b[0m\n' },
    });

    rejectEvaluation?.(new Error('Nelson evaluation interrupted'));
    await running;

    expect(pushes.filter((push) => push.channel === 'term.prompt')).toHaveLength(1);
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('reports an interrupt when the Command Window requested it while visibly busy', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    await backend.handle('term.interrupt', { busy: true }, signal);

    expect(pushes).toContainEqual({
      channel: 'term.output',
      payload: { text: '\u001b[31mNelson evaluation interrupted\u001b[0m\n' },
    });
    expect(pushes.filter((push) => push.channel === 'term.prompt')).toHaveLength(1);
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('persists command history across backend instances', async () => {
    const values = new Map<string, string>();
    const storage = {
      getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => values.set(key, value),
      removeItem: (key: string) => values.delete(key),
    };
    const first = clientFixture();
    const firstBackend = new WasmDesktopBackend(first.client, { historyStorage: storage });
    await firstBackend.handle('term.input', { line: 'persistentAnswer = 42;' }, signal);
    firstBackend.dispose();

    const second = clientFixture();
    const secondBackend = new WasmDesktopBackend(second.client, { historyStorage: storage });
    await expect(secondBackend.handle('history.list', {}, signal)).resolves.toMatchObject({
      entries: [
        expect.stringMatching(/^%% -- \d{2}-\d{2}-\d{4} \d{2}:\d{2}:\d{2} -- %%$/),
        'persistentAnswer = 42;',
        expect.stringMatching(/^%% -- \d{2}-\d{2}-\d{4} \d{2}:\d{2}:\d{2} -- %%$/),
      ],
    });
    await secondBackend.handle('history.clear', {}, signal);
    await expect(secondBackend.handle('history.list', {}, signal)).resolves.toMatchObject({
      entries: [expect.stringMatching(/^%% -- \d{2}-\d{2}-\d{4} \d{2}:\d{2}:\d{2} -- %%$/)],
    });
    secondBackend.dispose();
  });

  it('persists desktop preferences through the browser store', async () => {
    const fixture = clientFixture();
    const values = new Map<string, string>();
    const storage = {
      getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => values.set(key, value),
      removeItem: (key: string) => values.delete(key),
    };
    const first = new WasmDesktopBackend(fixture.client, { preferencesStorage: storage });
    await expect(
      first.handle(
        'prefs.save',
        {
          schema: 1,
          values: {
            'nelson-webview.theme': 'dark',
            'nelson-webview.editor.tabSize': '2',
          },
        },
        signal
      )
    ).resolves.toEqual({
      ok: true,
      path: 'browser:localStorage/nelson-webview.wasm.preferences.v1',
    });
    first.dispose();

    const second = new WasmDesktopBackend(fixture.client, { preferencesStorage: storage });
    await expect(second.handle('prefs.load', {}, signal)).resolves.toEqual({
      schema: 1,
      values: {
        'nelson-webview.theme': 'dark',
        'nelson-webview.editor.tabSize': '2',
      },
      path: 'browser:localStorage/nelson-webview.wasm.preferences.v1',
    });
    expect(fixture.client.writeFile).not.toHaveBeenCalled();
    second.dispose();
  });

  it('controls, reads, and exports the Nelson profiler through the shared panel contract', async () => {
    const fixture = clientFixture();
    vi.mocked(fixture.client.evaluate).mockImplementation(async (code, onOutput) => {
      if (code.includes('__NELSON_PROFILE_CONTROL_BEGIN__')) {
        onOutput?.({
          stream: 'stdout',
          text: '__NELSON_PROFILE_CONTROL_BEGIN__true__NELSON_PROFILE_CONTROL_END__',
        });
        return {
          exitCode: 0,
          stdout: '',
          stderr: '',
          figures: [],
        };
      }
      if (code.includes('__NELSON_PROFILE_RESULT_BEGIN__')) {
        return {
          exitCode: 0,
          stdout:
            '__NELSON_PROFILE_RESULT_BEGIN__' +
            JSON.stringify({
              rows: {
                FunctionName: 'sin',
                Filename: '/modules/elementary_functions/functions/example.m',
                LinePosition: 4,
                NumCalls: 3,
                TotalTime: 0.03,
                PerCall: 0.01,
              },
              running: false,
            }) +
            '__NELSON_PROFILE_RESULT_END__',
          stderr: '',
          figures: [],
        };
      }
      throw new Error(`Unexpected evaluation: ${code}`);
    });
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(backend.handle('profile.control', { action: 'on' }, signal)).resolves.toEqual({
      ok: true,
      on: true,
    });
    await expect(backend.handle('profile.info', {}, signal)).resolves.toEqual({
      rows: [
        {
          function: 'sin',
          file: '/modules/elementary_functions/functions/example.m',
          line: 4,
          calls: 3,
          total: 0.03,
          perCall: 0.01,
        },
      ],
      on: false,
    });
    const exported = (await backend.handle(
      'profile.export',
      { path: '/workspace/profile_result' },
      signal
    )) as { index: string; url: string };
    expect(exported.index).toBe('/workspace/profile_result/index.html');
    expect(exported.url).toMatch(/^data:text\/html/);
    const profileWrite = vi.mocked(fixture.client.writeFile).mock.calls[0];
    expect(profileWrite?.[0]).toBe('/workspace/profile_result/index.html');
    expect(new TextDecoder().decode(profileWrite?.[1])).toContain('Nelson profile report');
    backend.dispose();
  });

  it('caches Worker figure frames for desktop figure RPCs', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    const frame = {
      handle: 0,
      displayList: { width: 640, height: 480, primitives: [] },
    };

    fixture.sendFigure(frame);

    await expect(backend.handle('figure.list', {}, signal)).resolves.toEqual({
      figures: [{ handle: 0, title: 'Figure 1', windowStyle: 'docked' }],
    });
    await expect(
      backend.handle('figure.getDisplayList', { handle: 0 }, signal)
    ).resolves.toMatchObject({ handle: 0, width: 640, height: 480 });
    await expect(backend.handle('figure.getImage', { handle: 0 }, signal)).resolves.toEqual({
      handle: 0,
      image: 'data:image/png;base64,iVBORw0KGgo=',
      width: 640,
      height: 480,
    });
    expect(fixture.client.getFigureImage).toHaveBeenCalledWith(0);
    await expect(
      backend.handle('figure.saveAs', { handle: 0, path: 'saved.svg' }, signal)
    ).resolves.toEqual({ ok: true, path: '/workspace/saved.svg' });
    expect(fixture.client.saveFigure).toHaveBeenCalledWith(0, '/workspace/saved.svg');
    backend.dispose();
  });

  it('keeps the complete display list when compact matrix-only frames stream', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    const controls = [{ handle: 7, style: 'pushbutton', text: 'Start', x: 12, y: 8, w: 86, h: 28 }];
    const geometry = { id: 1, revision: '2', positions: [0, 0, 0], model: [1], projection: [1] };
    const complete = {
      width: 640,
      height: 480,
      layoutSignature: 'layout-1',
      primitives: [{ kind: 'rect', x: 0, y: 0, w: 10, h: 10 }],
      controls,
      webglMeshes: [geometry],
    };
    const compact = (model: number[]) => ({
      width: 640,
      height: 480,
      primitives: [],
      controls: [],
      matrixOnly: true,
      webglMeshes: [{ id: 1, revision: '2', model, projection: [3] }],
    });
    const pushedFrames: unknown[] = [];
    const off = onPush('figure.frame', (payload) => pushedFrames.push(payload));

    // A compact frame arriving before the flush must not discard the pending
    // complete frame of the same figure.
    fixture.sendFigure({ handle: 0, displayList: complete });
    fixture.sendFigure({ handle: 0, displayList: compact([2]) });
    await expect(
      backend.handle('figure.getDisplayList', { handle: 0 }, signal)
    ).resolves.toMatchObject({
      handle: 0,
      controls,
      layoutSignature: 'layout-1',
      primitives: complete.primitives,
      webglMeshes: [{ id: 1, positions: [0, 0, 0], model: [2], projection: [3] }],
    });

    // Streamed compact frames reach the panel as they are, while the cached
    // list keeps the controls for later refreshes and busy-time replies.
    fixture.sendFigure({ handle: 0, displayList: compact([5]) });
    await vi.waitFor(() => expect(pushedFrames).toHaveLength(2));
    expect(pushedFrames[1]).toMatchObject({
      handle: 0,
      displayList: { matrixOnly: true, controls: [] },
    });
    const cached = (await backend.handle('figure.getDisplayList', { handle: 0 }, signal)) as {
      controls: unknown[];
      matrixOnly?: boolean;
      webglMeshes: { model: number[] }[];
    };
    expect(cached.controls).toEqual(controls);
    expect(cached.matrixOnly).toBeUndefined();
    expect(cached.webglMeshes[0].model).toEqual([5]);
    off();
    backend.dispose();
  });

  it('reconciles script titles and removes closed figures from authoritative metadata', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    fixture.sendFigure({ handle: 0, displayList: { width: 640, height: 480, primitives: [] } });
    vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
      exitCode: 0,
      stdout: '',
      stderr: '',
      figureList: [{ handle: 0, title: 'Hélice' }],
    });
    await backend.handle('term.input', { line: "f.Name='Hélice';" }, signal);
    await expect(backend.handle('figure.list', {}, signal)).resolves.toMatchObject({
      figures: [{ handle: 0, title: 'Hélice' }],
    });
    vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
      exitCode: 0,
      stdout: '',
      stderr: '',
      figureList: [],
    });
    await backend.handle('term.input', { line: 'close(f);' }, signal);
    await expect(backend.handle('figure.list', {}, signal)).resolves.toEqual({ figures: [] });
    backend.dispose();
  });

  it('coalesces rapid Worker animation frames before pushing to the desktop', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    const pushedFrames: unknown[] = [];
    const off = onPush('figure.frame', (payload) => pushedFrames.push(payload));
    window.__nelsonWasmFigureProfileEnabled = true;
    resetWasmFigureProfile();

    fixture.sendFigure({
      handle: 0,
      displayList: { width: 640, height: 480, primitives: [{ kind: 'text', text: 'first' }] },
    });
    fixture.sendFigure({
      handle: 0,
      displayList: { width: 640, height: 480, primitives: [{ kind: 'text', text: 'second' }] },
    });
    fixture.sendFigure({
      handle: 0,
      displayList: { width: 640, height: 480, primitives: [{ kind: 'text', text: 'third' }] },
    });

    await new Promise((resolve) => window.setTimeout(resolve, 20));

    expect(pushedFrames).toHaveLength(1);
    expect(pushedFrames[0]).toMatchObject({
      handle: 0,
      displayList: { primitives: [expect.objectContaining({ text: 'third' })] },
    });
    expect(getWasmFigureProfile()).toMatchObject({
      receivedFrames: 3,
      flushedFrames: 1,
      coalescedFrames: 2,
      maxPendingFrames: 1,
    });
    window.__nelsonWasmFigureProfileEnabled = false;
    off();
    backend.dispose();
  });

  it('retains WASM image payloads referenced by later animation frames', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    fixture.sendFigure({
      handle: 0,
      displayList: {
        width: 2,
        height: 2,
        primitives: [
          { kind: 'image', x: 0, y: 0, w: 2, h: 2, imageId: 'pixels-1', image: 'data:first' },
        ],
      },
    });
    fixture.sendFigure({
      handle: 0,
      displayList: {
        width: 2,
        height: 2,
        primitives: [{ kind: 'image', x: 0, y: 0, w: 2, h: 2, imageId: 'pixels-1' }],
      },
    });

    await expect(backend.handle('figure.getDisplayList', { handle: 0 }, signal)).resolves.toEqual(
      expect.objectContaining({
        primitives: [expect.objectContaining({ imageId: 'pixels-1', image: 'data:first' })],
      })
    );
    backend.dispose();
  });

  it('retains WASM RGBA image attachments referenced by later animation frames', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    const pixels = new Uint8Array([1, 2, 3, 4]).buffer;
    fixture.sendFigure({
      handle: 0,
      displayList: {
        width: 2,
        height: 2,
        primitives: [
          {
            kind: 'image',
            x: 0,
            y: 0,
            w: 2,
            h: 2,
            imageId: 'rgba-1',
            imageWidth: 1,
            imageHeight: 1,
            rgbaBytes: pixels,
          },
        ],
      },
    });
    fixture.sendFigure({
      handle: 0,
      displayList: {
        width: 2,
        height: 2,
        primitives: [{ kind: 'image', x: 0, y: 0, w: 2, h: 2, imageId: 'rgba-1' }],
      },
    });

    await expect(backend.handle('figure.getDisplayList', { handle: 0 }, signal)).resolves.toEqual(
      expect.objectContaining({
        primitives: [
          expect.objectContaining({
            imageId: 'rgba-1',
            imageWidth: 1,
            imageHeight: 1,
            rgbaBytes: pixels,
          }),
        ],
      })
    );
    backend.dispose();
  });

  it('round-trips browser workspace files', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);

    await backend.handle(
      'fs.upload',
      { path: '/workspace/example.txt', data: btoa('portable') },
      signal
    );

    await expect(backend.handle('fs.list', { path: '/workspace' }, signal)).resolves.toEqual({
      cwd: '/workspace',
      entries: [
        {
          name: 'example.txt',
          path: '/workspace/example.txt',
          isDir: false,
          size: 8,
        },
      ],
    });
    await expect(
      backend.handle('fs.download', { path: '/workspace/example.txt' }, signal)
    ).resolves.toMatchObject({ name: 'example.txt', data: btoa('portable') });
    backend.dispose();
  });

  it('creates, navigates, renames, and deletes browser workspace entries', async () => {
    const fixture = clientFixture();
    const directories = new Set<string>();
    vi.mocked(fixture.client.listDirectories).mockImplementation(async () => [...directories]);
    vi.mocked(fixture.client.createDirectory).mockImplementation(async (path) => {
      directories.add(path);
      return { ok: true, path };
    });
    vi.mocked(fixture.client.renamePath).mockImplementation(async (from, to) => {
      directories.delete(from);
      directories.add(to);
      return { ok: true, path: to };
    });
    vi.mocked(fixture.client.deletePath).mockImplementation(async (path) => {
      directories.delete(path);
      return { ok: true };
    });
    const backend = new WasmDesktopBackend(fixture.client);

    await backend.handle('fs.create', { path: '/workspace/src', kind: 'dir' }, signal);
    await expect(backend.handle('fs.list', { path: '/workspace' }, signal)).resolves.toEqual({
      cwd: '/workspace',
      entries: [{ name: 'src', path: '/workspace/src', isDir: true, size: 0 }],
    });
    await expect(backend.handle('fs.chdir', { path: '/workspace/src' }, signal)).resolves.toEqual({
      cwd: '/workspace/src',
      separator: '/',
      recentDirectories: ['/workspace'],
    });
    await backend.handle('fs.rename', { from: '/workspace/src', to: '/workspace/lib' }, signal);
    await backend.handle('fs.delete', { path: '/workspace/lib' }, signal);

    expect(fixture.client.createDirectory).toHaveBeenCalledWith('/workspace/src');
    expect(fixture.client.renamePath).toHaveBeenCalledWith('/workspace/src', '/workspace/lib');
    expect(fixture.client.deletePath).toHaveBeenCalledWith('/workspace/lib');
    backend.dispose();
  });

  it('rejects browser imports above the generated resource limit', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client, {
      runtimeCapabilities: runtimeCapabilities(4),
    });

    await expect(
      backend.handle('fs.upload', { path: '/workspace/large.bin', data: btoa('12345') }, signal)
    ).rejects.toThrow('browser import exceeds the 4-byte limit');
    expect(fixture.client.writeFile).not.toHaveBeenCalled();
    backend.dispose();
  });

  it('opens, saves, and reopens editor files in the virtual workspace', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(backend.handle('editor.open', { path: 'script.m' }, signal)).resolves.toEqual({
      path: 'script.m',
      content: '',
      language: 'nelson',
    });
    await backend.handle(
      'editor.save',
      { path: 'script.m', content: 'editorAnswer = 73;' },
      signal
    );
    await expect(backend.handle('editor.open', { path: 'script.m' }, signal)).resolves.toEqual({
      path: 'script.m',
      content: 'editorAnswer = 73;',
      language: 'nelson',
    });
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/workspace/script.m',
      new TextEncoder().encode('editorAnswer = 73;')
    );
    backend.dispose();
  });

  it('opens and saves packaged module sources without rewriting their path', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    const path = '/modules/graphics/examples/portable_peaks.m';

    await backend.handle('editor.save', { path, content: 'plot(1:3);' }, signal);
    await expect(backend.handle('editor.open', { path }, signal)).resolves.toEqual({
      path,
      content: 'plot(1:3);',
      language: 'nelson',
    });
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      path,
      new TextEncoder().encode('plot(1:3);')
    );
    backend.dispose();
  });

  it('materializes packaged examples before opening them in the editor', async () => {
    const fixture = clientFixture();
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      if (
        String(input) ===
        'https://example.test/examples/files/graphics/examples/portable_peaks.m?build=abc123'
      ) {
        return new Response('surf(peaks)\n');
      }
      if (
        String(input) ===
        'https://example.test/examples/files/graphics/examples/portable_peaks.nh5?build=abc123'
      ) {
        return new Response('asset-bytes');
      }
      throw new Error(`Unexpected example fetch: ${String(input)}`);
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html?build=abc123',
      packageManagerUrl: 'https://example.test/nmm/index.html?build=abc123',
      deploymentManifest: {
        files: [
          { file: 'examples/files/graphics/examples/portable_peaks.m' },
          { file: 'examples/files/graphics/examples/portable_peaks.nh5' },
          { file: 'examples/files/graphics/examples/nested/ignored_support.dat' },
        ],
      },
    });
    const path = '/modules/graphics/examples/portable_peaks.m';

    await expect(backend.handle('editor.open', { path }, signal)).resolves.toEqual({
      path,
      content: 'surf(peaks)\n',
      language: 'nelson',
    });
    expect(fixture.client.writeFile).toHaveBeenCalledWith(path, expect.any(Uint8Array));
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/graphics/examples/portable_peaks.nh5',
      expect.any(Uint8Array)
    );
    expect(fixture.client.writeFile).not.toHaveBeenCalledWith(
      '/modules/graphics/examples/nested/ignored_support.dat',
      expect.any(Uint8Array)
    );
    const written = vi.mocked(fixture.client.writeFile).mock.calls[0]?.[1] as Uint8Array;
    expect(new TextDecoder().decode(written)).toBe('surf(peaks)\n');
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('materializes packaged example asset directories by convention', async () => {
    const fixture = clientFixture();
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      switch (String(input)) {
        case 'https://example.test/examples/files/graphics/examples/retro_raycaster.m?build=abc123':
          return new Response('disp("raycaster")\n');
        case 'https://example.test/examples/files/graphics/examples/retro_raycaster_assets/brick.png?build=abc123':
          return new Response('brick-bytes');
        default:
          throw new Error(`Unexpected example fetch: ${String(input)}`);
      }
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html?build=abc123',
      deploymentManifest: {
        files: [
          { file: 'examples/files/graphics/examples/retro_raycaster.m' },
          { file: 'examples/files/graphics/examples/retro_raycaster_assets/brick.png' },
          { file: 'examples/files/graphics/examples/unrelated_assets/ignored.png' },
        ],
      },
      historyStorage: null,
    });

    await backend.handle(
      'term.input',
      { line: "run('/modules/graphics/examples/retro_raycaster.m')" },
      signal
    );

    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/graphics/examples/retro_raycaster.m',
      expect.any(Uint8Array)
    );
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/graphics/examples/retro_raycaster_assets/brick.png',
      expect.any(Uint8Array)
    );
    expect(fixture.client.writeFile).not.toHaveBeenCalledWith(
      '/modules/graphics/examples/unrelated_assets/ignored.png',
      expect.any(Uint8Array)
    );
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('materializes a packaged example without the deployment manifest fallback', async () => {
    const fixture = clientFixture();
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      expect(String(input)).toBe(
        'https://example.test/examples/files/graphics/examples/portable_peaks.m?build=abc123'
      );
      return new Response('surf(peaks)\n');
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html?build=abc123',
      packageManagerUrl: 'https://example.test/nmm/index.html?build=abc123',
    });
    const path = '/modules/graphics/examples/portable_peaks.m';

    await expect(backend.handle('editor.open', { path }, signal)).resolves.toEqual({
      path,
      content: 'surf(peaks)\n',
      language: 'nelson',
    });
    expect(fixture.client.writeFile).toHaveBeenCalledWith(path, expect.any(Uint8Array));
    const written = vi.mocked(fixture.client.writeFile).mock.calls[0]?.[1] as Uint8Array;
    expect(new TextDecoder().decode(written)).toBe('surf(peaks)\n');
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('materializes packaged examples referenced by command-line run calls', async () => {
    const fixture = clientFixture();
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      expect(String(input)).toBe(
        'https://example.test/examples/files/graphics/examples/fluid_simulation_2d.m?build=abc123'
      );
      return new Response('disp("fluid example")\n');
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html?build=abc123',
      historyStorage: null,
    });

    await backend.handle(
      'term.input',
      { line: "run('/modules/graphics/examples/fluid_simulation_2d.m')" },
      signal
    );

    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/graphics/examples/fluid_simulation_2d.m',
      expect.any(Uint8Array)
    );
    const written = vi.mocked(fixture.client.writeFile).mock.calls[0]?.[1] as Uint8Array;
    expect(new TextDecoder().decode(written)).toBe('disp("fluid example")\n');
    expect(fixture.client.evaluate).toHaveBeenCalledWith(
      "run('/modules/graphics/examples/fluid_simulation_2d.m')",
      expect.any(Function)
    );
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('mounts packaged runtime resources before running tests', async () => {
    const fixture = clientFixture();
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      switch (String(input)) {
        case 'https://example.test/runtime-resources/index.json?build=abc123':
          return new Response(
            JSON.stringify({
              formatVersion: 1,
              modules: [
                {
                  module: 'string',
                  tests: true,
                  help: false,
                  bundles: {
                    tests: {
                      file: 'runtime-resources/bundles/string-tests.nlpack',
                      files: ['runtime-resources/modules/string/tests/test_strfind.m'],
                    },
                  },
                },
                {
                  module: 'tests_manager',
                  tests: true,
                  help: false,
                  bundles: {
                    tests: {
                      file: 'runtime-resources/bundles/tests_manager-tests.nlpack',
                      files: [
                        'runtime-resources/modules/tests_manager/tests/helpers/validateExampleModule.m',
                      ],
                    },
                  },
                },
                {
                  module: 'interpreter',
                  tests: true,
                  help: false,
                  bundles: {
                    tests: {
                      file: 'runtime-resources/bundles/interpreter-tests.nlpack',
                      files: ['runtime-resources/modules/interpreter/tests/bench_loop_00.m'],
                    },
                  },
                },
              ],
            })
          );
        case 'https://example.test/runtime-resources/bundles/string-tests.nlpack?build=abc123':
          return new Response(
            runtimeResourceBundle({
              'runtime-resources/modules/string/tests/test_strfind.m': 'assert_istrue(true)\n',
            })
          );
        case 'https://example.test/runtime-resources/bundles/tests_manager-tests.nlpack?build=abc123':
          return new Response(
            runtimeResourceBundle({
              'runtime-resources/modules/tests_manager/tests/helpers/validateExampleModule.m':
                'function validateExampleModule(varargin), end\n',
            })
          );
        case 'https://example.test/runtime-resources/bundles/interpreter-tests.nlpack?build=abc123':
          return new Response(
            runtimeResourceBundle({
              'runtime-resources/modules/interpreter/tests/bench_loop_00.m': 'disp("bench")\n',
            })
          );
        case 'https://example.test/examples/files/string/examples/parse_measurements.m?build=abc123':
          return new Response('disp("example")\n');
        default:
          throw new Error(`Unexpected runtime resource fetch: ${String(input)}`);
      }
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html?build=abc123',
      runtimeResourcesUrl: 'https://example.test/runtime-resources/index.json?build=abc123',
      runtimeCapabilities: runtimeCapabilities(),
      deploymentManifest: {
        files: [
          { file: 'runtime-resources/index.json' },
          { file: 'runtime-resources/bundles/string-tests.nlpack' },
          { file: 'runtime-resources/bundles/tests_manager-tests.nlpack' },
          { file: 'runtime-resources/bundles/interpreter-tests.nlpack' },
          { file: 'examples/files/string/examples/parse_measurements.m' },
        ],
        delivery: {
          formatVersion: 1,
          startup: [],
          onDemand: [
            {
              id: 'runtime-resources',
              mode: 'on-demand',
              trigger: 'runtime-resources',
              files: [
                'runtime-resources/index.json',
                'runtime-resources/bundles/string-tests.nlpack',
                'runtime-resources/bundles/tests_manager-tests.nlpack',
                'runtime-resources/bundles/interpreter-tests.nlpack',
              ],
              bytes: 42,
            },
            {
              id: 'examples',
              mode: 'on-demand',
              trigger: 'demo',
              files: ['examples/files/string/examples/parse_measurements.m'],
              bytes: 15,
            },
          ],
        },
      },
      historyStorage: null,
    });

    await backend.handle('term.input', { line: "test_run('string', 'test_strfind')" }, signal);

    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/string/tests/test_strfind.m',
      expect.any(Uint8Array)
    );
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/tests_manager/tests/helpers/validateExampleModule.m',
      expect.any(Uint8Array)
    );
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/string/examples/parse_measurements.m',
      expect.any(Uint8Array)
    );
    expect(fixture.client.evaluate).toHaveBeenCalledWith(
      "test_run('string', 'test_strfind')",
      expect.any(Function)
    );

    await backend.handle('term.input', { line: "bench_run('string')" }, signal);

    expect(fetchMock).toHaveBeenCalledTimes(4);

    await backend.handle('term.input', { line: 'bench_run' }, signal);

    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/interpreter/tests/bench_loop_00.m',
      expect.any(Uint8Array)
    );
    expect(fixture.client.evaluate).toHaveBeenCalledWith('bench_run', expect.any(Function));
    expect(fetchMock).toHaveBeenCalledTimes(5);
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('mounts only the generated help index for command-window help topics', async () => {
    const fixture = clientFixture();
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      switch (String(input)) {
        case 'https://example.test/runtime-resources/index.json?build=abc123':
          return new Response(
            JSON.stringify({
              formatVersion: 1,
              modules: [
                {
                  module: 'help_tools',
                  tests: false,
                  help: true,
                  bundles: {
                    help: {
                      file: 'runtime-resources/bundles/help_tools-help.nlpack',
                      files: ['runtime-resources/modules/help_tools/help/nelson_help_en_US.json'],
                    },
                  },
                },
                {
                  module: 'graphics',
                  tests: false,
                  help: true,
                  bundles: {
                    help: {
                      file: 'runtime-resources/bundles/graphics-help.nlpack',
                      files: ['runtime-resources/modules/graphics/help/en_US/xml/plot.xml'],
                    },
                    helpAssets: {
                      file: 'runtime-resources/bundles/graphics-help-assets.nlpack',
                      files: ['runtime-resources/modules/graphics/help/en_US/images/plot.svg'],
                    },
                  },
                },
              ],
            })
          );
        case 'https://example.test/runtime-resources/bundles/help_tools-help.nlpack?build=abc123':
          return new Response(
            runtimeResourceBundle({
              'runtime-resources/modules/help_tools/help/nelson_help_en_US.json': '{}\n',
            })
          );
        default:
          throw new Error(`Unexpected help resource fetch: ${String(input)}`);
      }
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      runtimeResourcesUrl: 'https://example.test/runtime-resources/index.json?build=abc123',
      runtimeCapabilities: runtimeCapabilities(),
      deploymentManifest: {
        files: [
          { file: 'runtime-resources/index.json' },
          { file: 'runtime-resources/bundles/help_tools-help.nlpack' },
          { file: 'runtime-resources/bundles/graphics-help.nlpack' },
          { file: 'runtime-resources/bundles/graphics-help-assets.nlpack' },
        ],
        delivery: {
          formatVersion: 1,
          startup: [],
          onDemand: [
            {
              id: 'runtime-resources',
              mode: 'on-demand',
              trigger: 'runtime-resources',
              files: [
                'runtime-resources/index.json',
                'runtime-resources/bundles/help_tools-help.nlpack',
                'runtime-resources/bundles/graphics-help.nlpack',
                'runtime-resources/bundles/graphics-help-assets.nlpack',
              ],
              bytes: 42,
            },
          ],
        },
      },
      historyStorage: null,
    });

    await backend.handle('term.input', { line: 'help' }, signal);

    expect(fetchMock).not.toHaveBeenCalled();

    await backend.handle('term.input', { line: 'help sin' }, signal);

    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/help_tools/help/nelson_help_en_US.json',
      expect.any(Uint8Array)
    );
    expect(fixture.client.evaluate).toHaveBeenCalledWith('help sin', expect.any(Function));
    expect(fetchMock).toHaveBeenCalledTimes(2);
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('mounts help archives before deployhelp', async () => {
    const fixture = clientFixture();
    const archiveBundle = 'runtime-resources/bundles/graphics-help-archive.nlpack';
    const archiveFile =
      'runtime-resources/modules/help_tools/help/io.github.nelson_lang.Nelson.modules.graphics.help.en_US.nhz';
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      switch (String(input)) {
        case 'https://example.test/runtime-resources/index.json?build=abc123':
          return new Response(
            JSON.stringify({
              formatVersion: 1,
              modules: [
                {
                  module: 'help_tools',
                  bundles: {
                    archives: {
                      file: archiveBundle,
                      files: [archiveFile],
                    },
                  },
                },
              ],
            })
          );
        case 'https://example.test/runtime-resources/bundles/graphics-help-archive.nlpack?build=abc123':
          return new Response(runtimeResourceBundle({ [archiveFile]: 'nhz' }));
        default:
          throw new Error(`Unexpected help archive fetch: ${String(input)}`);
      }
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      runtimeResourcesUrl: 'https://example.test/runtime-resources/index.json?build=abc123',
      runtimeCapabilities: runtimeCapabilities(),
      deploymentManifest: {
        files: [{ file: 'runtime-resources/index.json' }, { file: archiveBundle }],
        delivery: {
          formatVersion: 1,
          startup: [],
          onDemand: [
            {
              id: 'runtime-resources',
              mode: 'on-demand',
              trigger: 'runtime-resources',
              files: ['runtime-resources/index.json', archiveBundle],
              bytes: 3,
            },
          ],
        },
      },
      historyStorage: null,
    });

    await expect(backend.handle('term.input', { line: 'deployhelp' }, signal)).resolves.toEqual({
      accepted: true,
    });
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/graphics/help/io.github.nelson_lang.Nelson.modules.graphics.help.en_US.nhz',
      expect.any(Uint8Array)
    );
    expect(fixture.client.evaluate).toHaveBeenCalledWith('deployhelp', expect.any(Function));
    expect(fetchMock).toHaveBeenCalledTimes(2);
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('opens rich local documentation from an extracted help archive', async () => {
    const fixture = clientFixture();
    const archiveBundle = 'runtime-resources/bundles/graphics-help-archive.nlpack';
    const archiveFile =
      'runtime-resources/modules/help_tools/help/io.github.nelson_lang.Nelson.modules.graphics.help.en_US.nhz';
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      switch (String(input)) {
        case 'https://example.test/runtime-resources/index.json?build=abc123':
          return new Response(
            JSON.stringify({
              formatVersion: 1,
              modules: [
                {
                  module: 'help_tools',
                  bundles: { archives: { file: archiveBundle, files: [archiveFile] } },
                },
              ],
            })
          );
        case 'https://example.test/runtime-resources/bundles/graphics-help-archive.nlpack?build=abc123':
          return new Response(runtimeResourceBundle({ [archiveFile]: 'nhz' }));
        default:
          throw new Error(`Unexpected help archive fetch: ${String(input)}`);
      }
    });
    vi.stubGlobal('fetch', fetchMock);
    await fixture.client.writeFile(
      '/workspace/.nelson-wasm-help/en_US/graphics/1_plots/1_line_plots/plot.html',
      new TextEncoder().encode(
        '<html><head><title>plot</title><link rel="stylesheet" href="../../nelson_common.css"></head>' +
          '<body><a href="other.html">Other</a><p>Plot documentation</p></body></html>'
      )
    );
    await fixture.client.writeFile(
      '/workspace/.nelson-wasm-help/en_US/graphics/nelson_common.css',
      new TextEncoder().encode('body { color: red; }')
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      runtimeResourcesUrl: 'https://example.test/runtime-resources/index.json?build=abc123',
      runtimeCapabilities: runtimeCapabilities(),
      deploymentManifest: {
        files: [{ file: 'runtime-resources/index.json' }, { file: archiveBundle }],
        delivery: {
          formatVersion: 1,
          startup: [],
          onDemand: [
            {
              id: 'runtime-resources',
              mode: 'on-demand',
              trigger: 'runtime-resources',
              files: ['runtime-resources/index.json', archiveBundle],
              bytes: 3,
            },
          ],
        },
      },
      historyStorage: null,
    });

    vi.mocked(fixture.client.evaluate)
      .mockResolvedValueOnce({
        exitCode: 0,
        stdout: '__NELSON_DOCROOT_BEGIN__\n.\n__NELSON_DOCROOT_END__',
        stderr: '',
        figures: [],
      })
      .mockResolvedValueOnce({
        exitCode: 0,
        stdout:
          'file:///workspace/.nelson-wasm-help/en_US/index.html#open=graphics%2F1_plots%2F1_line_plots%2Fplot.html',
        stderr: '',
        figures: [],
      });
    await expect(backend.handle('help.document', { name: 'plot' }, signal)).resolves.toMatchObject({
      path: '/workspace/.nelson-wasm-help/en_US/graphics/1_plots/1_line_plots/plot.html',
      title: 'plot',
      html: expect.stringContaining('body { color: red; }'),
    });
    expect(fixture.client.evaluate).toHaveBeenCalledWith(
      expect.stringContaining("doc('plot')"),
      expect.any(Function)
    );
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('returns the configured remote documentation URL without mounting archives', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client, {
      runtimeResourcesUrl: 'https://example.test/runtime-resources/index.json?build=abc123',
      runtimeCapabilities: runtimeCapabilities(),
      deploymentManifest: {
        files: [{ file: 'runtime-resources/index.json' }],
        delivery: {
          formatVersion: 1,
          startup: [],
          onDemand: [
            {
              id: 'runtime-resources',
              mode: 'on-demand',
              trigger: 'runtime-resources',
              files: ['runtime-resources/index.json'],
              bytes: 1,
            },
          ],
        },
      },
      historyStorage: null,
    });

    vi.mocked(fixture.client.evaluate)
      .mockResolvedValueOnce({
        exitCode: 0,
        stdout: '__NELSON_DOCROOT_BEGIN__\nhttps://nelson.example/docs\n__NELSON_DOCROOT_END__',
        stderr: '',
        figures: [],
      })
      .mockResolvedValueOnce({
        exitCode: 0,
        stdout:
          'https://nelson.example/docs/en_US/v1.0.0/index.html?open=trigonometric_functions%2Fcos.html',
        stderr: '',
        figures: [],
      });

    await expect(backend.handle('help.document', { name: 'cos' }, signal)).resolves.toEqual({
      path: 'https://nelson.example/docs/en_US/v1.0.0/index.html?open=trigonometric_functions%2Fcos.html',
      title: 'cos',
      url: 'https://nelson.example/docs/en_US/v1.0.0/index.html?open=trigonometric_functions%2Fcos.html',
    });
    expect(fixture.client.evaluate).toHaveBeenCalledTimes(2);
    backend.dispose();
  });

  it('can run again after interrupting runtime resource preparation', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });

    let abortedFirstBundle = false;
    let bundleFetches = 0;
    const fetchMock = vi.fn(async (input: string | URL | Request, init?: RequestInit) => {
      switch (String(input)) {
        case 'https://example.test/runtime-resources/index.json?build=abc123':
          return new Response(
            JSON.stringify({
              formatVersion: 1,
              modules: [
                {
                  module: 'string',
                  tests: true,
                  help: false,
                  bundles: {
                    tests: {
                      file: 'runtime-resources/bundles/string-tests.nlpack',
                      files: ['runtime-resources/modules/string/tests/test_strfind.m'],
                    },
                  },
                },
              ],
            })
          );
        case 'https://example.test/runtime-resources/bundles/string-tests.nlpack?build=abc123': {
          bundleFetches += 1;
          if (bundleFetches === 1) {
            return await new Promise<Response>((_resolve, reject) => {
              init?.signal?.addEventListener('abort', () => {
                abortedFirstBundle = true;
                reject(new DOMException('aborted', 'AbortError'));
              });
            });
          }
          return new Response(
            runtimeResourceBundle({
              'runtime-resources/modules/string/tests/test_strfind.m': 'assert_istrue(true)\n',
            })
          );
        }
        default:
          throw new Error(`Unexpected runtime resource fetch: ${String(input)}`);
      }
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      runtimeResourcesUrl: 'https://example.test/runtime-resources/index.json?build=abc123',
      runtimeCapabilities: runtimeCapabilities(),
      deploymentManifest: {
        files: [
          { file: 'runtime-resources/index.json' },
          { file: 'runtime-resources/bundles/string-tests.nlpack' },
        ],
        delivery: {
          formatVersion: 1,
          startup: [],
          onDemand: [
            {
              id: 'runtime-resources',
              mode: 'on-demand',
              trigger: 'runtime-resources',
              files: [
                'runtime-resources/index.json',
                'runtime-resources/bundles/string-tests.nlpack',
              ],
              bytes: 42,
            },
          ],
        },
      },
      historyStorage: null,
    });

    const firstRun = backend.handle('term.input', { line: "test_run('string')" }, signal);
    await vi.waitFor(() => expect(fetchMock).toHaveBeenCalledTimes(2));
    await backend.handle('term.interrupt', { busy: true }, signal);
    await firstRun;

    expect(abortedFirstBundle).toBe(true);
    expect(fixture.client.interrupt).not.toHaveBeenCalled();
    expect(pushes).toContainEqual({
      channel: 'term.output',
      payload: { text: '\u001b[31mNelson evaluation interrupted\u001b[0m\n' },
    });

    await backend.handle('term.input', { line: "test_run('string')" }, signal);

    expect(bundleFetches).toBe(2);
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/string/tests/test_strfind.m',
      expect.any(Uint8Array)
    );
    expect(fixture.client.evaluate).toHaveBeenCalledWith(
      "test_run('string')",
      expect.any(Function)
    );
    backend.dispose();
    window.__nelsonPush = previousPush;
    vi.unstubAllGlobals();
  });

  it('remounts runtime resources after interrupting a running WebAssembly evaluation', async () => {
    const fixture = clientFixture();
    let bundleFetches = 0;
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      switch (String(input)) {
        case 'https://example.test/runtime-resources/index.json?build=abc123':
          return new Response(
            JSON.stringify({
              formatVersion: 1,
              modules: [
                {
                  module: 'string',
                  tests: true,
                  help: false,
                  bundles: {
                    tests: {
                      file: 'runtime-resources/bundles/string-tests.nlpack',
                      files: ['runtime-resources/modules/string/tests/bench_string.m'],
                    },
                  },
                },
              ],
            })
          );
        case 'https://example.test/runtime-resources/bundles/string-tests.nlpack?build=abc123':
          bundleFetches += 1;
          return new Response(
            runtimeResourceBundle({
              'runtime-resources/modules/string/tests/bench_string.m': 'disp("bench")\n',
            })
          );
        default:
          throw new Error(`Unexpected runtime resource fetch: ${String(input)}`);
      }
    });
    vi.stubGlobal('fetch', fetchMock);
    let rejectEvaluation: ((error: Error) => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise((_, reject) => {
          rejectEvaluation = reject;
        })
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      runtimeResourcesUrl: 'https://example.test/runtime-resources/index.json?build=abc123',
      runtimeCapabilities: runtimeCapabilities(),
      deploymentManifest: {
        files: [
          { file: 'runtime-resources/index.json' },
          { file: 'runtime-resources/bundles/string-tests.nlpack' },
        ],
        delivery: {
          formatVersion: 1,
          startup: [],
          onDemand: [
            {
              id: 'runtime-resources',
              mode: 'on-demand',
              trigger: 'runtime-resources',
              files: [
                'runtime-resources/index.json',
                'runtime-resources/bundles/string-tests.nlpack',
              ],
              bytes: 42,
            },
          ],
        },
      },
      historyStorage: null,
    });

    const firstRun = backend.handle('term.input', { line: "test_run('string')" }, signal);
    await vi.waitFor(() => expect(fixture.client.evaluate).toHaveBeenCalledTimes(1));
    await backend.handle('term.interrupt', { busy: true }, signal);
    rejectEvaluation?.(new Error('Nelson evaluation interrupted'));
    await firstRun;

    await backend.handle('term.input', { line: 'bench_run' }, signal);

    expect(fixture.client.interrupt).toHaveBeenCalledTimes(1);
    expect(bundleFetches).toBe(2);
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/modules/string/tests/bench_string.m',
      expect.any(Uint8Array)
    );
    expect(fixture.client.evaluate).toHaveBeenLastCalledWith('bench_run', expect.any(Function));
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('adds installed package examples to the WebAssembly examples catalog', async () => {
    const fixture = clientFixture();
    const begin = '__NELSON_NMM_RESULT_BEGIN__';
    const end = '__NELSON_NMM_RESULT_END__';
    vi.mocked(fixture.client.evaluate).mockImplementation(async (code: string) => {
      if (String(code).includes('nmm_gui_rpc') && String(code).includes("'list'")) {
        return {
          exitCode: 0,
          stdout: `${begin}\n${JSON.stringify({
            ok: true,
            installed: [
              {
                name: 'morse',
                version: '1.0.0',
                path: '/workspace/modules/morse/1.0.0/',
              },
            ],
          })}\n${end}\n`,
          stderr: '',
          figures: [],
        };
      }
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    });
    await fixture.client.writeFile(
      '/workspace/modules/morse/1.0.0/examples/index.json',
      new TextEncoder().encode(
        JSON.stringify({
          schemaVersion: 1,
          examples: [
            {
              file: 'morse_demo.m',
              title: { en_US: 'A tour of the morse toolbox' },
              description: { en_US: 'Encode and decode Morse.' },
              tags: ['morse'],
              requirements: [],
            },
          ],
        })
      )
    );
    vi.mocked(fixture.client.writeFile).mockClear();
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async () => new Response(JSON.stringify({ locale: 'en_US', diagnostics: [], groups: [] }))
      )
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html?build=abc123',
      packageManagerUrl: 'https://example.test/nmm/index.html?build=abc123',
    });

    const result = (await backend.handle('examples.list', {}, signal)) as {
      groups: Array<{ name: string; source: string; examples: Array<{ file: string }> }>;
    };

    expect(result.groups).toEqual([
      {
        name: 'external:morse',
        title: 'morse',
        source: 'external',
        examples: [
          {
            title: 'A tour of the morse toolbox',
            description: 'Encode and decode Morse.',
            kind: 'script',
            tags: ['morse'],
            requirements: [],
            file: '/workspace/modules/morse/1.0.0/examples/morse_demo.m',
          },
        ],
      },
    ]);
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('opens an example from an installed WebAssembly package', async () => {
    const fixture = clientFixture();
    const begin = '__NELSON_NMM_RESULT_BEGIN__';
    const end = '__NELSON_NMM_RESULT_END__';
    vi.mocked(fixture.client.evaluate).mockImplementation(async (code: string) => {
      if (String(code).includes('nmm_gui_rpc') && String(code).includes("'list'")) {
        return {
          exitCode: 0,
          stdout: `${begin}\n${JSON.stringify({
            ok: true,
            installed: [
              {
                name: 'tessera',
                version: '1.0.0',
                path: '/user-modules/Nelson/2.0.0/modules/tessera/1.0.0/',
              },
            ],
          })}\n${end}\n`,
          stderr: '',
          figures: [],
        };
      }
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    });
    await fixture.client.writeFile(
      '/user-modules/Nelson/2.0.0/modules/tessera/1.0.0/examples/index.json',
      new TextEncoder().encode(
        JSON.stringify({
          schemaVersion: 1,
          examples: [{ file: 'animated_julia.m', title: { en_US: 'Animated Julia morph' } }],
        })
      )
    );
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async () => new Response(JSON.stringify({ locale: 'en_US', diagnostics: [], groups: [] }))
      )
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html?build=abc123',
      packageManagerUrl: 'https://example.test/nmm/index.html?build=abc123',
    });

    await expect(
      backend.handle(
        'examples.open',
        {
          path: '/user-modules/Nelson/2.0.0/modules/tessera/1.0.0/examples/animated_julia.m',
        },
        signal
      )
    ).resolves.toEqual({ accepted: true });
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('invalidates examples after package manager mutations', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    const backend = new WasmDesktopBackend(fixture.client, {
      packageManagerUrl: 'https://example.test/nmm/index.html?build=abc123',
    });

    await backend.handle(
      'tool.dispatch',
      { tool: 'nmm', method: 'installSource', params: { source: '/workspace/morse.nmz' } },
      signal
    );

    expect(pushes).toContainEqual({
      channel: 'examples.changed',
      payload: { reason: 'packages' },
    });
    expect(fixture.client.evaluate).toHaveBeenCalledWith("nmm('install', '/workspace/morse.nmz');");
    window.__nelsonPush = previousPush;
    backend.dispose();
  });

  it('routes a portable editor UI action to the desktop editor', async () => {
    const fixture = clientFixture();
    vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
      exitCode: 0,
      stdout: '',
      stderr: '',
      figures: [],
      uiActions: [{ type: 'open-editor', path: '/workspace/demo.m', line: 7 }],
    });
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    const backend = new WasmDesktopBackend(fixture.client);

    await backend.handle('term.input', { line: "editor('/workspace/demo.m', 7)" }, signal);

    expect(pushes).toContainEqual({
      channel: 'editor.show',
      payload: { path: '/workspace/demo.m', line: 7 },
    });
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('routes 3D view changes through the Worker', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(
      backend.handle(
        'figure.setView',
        { handle: 0, axesHandle: 42, azimuth: 12, elevation: 34, compactView: true },
        signal
      )
    ).resolves.toMatchObject({ handle: 0, view: [12, 34] });
    expect(fixture.client.setFigureView).toHaveBeenCalledWith(0, 12, 34, true, 42);
    backend.dispose();
  });

  it('routes figure input, resize, pan, controls, and close through the Worker', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    fixture.sendFigure({
      handle: 0,
      displayList: { width: 640, height: 480, primitives: [] },
    });

    await backend.handle('figure.setSize', { handle: 0, width: 800, height: 600 }, signal);
    await backend.handle('figure.pan', { handle: 0, dxFrac: 0.1, dyFrac: -0.2 }, signal);
    await backend.handle(
      'figure.keyEvent',
      {
        handle: 0,
        pressed: true,
        character: 'a',
        key: 'A',
        shift: true,
        control: false,
        alt: false,
        meta: false,
      },
      signal
    );
    await expect(
      backend.handle('figure.mouseEvent', { handle: 0, action: 'motion', x: 12, y: 34 }, signal)
    ).resolves.toMatchObject({ ok: true, motion: true });
    await backend.handle('uicontrol.action', { handle: 9, value: 1, text: 'Apply' }, signal);
    await backend.handle('figure.close', { handle: 0 }, signal);

    expect(fixture.client.setFigureSize).toHaveBeenCalledWith(0, 800, 600);
    expect(fixture.client.panFigure).toHaveBeenCalledWith(0, 0.1, -0.2);
    expect(fixture.client.sendFigureKeyEvent).toHaveBeenCalledWith(
      expect.objectContaining({ handle: 0, key: 'A', shift: true })
    );
    expect(fixture.client.sendFigureMouseEvent).toHaveBeenCalledWith(
      expect.objectContaining({ handle: 0, action: 'motion', x: 12, y: 34 })
    );
    expect(fixture.client.sendUIControlAction).toHaveBeenCalledWith(9, 1, 'Apply');
    expect(fixture.client.closeFigure).toHaveBeenCalledWith(0);
    await expect(backend.handle('figure.list', {}, signal)).resolves.toEqual({ figures: [] });
    backend.dispose();
  });

  it('returns fresh display lists from WebAssembly figure action responses', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);
    fixture.sendFigure({
      handle: 0,
      displayList: { width: 640, height: 480, primitives: [], view: [0, 90] },
    });
    vi.mocked(fixture.client.setFigureSize).mockResolvedValueOnce({
      ok: true,
      figures: [
        {
          handle: 0,
          displayList: { width: 800, height: 600, primitives: [], view: [-37.5, 30] },
        },
      ],
    });

    await expect(
      backend.handle('figure.setSize', { handle: 0, width: 800, height: 600 }, signal)
    ).resolves.toMatchObject({ width: 800, height: 600, view: [-37.5, 30] });

    backend.dispose();
  });

  it('routes live figure actions through the evaluator command queue while a demo is running', async () => {
    const fixture = clientFixture();
    let finishEvaluation: (() => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise((resolve) => {
          finishEvaluation = () => resolve({ exitCode: 0, stdout: '', stderr: '', figures: [] });
        })
    );
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    const running = backend.handle('term.input', { line: 'interactive_demo()' }, signal);
    await Promise.resolve();
    fixture.sendFigure({
      handle: 0,
      displayList: { width: 640, height: 480, primitives: [], view: [-37.5, 30] },
    });
    await expect(
      backend.handle(
        'figure.setView',
        { handle: 0, axesHandle: 42, azimuth: 12, elevation: 34, compactView: true },
        signal
      )
    ).resolves.toMatchObject({ handle: 0, view: [12, 34] });
    await expect(
      backend.handle('figure.setSize', { handle: 0, width: 800, height: 600 }, signal)
    ).resolves.toMatchObject({ handle: 0, width: 800, height: 600 });
    await backend.handle('figure.pan', { handle: 0, dxFrac: 0.1, dyFrac: -0.2 }, signal);
    await backend.handle(
      'figure.keyEvent',
      {
        handle: 0,
        pressed: true,
        character: "'",
        key: 'Quote',
        shift: true,
        control: false,
        alt: false,
        meta: false,
      },
      signal
    );
    await expect(
      backend.handle('figure.mouseEvent', { handle: 0, action: 'motion', x: 12, y: 34 }, signal)
    ).resolves.toMatchObject({ ok: true, motion: true });
    await expect(
      backend.handle('uicontrol.action', { handle: 9, value: 1, text: "Start 'now'" }, signal)
    ).resolves.toEqual({ ok: true, figures: [] });

    // a busy interpreter gets the view change as a deferred figure action
    expect(fixture.client.queueCommand).toHaveBeenNthCalledWith(
      1,
      `__web_figure_deferred_action__(0, '{"action":"view","axesHandle":42,"azimuth":12,"elevation":34,"compactView":true,"softwareFallback":true}');`
    );
    expect(fixture.client.queueCommand).toHaveBeenNthCalledWith(
      2,
      '__web_figure_size__(0, 800, 600);'
    );
    expect(fixture.client.queueCommand).toHaveBeenNthCalledWith(
      3,
      '__web_figure_pan__(0, 0.1, -0.2);'
    );
    expect(fixture.client.queueCommand).toHaveBeenNthCalledWith(
      4,
      "__web_figure_key_event__(0, true, '''', 'Quote', true, false, false, false);"
    );
    expect(fixture.client.queueCommand).toHaveBeenNthCalledWith(
      5,
      "__figure_mouse_event__(0, 'motion', [12 34], 'web', 'normal');"
    );
    expect(fixture.client.queueCommand).toHaveBeenNthCalledWith(
      6,
      "__web_uicontrol_action__(9, 1, 'Start ''now''');"
    );
    expect(fixture.client.setFigureView).not.toHaveBeenCalled();
    expect(fixture.client.setFigureSize).not.toHaveBeenCalled();
    expect(fixture.client.panFigure).not.toHaveBeenCalled();
    expect(fixture.client.sendFigureKeyEvent).not.toHaveBeenCalled();
    expect(fixture.client.sendFigureMouseEvent).not.toHaveBeenCalled();
    expect(fixture.client.sendUIControlAction).not.toHaveBeenCalled();
    finishEvaluation?.();
    await running;
    backend.dispose();
  });

  it('maps Nelson workspace metadata to the desktop viewer', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(backend.handle('workspace.list', {}, signal)).resolves.toEqual({
      scope: 'base',
      variables: [
        { name: 'answer', className: 'double', size: [1, 1], bytes: 8, value: '1x1 double' },
      ],
    });
    backend.dispose();
  });

  it('opens and replaces variables through the persistent Worker', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(backend.handle('var.open', { name: 'answer' }, signal)).resolves.toMatchObject({
      name: 'answer',
      kind: 'matrix',
    });
    await expect(
      backend.handle('var.getBlock', { name: 'answer', row0: 0, col0: 0, rows: 1, cols: 1 }, signal)
    ).resolves.toEqual({ row0: 0, col0: 0, cells: [[42]] });
    await expect(
      backend.handle('var.replace', { name: 'answer', values: [[73]] }, signal)
    ).resolves.toEqual({ ok: true });
    expect(fixture.client.replaceVariable).toHaveBeenCalledWith('answer', [[73]]);
    await expect(
      backend.handle(
        'var.setNested',
        { name: 'record', kind: 'struct', row: 0, col: 1, value: 'updated' },
        signal
      )
    ).resolves.toEqual({ ok: true });
    expect(fixture.client.setNestedVariable).toHaveBeenCalledWith(
      'record',
      'struct',
      0,
      1,
      'updated'
    );
    backend.dispose();
  });

  it('maps the Nelson completion result to the existing console contract', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(backend.handle('completion.request', { line: 'an' }, signal)).resolves.toEqual({
      prefix: 'an',
      builtin: ['and', 'Angle'],
      macros: [],
      variables: ['answer'],
      files: [],
    });
    expect(fixture.client.complete).toHaveBeenCalledWith('an');
    backend.dispose();
  });

  it('serves documentation text from the portable help function', async () => {
    const fixture = clientFixture();
    vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
      exitCode: 0,
      stdout:
        'warning before marker\n__NELSON_HELP_TEXT_BEGIN__\nSine function help\n__NELSON_HELP_TEXT_END__\n',
      stderr: '',
      figures: [],
    });
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(backend.handle('help.text', { name: 'sin' }, signal)).resolves.toEqual({
      text: 'Sine function help',
    });
    expect(fixture.client.evaluate).toHaveBeenCalledWith(
      expect.stringContaining("help('sin')"),
      expect.any(Function)
    );
    const code = vi.mocked(fixture.client.evaluate).mock.calls[0][0];
    expect(code).not.toContain('__nelson_help_');
    expect(code).not.toContain('clear ');
    backend.dispose();
  });

  it('opens the shared documentation panel for doc commands in WebAssembly', async () => {
    const fixture = clientFixture();
    const shown: unknown[] = [];
    const prompts: unknown[] = [];
    const off = onPush('help.show', (payload) => shown.push(payload));
    const offPrompt = onPush('term.prompt', (payload) => prompts.push(payload));
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(backend.handle('term.input', { line: 'doc cos' }, signal)).resolves.toEqual({
      accepted: true,
    });
    expect(prompts).toEqual([]);
    await new Promise((resolve) => window.setTimeout(resolve, 0));
    expect(prompts).toEqual([{ prompt: '>> ' }]);

    await expect(backend.handle('term.input', { line: "doc('sin')" }, signal)).resolves.toEqual({
      accepted: true,
    });
    await new Promise((resolve) => window.setTimeout(resolve, 0));

    await expect(
      backend.handle('term.input', { line: 'doc tessera.dla' }, signal)
    ).resolves.toEqual({
      accepted: true,
    });
    await new Promise((resolve) => window.setTimeout(resolve, 0));

    expect(shown).toEqual([{ topic: 'cos' }, { topic: 'sin' }, { topic: 'tessera.dla' }]);
    expect(prompts).toEqual([{ prompt: '>> ' }, { prompt: '>> ' }, { prompt: '>> ' }]);
    expect(fixture.client.evaluate).not.toHaveBeenCalled();
    off();
    offPrompt();
    backend.dispose();
  });

  it('rejects missing documentation when no marked text is returned', async () => {
    const fixture = clientFixture();
    vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
      exitCode: 0,
      stdout: '__NELSON_HELP_TEXT_BEGIN__\n\n__NELSON_HELP_TEXT_END__\n',
      stderr: '',
      figures: [],
    });
    const backend = new WasmDesktopBackend(fixture.client);

    await expect(backend.handle('help.text', { name: 'missing_topic' }, signal)).rejects.toThrow(
      'No documentation found for "missing_topic".'
    );
    backend.dispose();
  });

  it('does not re-enter the interpreter for documentation while busy', async () => {
    const fixture = clientFixture();
    let finishEvaluation: (() => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise((resolve) => {
          finishEvaluation = () => resolve({ exitCode: 0, stdout: '', stderr: '', figures: [] });
        })
    );
    const backend = new WasmDesktopBackend(fixture.client, { historyStorage: null });

    const running = backend.handle('term.input', { line: 'animated_demo()' }, signal);
    await Promise.resolve();
    await expect(backend.handle('help.text', { name: 'sin' }, signal)).rejects.toThrow(
      'Documentation is unavailable while Nelson is busy.'
    );
    expect(fixture.client.evaluate).toHaveBeenCalledTimes(1);

    finishEvaluation?.();
    await running;
    backend.dispose();
  });

  it('advertises and opens the packaged NFlow editor when configured', async () => {
    const fixture = clientFixture();
    const openExternal = vi.fn();
    vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
      exitCode: 0,
      stdout: '',
      stderr: '',
      figures: [],
      uiActions: [{ type: 'open-nflow' }],
    });
    const backend = new WasmDesktopBackend(fixture.client, {
      nflowUrl: 'https://example.test/nflow/index.html',
      openExternal,
    });

    await expect(backend.handle('app.capabilities', {}, signal)).resolves.toMatchObject({
      nflow: true,
    });
    await backend.handle('term.input', { line: 'nflow' }, signal);

    expect(openExternal).toHaveBeenCalledWith('https://example.test/nflow/index.html');
    expect(fixture.client.evaluate).toHaveBeenCalledWith('nflow', expect.any(Function));
    backend.dispose();
  });

  it('hands an open_system model to an open NFlow page, or opens one on it', async () => {
    // Minimal same-process BroadcastChannel: every instance of a name sees
    // the messages posted by the others.
    const channels = new Map<string, Set<FakeChannel>>();
    class FakeChannel {
      onmessage: ((event: { data: unknown }) => void) | null = null;
      constructor(readonly name: string) {
        if (!channels.has(name)) channels.set(name, new Set());
        channels.get(name)!.add(this);
      }
      postMessage(data: unknown): void {
        for (const peer of channels.get(this.name) ?? []) {
          if (peer !== this) queueMicrotask(() => peer.onmessage?.({ data }));
        }
      }
      close(): void {
        channels.get(this.name)?.delete(this);
      }
    }
    const previous = globalThis.BroadcastChannel;
    (globalThis as { BroadcastChannel: unknown }).BroadcastChannel = FakeChannel;
    try {
      const action = {
        type: 'open-nflow',
        path: '/modules/nflow_blocks/examples/workspace/From_Workspace_Demo.nflow',
      };
      // No NFlow page listening: a new page opens on the model.
      const fixture = clientFixture();
      const openExternal = vi.fn();
      vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
        exitCode: 0,
        stdout: '',
        stderr: '',
        figures: [],
        uiActions: [action],
      });
      const backend = new WasmDesktopBackend(fixture.client, {
        nflowUrl: 'https://example.test/nflow/index.html?build=abc',
        openExternal,
      });
      await backend.handle('term.input', { line: 'open_system(f)' }, signal);
      await new Promise((resolve) => setTimeout(resolve, 450));
      expect(openExternal).toHaveBeenCalledWith(
        'https://example.test/nflow/index.html?build=abc&model=%2Fmodules%2Fnflow_blocks%2Fexamples%2Fworkspace%2FFrom_Workspace_Demo.nflow'
      );
      backend.dispose();

      // An NFlow page acknowledges: nothing else opens.
      const received: unknown[] = [];
      const nflowPage = new FakeChannel('nelson-wasm-nflow');
      nflowPage.onmessage = ({ data }) => {
        const message = data as { type?: string; path?: string; token?: string };
        if (message.type === 'open-model') {
          received.push(message.path);
          nflowPage.postMessage({ type: 'open-model-ack', token: message.token });
        }
      };
      const second = clientFixture();
      const openExternalSecond = vi.fn();
      vi.mocked(second.client.evaluate).mockResolvedValueOnce({
        exitCode: 0,
        stdout: '',
        stderr: '',
        figures: [],
        uiActions: [action],
      });
      const secondBackend = new WasmDesktopBackend(second.client, {
        nflowUrl: 'https://example.test/nflow/index.html?build=abc',
        openExternal: openExternalSecond,
      });
      await secondBackend.handle('term.input', { line: 'open_system(f)' }, signal);
      await new Promise((resolve) => setTimeout(resolve, 50));
      expect(received).toEqual([action.path]);
      expect(openExternalSecond).not.toHaveBeenCalled();
      secondBackend.dispose();
      nflowPage.close();
    } finally {
      (globalThis as { BroadcastChannel: unknown }).BroadcastChannel = previous;
    }
  });

  it('refreshes the Files panel when another page of the shared engine writes files', () => {
    const fixture = clientFixture();
    let announce: ((event: { origin: string }) => void) | null = null;
    const client = {
      ...fixture.client,
      onFilesChanged: vi.fn((handler: (event: { origin: string }) => void) => {
        announce = handler;
        return () => {
          announce = null;
        };
      }),
    } as NelsonWorkerClientApi;
    const backend = new WasmDesktopBackend(client);
    const changes: unknown[] = [];
    const stop = onPush('fs.changed', (payload) => changes.push(payload));
    try {
      expect(client.onFilesChanged).toHaveBeenCalledTimes(1);
      announce?.({ origin: 'peer' });
      expect(changes).toEqual([{ path: '/workspace' }]);
    } finally {
      stop();
      backend.dispose();
    }
    // Disposing the backend detaches the listener.
    expect(announce).toBeNull();
  });

  it('advertises and opens the shared Package Manager panel when configured', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    const backend = new WasmDesktopBackend(fixture.client, {
      packageManagerUrl: 'https://example.test/nmm/index.html',
      runtimeCapabilities: runtimeCapabilities(),
    });

    await expect(backend.handle('app.capabilities', {}, signal)).resolves.toMatchObject({
      packageManager: true,
    });
    await expect(backend.handle('app.openPackageManager', {}, signal)).resolves.toEqual({
      opened: true,
    });
    expect(pushes).toContainEqual({ channel: 'tool.show', payload: { tool: 'nmm', args: {} } });
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it.each([undefined, '', 42])(
    'keeps the ping version when no build is known (%s)',
    async (buildId) => {
      const fixture = clientFixture();
      const backend = new WasmDesktopBackend(fixture.client, {
        deploymentManifest: buildId === undefined ? undefined : { buildId },
      });

      await expect(backend.handle('ping', {}, signal)).resolves.toEqual({
        pong: true,
        backend: 'WebAssembly Worker',
        version: 'portable',
      });
      backend.dispose();
    }
  );

  it('reports WebAssembly deployment diagnostics', async () => {
    const fixture = clientFixture();
    const originalCreateElement = document.createElement.bind(document);
    const createElement = vi.spyOn(document, 'createElement').mockImplementation((tagName) => {
      if (tagName.toLowerCase() === 'canvas') {
        return {
          getContext: (name: string) => (name === 'webgl' || name === 'webgl2' ? {} : null),
        } as HTMLCanvasElement;
      }
      return originalCreateElement(tagName);
    });
    const values = new Map<string, string>();
    const storage = {
      getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => values.set(key, value),
      removeItem: (key: string) => values.delete(key),
    };
    const capabilities = runtimeCapabilities();
    const backend = new WasmDesktopBackend(fixture.client, {
      runtimeCapabilities: capabilities,
      preferencesStorage: storage,
      deploymentManifest: {
        formatVersion: 1,
        buildId: 'abc123def456',
        totalBytes: 4096,
        files: [{ file: 'index.html', bytes: 1024 }],
        delivery: { formatVersion: 1, startup: [], onDemand: [] },
      },
      downloadPlan: { startupBytes: 3000, onDemandBytes: 1096 },
    });

    await expect(backend.handle('ping', {}, signal)).resolves.toEqual({
      pong: true,
      backend: 'WebAssembly Worker',
      version: 'portable (build abc123def456)',
    });
    await expect(backend.handle('diagnostics.info', {}, signal)).resolves.toMatchObject({
      backend: { name: 'WebAssembly Worker', version: 'portable', launchMode: 'browser' },
      runtime: { name: 'webassembly', profile: capabilities.profile, buildId: 'abc123def456' },
      delivery: { files: 1, totalBytes: 4096, startupBytes: 3000, onDemandBytes: 1096 },
      persistence: {
        commandHistory: true,
        preferencesPath: 'browser:localStorage/nelson-webview.wasm.preferences.v1',
        workspacePath: '/workspace',
      },
      capabilities: {
        modules: capabilities.modules,
        capabilities: capabilities.capabilities,
        features: capabilities.features,
        limits: capabilities.limits,
        unavailable: capabilities.unavailable,
      },
      browser: {
        features: expect.objectContaining({
          wasmCore: true,
          webGl: true,
          webGl2: true,
        }),
      },
    });
    createElement.mockRestore();
    backend.dispose();
  });

  it('keeps the platform-tagged archive name of a registry package in the workspace', async () => {
    const fixture = clientFixture();
    const checksum = 'b'.repeat(64);
    const marker = (
      value: unknown,
      onOutput?: Parameters<NelsonWorkerClientApi['evaluate']>[1]
    ) => {
      onOutput?.({
        stream: 'stdout',
        text: `__NELSON_NMM_RESULT_BEGIN__\n${JSON.stringify(value)}\n__NELSON_NMM_RESULT_END__\n`,
      });
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    };
    vi.mocked(fixture.client.evaluate).mockImplementation(async (code, onOutput) => {
      if (code.includes("nmm_gui_rpc('registry'"))
        return marker({ ok: true, verified: true }, onOutput);
      if (code.includes("nmm_gui_rpc('preview'")) {
        return marker(
          {
            ok: true,
            plan: {
              install_plan: {
                action: 'install',
                package: 'tessera',
                version: '1.0.0',
                source: 'https://packages.example.test/tessera-wasm32-1.0.0.nmz',
                checksum,
              },
            },
          },
          onOutput
        );
      }
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    });
    vi.stubGlobal(
      'fetch',
      vi.fn(async (input: string | URL | Request) => {
        const url = String(input);
        if (url.endsWith('.sha256')) return new Response(`${checksum}  registry.json\n`);
        if (url.endsWith('.sig')) return new Response('{"format":"test"}');
        if (url.endsWith('.nmz')) return new Response(new Uint8Array([9, 9]));
        return new Response('{"packages":[]}');
      })
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      packageManagerUrl: 'https://example.test/nmm/index.html',
      runtimeCapabilities: runtimeCapabilities(),
    });
    const dispatch = (
      backend as unknown as {
        dispatchPackageManagerRequest(
          method: string,
          params: Record<string, unknown>
        ): Promise<unknown>;
      }
    ).dispatchPackageManagerRequest.bind(backend);

    await expect(
      dispatch('install', {
        name: 'tessera',
        version: '1.0.0',
        browserRegistryUrl: 'https://packages.example.test/registry.json',
      })
    ).resolves.toEqual({ accepted: true });

    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/workspace/tessera-wasm32-1.0.0.nmz',
      new Uint8Array([9, 9])
    );
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/workspace/tessera-wasm32-1.0.0.nmz.sha256',
      new TextEncoder().encode(`${checksum}\n`)
    );
    expect(fixture.client.evaluate).toHaveBeenCalledWith(
      expect.stringContaining("nmm('install', '/workspace/tessera-wasm32-1.0.0.nmz')")
    );
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('downloads, validates, and installs a registry archive through the browser adapter', async () => {
    const fixture = clientFixture();
    const checksum = 'a'.repeat(64);
    const marker = (
      value: unknown,
      onOutput?: Parameters<NelsonWorkerClientApi['evaluate']>[1]
    ) => {
      onOutput?.({
        stream: 'stdout',
        text: `__NELSON_NMM_RESULT_BEGIN__\n${JSON.stringify(value)}\n__NELSON_NMM_RESULT_END__\n`,
      });
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    };
    vi.mocked(fixture.client.evaluate).mockImplementation(async (code, onOutput) => {
      if (code.includes("nmm_gui_rpc('registry'"))
        return marker({ ok: true, verified: true }, onOutput);
      if (code.includes("nmm_gui_rpc('preview'")) {
        return marker(
          {
            ok: true,
            plan: {
              install_plan: {
                action: 'install',
                package: 'morse',
                version: '1.0.0',
                source: 'https://packages.example.test/morse-1.0.0.nmz',
                checksum,
              },
            },
          },
          onOutput
        );
      }
      return { exitCode: 0, stdout: '', stderr: '', figures: [] };
    });
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      const url = String(input);
      if (url.endsWith('.sha256')) return new Response(`${checksum}  registry.json\n`);
      if (url.endsWith('.sig')) return new Response('{"format":"test"}');
      if (url.endsWith('.nmz')) return new Response(new Uint8Array([1, 2, 3, 4]));
      return new Response('{"packages":[]}');
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      packageManagerUrl: 'https://example.test/nmm/index.html',
      runtimeCapabilities: runtimeCapabilities(),
    });
    const dispatch = (
      backend as unknown as {
        dispatchPackageManagerRequest(
          method: string,
          params: Record<string, unknown>
        ): Promise<unknown>;
      }
    ).dispatchPackageManagerRequest.bind(backend);

    await expect(
      dispatch('install', {
        name: 'morse',
        version: '1.0.0',
        browserRegistryUrl: 'https://packages.example.test/registry.json',
      })
    ).resolves.toEqual({ accepted: true });

    expect(fetchMock).toHaveBeenCalledTimes(4);
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/workspace/morse-1.0.0.nmz',
      new Uint8Array([1, 2, 3, 4])
    );
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/workspace/morse-1.0.0.nmz.sha256',
      new TextEncoder().encode(`${checksum}\n`)
    );
    expect(fixture.client.evaluate).toHaveBeenCalledWith(
      expect.stringContaining("nmm('install', '/workspace/morse-1.0.0.nmz')")
    );
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('installs an uploaded workspace package without a network dependency', async () => {
    const fixture = clientFixture();
    const backend = new WasmDesktopBackend(fixture.client, {
      packageManagerUrl: 'https://example.test/nmm/index.html',
      runtimeCapabilities: runtimeCapabilities(),
    });
    const dispatch = (
      backend as unknown as {
        dispatchPackageManagerRequest(
          method: string,
          params: Record<string, unknown>
        ): Promise<unknown>;
      }
    ).dispatchPackageManagerRequest.bind(backend);

    await expect(
      dispatch('installSource', { source: '/workspace/example-1.0.0.nmz' })
    ).resolves.toEqual({ accepted: true });
    expect(fixture.client.evaluate).toHaveBeenCalledWith(
      "nmm('install', '/workspace/example-1.0.0.nmz');"
    );
    backend.dispose();
  });

  it('falls back to a same-origin package mirror when the archive host blocks fetch', async () => {
    const fixture = clientFixture();
    const checksum = 'a'.repeat(64);
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      const url = String(input);
      if (url.startsWith('https://packages.example.test/')) {
        throw new TypeError('Failed to fetch');
      }
      if (url.endsWith('.sha256')) return new Response(`${checksum}  example-1.0.0.nmz\n`);
      return new Response(new Uint8Array([1, 2, 3, 4]));
    });
    vi.stubGlobal('fetch', fetchMock);
    const backend = new WasmDesktopBackend(fixture.client, {
      packageManagerUrl: 'https://app.example.test/nmm/index.html',
      runtimeCapabilities: runtimeCapabilities(),
    });
    const dispatch = (
      backend as unknown as {
        dispatchPackageManagerRequest(
          method: string,
          params: Record<string, unknown>
        ): Promise<unknown>;
      }
    ).dispatchPackageManagerRequest.bind(backend);

    await expect(
      dispatch('installSource', { source: 'https://packages.example.test/example-1.0.0.nmz' })
    ).resolves.toEqual({ accepted: true });
    expect(fetchMock).toHaveBeenCalledWith(
      'https://app.example.test/nmm-packages/example-1.0.0.nmz'
    );
    expect(fetchMock).toHaveBeenCalledWith(
      'https://app.example.test/nmm-packages/example-1.0.0.nmz.sha256'
    );
    expect(fixture.client.writeFile).toHaveBeenCalledWith(
      '/workspace/example-1.0.0.nmz',
      new Uint8Array([1, 2, 3, 4])
    );
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('reports the browser hosting requirement when a package origin blocks fetch', async () => {
    const fixture = clientFixture();
    vi.stubGlobal(
      'fetch',
      vi.fn(async () => Promise.reject(new TypeError('Failed to fetch')))
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      packageManagerUrl: 'https://example.test/nmm/index.html',
      runtimeCapabilities: runtimeCapabilities(),
    });
    const dispatch = (
      backend as unknown as {
        dispatchPackageManagerRequest(
          method: string,
          params: Record<string, unknown>
        ): Promise<unknown>;
      }
    ).dispatchPackageManagerRequest.bind(backend);

    await expect(
      dispatch('installSource', { source: 'https://packages.example.test/example-1.0.0.nmz' })
    ).rejects.toThrow('host must allow cross-origin browser downloads');
    backend.dispose();
    vi.unstubAllGlobals();
  });

  it('evaluates demo() and opens the shared examples panel from its UI action', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
      exitCode: 0,
      stdout: '',
      stderr: '',
      figures: [],
      uiActions: [{ type: 'open-examples' }],
    });
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html',
    });

    await backend.handle('term.input', { line: 'demo();' }, signal);

    expect(pushes).toContainEqual({
      channel: 'tool.show',
      payload: { tool: 'examples', args: {} },
    });
    expect(fixture.client.evaluate).toHaveBeenCalledWith('demo();', expect.any(Function));
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('handles the demo UI action when a script invokes the portable function', async () => {
    const fixture = clientFixture();
    const pushes: { channel: string; payload: unknown }[] = [];
    const previousPush = window.__nelsonPush;
    window.__nelsonPush = (channel, payload) =>
      pushes.push({ channel, payload: JSON.parse(payload) as unknown });
    vi.mocked(fixture.client.evaluate).mockResolvedValueOnce({
      exitCode: 0,
      stdout: '',
      stderr: '',
      figures: [],
      uiActions: [{ type: 'open-examples' }],
    });
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html',
    });

    await backend.handle('term.input', { line: 'if true, demo(); end' }, signal);

    expect(pushes).toContainEqual({
      channel: 'tool.show',
      payload: { tool: 'examples', args: {} },
    });
    backend.dispose();
    window.__nelsonPush = previousPush;
  });

  it('acknowledges a demo on its first figure and rejects another run while busy', async () => {
    const fixture = clientFixture();
    let finishEvaluation: ((result: NelsonEvaluationResult) => void) | undefined;
    vi.mocked(fixture.client.evaluate).mockImplementationOnce(
      () =>
        new Promise<NelsonEvaluationResult>((resolve) => {
          finishEvaluation = resolve;
        })
    );
    vi.stubGlobal(
      'fetch',
      vi.fn(
        async () =>
          new Response(
            JSON.stringify({
              groups: [{ examples: [{ file: '/modules/graphics/examples/animated.m' }] }],
              diagnostics: [],
              locale: 'en_US',
            })
          )
      )
    );
    const backend = new WasmDesktopBackend(fixture.client, {
      examplesUrl: 'https://example.test/examples/index.html',
      historyStorage: null,
    });
    const dispatch = (
      backend as unknown as {
        dispatchExamplesRequest(method: string, params: Record<string, unknown>): Promise<unknown>;
      }
    ).dispatchExamplesRequest.bind(backend);

    const firstRun = dispatch('run', { path: '/modules/graphics/examples/animated.m' });
    await vi.waitFor(() => expect(fixture.client.evaluate).toHaveBeenCalledOnce());
    await expect(
      dispatch('run', { path: '/modules/graphics/examples/animated.m' })
    ).rejects.toThrow('Nelson is busy');

    fixture.sendFigure({
      handle: 0,
      displayList: { width: 640, height: 480, primitives: [] },
    });
    await expect(firstRun).resolves.toEqual({ accepted: true });

    finishEvaluation?.({ exitCode: 0, stdout: '', stderr: '', figures: [] });
    await vi.waitFor(async () => {
      await expect(backend.handle('term.input', { line: 'disp(42);' }, signal)).resolves.toEqual({
        accepted: true,
      });
    });
    backend.dispose();
    vi.unstubAllGlobals();
  });
});
//=============================================================================
