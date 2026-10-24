//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, it, vi } from 'vitest';
import type { NelsonWorkerClientApi } from '@nelson/wasm-client';
import { WasmDesktopBackend } from '../../src/wasm/WasmDesktopBackend';
import { setWasmFigureView } from '../../src/wasm/figureView';
import { onPush } from '../../src/rpc/client';
//=============================================================================
it('routes enhanced figure actions and grouped rotation through the shared Worker engine', async () => {
  const frame = { handle: 0, width: 400, height: 300, primitives: [], view: [12, 34] };
  const client = {
    terminate: vi.fn(),
    onFigure: vi.fn(() => () => undefined),
    onAudioAction: vi.fn(() => () => undefined),
    onDebuggerState: vi.fn(() => () => undefined),
    onInputRequest: vi.fn(() => () => undefined),
    figureAction: vi.fn(async () => ({
      displayList: frame,
      state: { canUndo: true, canRedo: false, axes: [] },
    })),
  };
  const backend = new WasmDesktopBackend(client as unknown as NelsonWorkerClientApi);
  const signal = new AbortController().signal;
  try {
    const request = {
      axesHandle: 42,
      azimuth: 12,
      elevation: 34,
      compactView: true,
      historyGroup: 'rotation',
      softwareFallback: false,
    };
    await expect(
      backend.handle('figure.setView', { handle: 0, ...request }, signal)
    ).resolves.toMatchObject(frame);
    expect(client.figureAction).toHaveBeenLastCalledWith(0, { action: 'view', ...request });
    for (const action of [
      { action: 'undo' },
      { action: 'redo' },
      { action: 'isolate', objectHandle: 77 },
      { action: 'step', objectHandle: 77, index: 2 },
      { action: 'properties', objectHandle: 77, properties: { LineStyle: '--', Marker: 'o' } },
    ]) {
      await backend.handle('figure.action', { handle: 0, request: action }, signal);
      expect(client.figureAction).toHaveBeenLastCalledWith(0, action);
    }
  } finally {
    backend.dispose();
  }
});
//=============================================================================
it('queues busy grouped rotation with stale-target validation', async () => {
  const frame = { handle: 0, width: 400, height: 300, primitives: [] };
  const client = { queueCommand: vi.fn(), figureAction: vi.fn() };
  const accept = vi.fn();
  const request = {
    axesHandle: 42,
    azimuth: 12,
    elevation: 34,
    compactView: true,
    historyGroup: 'rotation',
    softwareFallback: false,
  };
  const result = await setWasmFigureView(request, {
    client: client as unknown as NelsonWorkerClientApi,
    handle: 0,
    busy: true,
    current: () => frame,
    accept,
    text: (value) => `'${value}'`,
    finite: Number,
  });
  expect(result).toBe(frame);
  expect(client.queueCommand).toHaveBeenCalledExactlyOnceWith(
    `__web_figure_deferred_action__(0, '${JSON.stringify({ action: 'view', ...request })}');`
  );
  expect(client.figureAction).not.toHaveBeenCalled();
  expect(accept).not.toHaveBeenCalled();
});
//=============================================================================
it('queues busy view updates through the shared deferred action path', async () => {
  const frame = { handle: 0, width: 400, height: 300, primitives: [] };
  const client = { queueCommand: vi.fn(), figureAction: vi.fn() };
  const accept = vi.fn((next) => next);
  const request = {
    axesHandle: 42,
    azimuth: 12,
    elevation: 34,
    compactView: true,
  };
  const result = await setWasmFigureView(request, {
    client: client as unknown as NelsonWorkerClientApi,
    handle: 0,
    busy: true,
    current: () => frame,
    accept,
    text: (value) => `'${value}'`,
    finite: Number,
  });
  expect(result).toMatchObject({ ...frame, view: [12, 34] });
  expect(client.queueCommand).toHaveBeenCalledExactlyOnceWith(
    `__web_figure_deferred_action__(0, '${JSON.stringify({
      action: 'view',
      ...request,
      softwareFallback: true,
    })}');`
  );
  expect(client.figureAction).not.toHaveBeenCalled();
  expect(accept).toHaveBeenCalledExactlyOnceWith({ ...frame, view: [12, 34] });
});
//=============================================================================
it('keeps interactive Notebook figure actions inline', async () => {
  const frame = { handle: 7, width: 400, height: 300, primitives: [], view: [12, 34] };
  const client = {
    terminate: vi.fn(),
    onFigure: vi.fn(() => () => undefined),
    onAudioAction: vi.fn(() => () => undefined),
    onDebuggerState: vi.fn(() => () => undefined),
    onInputRequest: vi.fn(() => () => undefined),
    figureAction: vi.fn(async () => ({ displayList: frame })),
  };
  const pushed: unknown[] = [];
  const off = onPush('figure.frame', (payload) => pushed.push(payload));
  const backend = new WasmDesktopBackend(client as unknown as NelsonWorkerClientApi);
  try {
    const signal = new AbortController().signal;
    (backend as unknown as { notebookFigures: Set<number> }).notebookFigures.add(frame.handle);
    await backend.handle(
      'figure.action',
      { handle: frame.handle, request: { action: 'state' } },
      signal
    );
    expect(client.figureAction).toHaveBeenCalledWith(frame.handle, { action: 'state' });
    expect(pushed).toHaveLength(0);
    for (const request of [
      { action: 'zoom', factor: 1.1 },
      { action: 'pan', dxFrac: 0.1, dyFrac: 0 },
      { action: 'fit' },
      { action: 'view', azimuth: 20, elevation: 30 },
    ]) {
      await backend.handle('figure.action', { handle: frame.handle, request }, signal);
      expect(pushed).toHaveLength(0);
    }
  } finally {
    off();
    backend.dispose();
  }
});
//=============================================================================
