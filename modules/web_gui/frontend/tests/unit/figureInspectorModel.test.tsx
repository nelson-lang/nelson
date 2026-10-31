//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { rpc } from '../../src/rpc/client';
import { useInspectorModel } from '../../src/panels/useInspectorModel';
import type { InspectorPayload } from '../../src/rpc/figureTypes';
//=============================================================================
const events = vi.hoisted(() => ({
  push: new Map<string, (payload: unknown) => void>(),
  busy: false,
}));
//=============================================================================
vi.mock('../../src/rpc/client', () => ({
  rpc: vi.fn(),
  onPush: (name: string, callback: (payload: unknown) => void) => {
    events.push.set(name, callback);
    return () => events.push.delete(name);
  },
}));
//=============================================================================
vi.mock('../../src/desktop/engineSession', () => ({
  isEngineBusy: () => events.busy,
  onEngineReady: () => () => undefined,
}));
//=============================================================================
// Each inspect request is answered by hand, so the test controls the order in
// which the engine replies arrive, as parallel server workers do.
type Pending = { objectHandle: number; resolve: (payload: unknown) => void };
const pending: Pending[] = [];
const tree = { handle: 0, type: 'figure', label: '', children: [] };
function answer(request: Pending) {
  pending.splice(pending.indexOf(request), 1);
  request.resolve({
    model: { handle: request.objectHandle, type: 'axes', label: '', properties: [] },
    tree,
  });
}
//=============================================================================
let host: HTMLDivElement;
let root: ReturnType<typeof createRoot>;
let payload: InspectorPayload | null = null;
function Inspector({ selected }: { selected: number }) {
  payload = useInspectorModel(4, selected, true);
  return <span>{payload?.model.handle}</span>;
}
//=============================================================================
beforeEach(() => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  events.busy = false;
  pending.length = 0;
  payload = null;
  vi.mocked(rpc).mockImplementation(
    (_method, params) =>
      new Promise((resolve) => {
        const request = (params as { request: { objectHandle: number } }).request;
        pending.push({ objectHandle: request.objectHandle, resolve });
      })
  );
  host = document.createElement('div');
  root = createRoot(host);
});
//=============================================================================
afterEach(() => {
  act(() => root.unmount());
  vi.mocked(rpc).mockReset();
  vi.unstubAllGlobals();
});
//=============================================================================
it('ignores the model of an object that was deselected before its reply arrived', async () => {
  await act(async () => root.render(<Inspector selected={11} />));
  expect(pending.map((request) => request.objectHandle)).toEqual([11]);
  const stale = pending[0];
  await act(async () => root.render(<Inspector selected={22} />));
  expect(pending.map((request) => request.objectHandle)).toEqual([11, 22]);
  await act(async () => answer(pending[1]));
  expect(payload?.model.handle).toBe(22);
  // the engine answers the earlier request last: it must not replace the selection
  await act(async () => answer(stale));
  expect(payload?.model.handle).toBe(22);
});
//=============================================================================
it('keeps the newest reply when refreshes of the same object are answered out of order', async () => {
  await act(async () => root.render(<Inspector selected={11} />));
  const first = pending[0];
  await act(async () => answer(first));
  expect(payload?.model.handle).toBe(11);
  // a figure update while a refresh is in flight queues a single later refresh
  vi.useFakeTimers();
  await act(async () => {
    events.push.get('figure.updated')?.({ handle: 4 });
    await vi.advanceTimersByTimeAsync(250);
  });
  expect(pending.map((request) => request.objectHandle)).toEqual([11]);
  const second = pending[0];
  await act(async () => {
    events.push.get('figure.frame')?.({ handle: 4 });
    events.push.get('figure.updated')?.({ handle: 4 });
    await vi.advanceTimersByTimeAsync(250);
  });
  expect(pending.length).toBe(1);
  const before = payload;
  await act(async () => answer(second));
  expect(payload).not.toBe(before);
  // the deferred refresh runs once the first one has been answered
  expect(pending.map((request) => request.objectHandle)).toEqual([11]);
  vi.useRealTimers();
});
//=============================================================================
