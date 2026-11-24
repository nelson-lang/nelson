//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
import { useFigureInspector } from '../../src/panels/useFigureInspector';
//=============================================================================
const events = vi.hoisted(() => ({
  push: new Map<string, (payload: unknown) => void>(),
  ready: () => undefined as void,
  busy: false,
}));
//=============================================================================
vi.mock('../../src/rpc/client', () => ({
  rpc: vi.fn(async () => ({})),
  onPush: (name: string, callback: (payload: unknown) => void) => {
    events.push.set(name, callback);
    return () => events.push.delete(name);
  },
}));
//=============================================================================
vi.mock('../../src/desktop/engineSession', () => ({
  isEngineBusy: () => events.busy,
  onEngineReady: (callback: () => void) => {
    events.ready = callback;
    return () => undefined;
  },
}));
//=============================================================================
let host: HTMLDivElement;
let root: ReturnType<typeof createRoot>;
let inspector: ReturnType<typeof useFigureInspector>;
function Workspace({ open }: { open: boolean }) {
  inspector = useFigureInspector(4, () => undefined, open);
  return <span>{inspector.state?.name}</span>;
}
//=============================================================================
beforeEach(() => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  vi.useFakeTimers();
  vi.mocked(rpc).mockClear();
  events.busy = false;
  host = document.createElement('div');
  root = createRoot(host);
});
//=============================================================================
afterEach(() => {
  act(() => root.unmount());
  vi.mocked(rpc).mockReset();
  vi.mocked(rpc).mockResolvedValue({});
  vi.useRealTimers();
  vi.unstubAllGlobals();
});
//=============================================================================
it('does not poll closed properties on animation frames, updates or command completion', async () => {
  await act(async () => root.render(<Workspace open={false} />));
  await act(async () => {
    events.ready();
    for (let frame = 0; frame < 60; frame++) {
      events.push.get('figure.frame')?.({ handle: 4 });
      events.push.get('figure.updated')?.({ handle: 4 });
      await vi.advanceTimersByTimeAsync(20);
    }
    await inspector.refresh();
  });
  expect(rpc).not.toHaveBeenCalled();
  await act(async () => inspector.action({ action: 'fit' }));
  expect(rpc).toHaveBeenCalledWith('figure.action', {
    handle: 4,
    request: { axesHandle: 0, action: 'fit' },
  });
});
//=============================================================================
it('refreshes on opening, cancels scheduled polling on closing and resumes with fresh state', async () => {
  vi.mocked(rpc).mockResolvedValue({
    state: { handle: 4, name: 'Current', axes: [], color: [], numberTitle: 'on' },
  });
  await act(async () => root.render(<Workspace open={false} />));
  expect(rpc).not.toHaveBeenCalled();
  await act(async () => root.render(<Workspace open />));
  expect(rpc).toHaveBeenCalledOnce();
  expect(host.textContent).toBe('Current');
  act(() => events.push.get('figure.frame')?.({ handle: 4 }));
  await act(async () => root.render(<Workspace open={false} />));
  await act(async () => vi.advanceTimersByTimeAsync(250));
  expect(rpc).toHaveBeenCalledOnce();
  await act(async () => root.render(<Workspace open />));
  expect(rpc).toHaveBeenCalledTimes(2);
  events.busy = true;
  await act(async () => {
    for (let frame = 0; frame < 60; frame++) {
      events.push.get('figure.frame')?.({ handle: 4 });
      events.push.get('figure.updated')?.({ handle: 4 });
      await vi.advanceTimersByTimeAsync(20);
    }
    await vi.advanceTimersByTimeAsync(250);
  });
  expect(rpc).toHaveBeenCalledTimes(2);
  events.busy = false;
  await act(async () => events.ready());
  expect(rpc).toHaveBeenCalledTimes(3);
});
//=============================================================================
