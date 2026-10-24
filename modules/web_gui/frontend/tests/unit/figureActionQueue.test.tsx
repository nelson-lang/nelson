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
import { afterEach, expect, it, vi } from 'vitest';
import { rpc } from '../../src/rpc/client';
import { useFigureInspector } from '../../src/panels/useFigureInspector';
//=============================================================================
vi.mock('../../src/rpc/client', () => ({ rpc: vi.fn(), onPush: () => () => undefined }));
vi.mock('../../src/desktop/figureQueue', () => ({ withFigureSlot: (run: () => unknown) => run() }));
vi.mock('../../src/desktop/engineSession', () => ({
  isEngineBusy: () => false,
  onEngineReady: () => () => undefined,
}));
//=============================================================================
afterEach(() => vi.unstubAllGlobals());
it('keeps pending visible until every queued camera action, including Fit, has finished', async () => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  const completions: ((value: unknown) => void)[] = [];
  const actions: string[] = [];
  vi.mocked(rpc).mockImplementation(async (_method, params) => {
    const action = (params?.request as { action: string }).action;
    if (action === 'state') return {} as never;
    actions.push(action);
    return new Promise((resolve) => completions.push(resolve)) as never;
  });
  const host = document.createElement('div');
  const root = createRoot(host);
  let inspector: ReturnType<typeof useFigureInspector>;
  function Workspace() {
    inspector = useFigureInspector(0, () => undefined);
    return <span>{inspector.pending ? 'Pending' : 'Ready'}</span>;
  }
  try {
    await act(async () => root.render(<Workspace />));
    expect(host.textContent).toBe('Ready');
    await act(async () => {
      void inspector.action({ action: 'properties', properties: { Projection: 'perspective' } });
      void inspector.action({ action: 'view', azimuth: -37.5, elevation: 30 });
      void inspector.action({ action: 'fit' });
    });
    expect(actions).toEqual(['properties']);
    expect(host.textContent).toBe('Pending');
    await act(async () => completions[0]({}));
    expect(actions).toEqual(['properties', 'view']);
    expect(host.textContent).toBe('Pending');
    await act(async () => completions[1]({}));
    expect(actions).toEqual(['properties', 'view', 'fit']);
    expect(host.textContent).toBe('Pending');
    await act(async () => completions[2]({}));
    expect(host.textContent).toBe('Ready');
  } finally {
    act(() => root.unmount());
    vi.mocked(rpc).mockReset();
  }
});
//=============================================================================
