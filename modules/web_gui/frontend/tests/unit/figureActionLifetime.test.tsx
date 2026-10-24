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
import { useFigureInspector } from '../../src/panels/useFigureInspector';
import { rpc } from '../../src/rpc/client';
import { withFigureSlot } from '../../src/desktop/figureQueue';
//=============================================================================
vi.mock('../../src/rpc/client', () => ({ rpc: vi.fn(), onPush: vi.fn(() => () => undefined) }));
vi.mock('../../src/desktop/engineSession', () => ({
  isEngineBusy: () => false,
  onEngineReady: () => () => undefined,
}));
//=============================================================================
let dispose: () => void;
afterEach(() => {
  dispose?.();
  vi.mocked(rpc).mockReset();
  vi.unstubAllGlobals();
});
//=============================================================================
async function workspace() {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  vi.mocked(rpc).mockResolvedValue({});
  const host = document.createElement('div');
  document.body.append(host);
  const root = createRoot(host),
    apply = vi.fn();
  let inspector: ReturnType<typeof useFigureInspector>;
  function Workspace({ handle }: { handle: number }) {
    inspector = useFigureInspector(handle, apply);
    return null;
  }
  await act(async () => {
    root.render(<Workspace handle={0} />);
  });
  vi.mocked(rpc).mockClear();
  const unmount = () => {
    act(() => root.unmount());
    host.remove();
  };
  dispose = unmount;
  return {
    apply,
    action: (action: string) => inspector!.action({ action }),
    replace: () =>
      act(async () => {
        root.render(<Workspace handle={1} />);
      }),
    close: () => {
      unmount();
      dispose = () => undefined;
    },
  };
}
//=============================================================================
it('drops queued actions and ignores a completed frame after closing', async () => {
  const w = await workspace();
  let finish!: (value: unknown) => void;
  vi.mocked(rpc).mockImplementation(
    () =>
      new Promise((resolve) => {
        finish = resolve;
      })
  );
  let first!: Promise<void>, second!: Promise<void>;
  await act(async () => {
    first = w.action('fit');
    second = w.action('restore');
  });
  expect(rpc).toHaveBeenCalledOnce();
  w.close();
  await act(async () => {
    finish({ displayList: { primitives: [] } });
    await Promise.all([first, second]);
  });
  expect(rpc).toHaveBeenCalledOnce();
  expect(w.apply).not.toHaveBeenCalled();
  await w.action('pan');
  expect(rpc).toHaveBeenCalledOnce();
});
//=============================================================================
it('ignores a rejected action after closing without scheduling a refresh', async () => {
  const w = await workspace();
  let fail!: (value: Error) => void;
  vi.mocked(rpc).mockImplementation(
    () =>
      new Promise((_resolve, reject) => {
        fail = reject;
      })
  );
  let pending!: Promise<void>;
  await act(async () => {
    pending = w.action('fit');
  });
  w.close();
  await act(async () => {
    fail(new Error('Figure no longer exists'));
    await pending;
  });
  expect(rpc).toHaveBeenCalledOnce();
  expect(w.apply).not.toHaveBeenCalled();
});
//=============================================================================
it('does not send old queued actions to a replacement figure', async () => {
  const w = await workspace();
  let finish!: (value: unknown) => void;
  vi.mocked(rpc).mockImplementationOnce(
    () =>
      new Promise((resolve) => {
        finish = resolve;
      })
  );
  let first!: Promise<void>, second!: Promise<void>;
  await act(async () => {
    first = w.action('fit');
    second = w.action('restore');
  });
  await w.replace();
  await act(async () => {
    finish({ displayList: { primitives: [] } });
    await Promise.all([first, second]);
  });
  expect(w.apply).not.toHaveBeenCalled();
  expect(rpc).toHaveBeenCalledTimes(2);
  expect(rpc).toHaveBeenLastCalledWith('figure.action', {
    handle: 1,
    request: { action: 'state' },
  });
});
//=============================================================================
it('cancels an action waiting for a shared graphics slot when closed', async () => {
  const w = await workspace();
  const releases: (() => void)[] = [];
  const blockers = [0, 1].map(() =>
    withFigureSlot(() => new Promise<void>((resolve) => releases.push(resolve)))
  );
  let pending!: Promise<void>;
  await act(async () => {
    pending = w.action('fit');
  });
  expect(rpc).not.toHaveBeenCalled();
  w.close();
  await act(async () => {
    releases.forEach((release) => release());
    await Promise.all([...blockers, pending]);
  });
  expect(rpc).not.toHaveBeenCalled();
  expect(w.apply).not.toHaveBeenCalled();
});
//=============================================================================
