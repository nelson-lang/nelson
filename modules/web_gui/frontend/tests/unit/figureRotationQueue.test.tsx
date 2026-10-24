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
import { useFigureNavigation } from '../../src/panels/useFigureNavigation';
import { rpc } from '../../src/rpc/client';
import type { FigureDisplayListResult } from '../../src/rpc/types';
//=============================================================================
vi.mock('../../src/rpc/client', () => ({ rpc: vi.fn(), onPush: vi.fn(() => () => undefined) }));
let dispose: () => void;
afterEach(() => {
  dispose?.();
  vi.mocked(rpc).mockReset();
  vi.unstubAllGlobals();
});
//=============================================================================
function workspace() {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  const frames = new Map<number, FrameRequestCallback>();
  let id = 0;
  vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => {
    frames.set(++id, callback);
    return id;
  });
  vi.stubGlobal('cancelAnimationFrame', (id: number) => frames.delete(id));
  const completions: ((frame: FigureDisplayListResult) => void)[] = [];
  vi.mocked(rpc).mockImplementation(() => new Promise((resolve) => completions.push(resolve)));
  const host = document.createElement('div');
  document.body.append(host);
  const root = createRoot(host);
  let navigation: ReturnType<typeof useFigureNavigation>;
  const frame = {
    handle: 0,
    width: 400,
    height: 300,
    is3d: true,
    view: [-37.5, 30],
    primitives: [],
  } as FigureDisplayListResult;
  const apply = vi.fn(),
    action = vi.fn(async () => undefined),
    refresh = vi.fn(async () => undefined);
  function Workspace({ handle }: { handle: number }) {
    navigation = useFigureNavigation({
      handle,
      dl: frame,
      displayListParams: () => ({ handle }),
      apply,
      axes: null,
      selectAxes: vi.fn(),
      action,
      refreshProperties: refresh,
    });
    return <div onMouseDown={navigation.startDrag}>Figure</div>;
  }
  act(() => root.render(<Workspace handle={0} />));
  const unmount = () => {
    act(() => root.unmount());
    host.remove();
  };
  dispose = unmount;
  const move = (x = 110) =>
    act(() => window.dispatchEvent(new MouseEvent('mousemove', { clientX: x, clientY: 120 })));
  const up = () => act(() => window.dispatchEvent(new MouseEvent('mouseup')));
  const start = () =>
    act(() =>
      host.firstElementChild!.dispatchEvent(
        new MouseEvent('mousedown', { bubbles: true, clientX: 100, clientY: 100 })
      )
    );
  const tick = () =>
    act(async () => {
      frames.forEach((callback) => callback(0));
      frames.clear();
      await Promise.resolve();
    });
  const complete = () =>
    act(async () => {
      completions.shift()!(frame);
    });
  const command = () =>
    navigation!.finishRotation().then((active) => {
      if (active) return action();
    });
  return {
    start,
    move,
    up,
    tick,
    complete,
    command,
    apply,
    action,
    refresh,
    replace: () => act(() => root.render(<Workspace handle={1} />)),
    close: () => {
      unmount();
      dispose = () => undefined;
    },
  };
}
//=============================================================================
it('waits for the final coalesced rotation before restoring or zooming the view', async () => {
  const w = workspace();
  w.start();
  w.move();
  await w.tick();
  expect(rpc).toHaveBeenCalledOnce();
  w.move(140);
  w.up();
  const restored = w.command();
  expect(w.action).not.toHaveBeenCalled();
  await w.complete();
  expect(rpc).toHaveBeenCalledTimes(2);
  expect(rpc).toHaveBeenLastCalledWith(
    'figure.setView',
    expect.objectContaining({ compactView: false, azimuth: -57.5, elevation: 40 })
  );
  expect(w.action).not.toHaveBeenCalled();
  await w.complete();
  await restored;
  expect(w.action).toHaveBeenCalledOnce();
  expect(w.apply).toHaveBeenCalledTimes(2);
});
//=============================================================================
it.each(['Fit', 'Restore'])(
  'ends a held rotation before %s even when the compact RPC is idle',
  async () => {
    const w = workspace();
    w.start();
    w.move();
    await w.tick();
    await w.complete();
    const finished = w.command();
    await w.tick();
    expect(rpc).toHaveBeenCalledTimes(2);
    expect(w.action).not.toHaveBeenCalled();
    await w.complete();
    await finished;
    w.move(200);
    w.up();
    await w.tick();
    expect(w.action).toHaveBeenCalledOnce();
    expect(rpc).toHaveBeenCalledTimes(2);
  }
);
//=============================================================================
it('discards a queued final rotation and waiting command when the figure closes', async () => {
  const w = workspace();
  w.start();
  w.move();
  await w.tick();
  const finished = w.command();
  w.close();
  await w.complete();
  await finished;
  expect(rpc).toHaveBeenCalledOnce();
  expect(w.apply).not.toHaveBeenCalled();
  expect(w.refresh).not.toHaveBeenCalled();
  expect(w.action).not.toHaveBeenCalled();
});
//=============================================================================
it('does not apply an old rotation to a replacement figure handle', async () => {
  const w = workspace();
  w.start();
  w.move();
  await w.tick();
  w.replace();
  await w.complete();
  expect(w.apply).not.toHaveBeenCalled();
  expect(w.refresh).not.toHaveBeenCalled();
  expect(rpc).toHaveBeenCalledOnce();
});
//=============================================================================
it('finishes a rotation when the window loses focus', async () => {
  const w = workspace();
  w.start();
  w.move();
  await w.tick();
  await w.complete();
  act(() => window.dispatchEvent(new Event('blur')));
  await w.tick();
  expect(rpc).toHaveBeenCalledTimes(2);
  expect(rpc).toHaveBeenLastCalledWith(
    'figure.setView',
    expect.objectContaining({ compactView: false })
  );
  await w.complete();
});
//=============================================================================
