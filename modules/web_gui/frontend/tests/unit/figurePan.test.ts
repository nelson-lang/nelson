//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { MouseEvent as ReactMouseEvent } from 'react';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { isEngineBusy } from '../../src/desktop/engineSession';
import { startFigurePan } from '../../src/panels/figurePan';
import type { FigureDisplayListResult } from '../../src/rpc/types';
//=============================================================================
vi.mock('../../src/desktop/engineSession', () => ({ isEngineBusy: vi.fn(() => false) }));
let frames: Map<number, FrameRequestCallback>;
let cancelDrag: { current: (() => void) | null };
beforeEach(() => {
  frames = new Map();
  cancelDrag = { current: null };
  let id = 0;
  vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => {
    frames.set(++id, callback);
    return id;
  });
  vi.stubGlobal('cancelAnimationFrame', (id: number) => frames.delete(id));
  vi.mocked(isEngineBusy).mockReturnValue(false);
});
//=============================================================================
afterEach(() => {
  cancelDrag.current?.();
  vi.unstubAllGlobals();
});
//=============================================================================
function start() {
  const target = document.createElement('div');
  target.getBoundingClientRect = () => ({ left: 0, top: 0, width: 1000, height: 600 } as DOMRect);
  const completions: (() => void)[] = [];
  const action = vi.fn(
    (_request: Record<string, unknown>) => new Promise<void>((resolve) => completions.push(resolve))
  );
  const selectAxes = vi.fn();
  const dl = {
    rotatableAxes: [
      { handle: 11, rect: [0, 0, 0.5, 0.5] },
      { handle: 22, rect: [0.5, 0, 0.5, 0.5] },
    ],
  } as FigureDisplayListResult;
  startFigurePan(
    {
      button: 0,
      clientX: 700,
      clientY: 150,
      currentTarget: target,
      preventDefault: () => undefined,
    } as unknown as ReactMouseEvent,
    { dl, axesHandle: 99, selectAxes, action, cancelDrag }
  );
  return { action, completions, selectAxes };
}
//=============================================================================
function move(x: number, y = 150) {
  window.dispatchEvent(new MouseEvent('mousemove', { clientX: x, clientY: y }));
}
//=============================================================================
function up(x: number, y = 150) {
  window.dispatchEvent(new MouseEvent('mouseup', { clientX: x, clientY: y }));
}
//=============================================================================
function paint() {
  const callbacks = [...frames.values()];
  frames.clear();
  callbacks.forEach((callback) => callback(0));
}
//=============================================================================
async function complete(resolve: () => void) {
  resolve();
  await Promise.resolve();
  await Promise.resolve();
}
//=============================================================================
it('updates the picked subplot while held and coalesces movements behind the running action', async () => {
  const { action, completions, selectAxes } = start();
  expect(selectAxes).toHaveBeenCalledWith(22);
  for (let x = 701; x <= 750; x++) move(x, 180);
  expect(frames.size).toBe(1);
  paint();
  expect(action.mock.calls.map(([request]) => request)).toEqual([
    {
      action: 'pan',
      axesHandle: 22,
      dxFrac: 0.1,
      dyFrac: 0.1,
      historyGroup: expect.any(String),
      compactView: true,
    },
  ]);
  for (let x = 751; x <= 800; x++) {
    move(x, 210);
    paint();
  }
  expect(action).toHaveBeenCalledTimes(1);
  await complete(completions[0]);
  paint();
  expect(action.mock.calls[1][0]).toEqual({
    action: 'pan',
    axesHandle: 22,
    dxFrac: 0.1,
    dyFrac: 0.1,
    historyGroup: action.mock.calls[0][0].historyGroup,
    compactView: true,
  });
  up(800, 210);
  await complete(completions[1]);
  paint();
  expect(action).toHaveBeenCalledTimes(2);
});
//=============================================================================
it('queues the final delta before Fit without counting the already sent movement twice', async () => {
  const { action, completions } = start();
  move(750);
  paint();
  move(800);
  up(820);
  void action({ action: 'fit' });
  expect(action.mock.calls.map(([request]) => request)).toEqual([
    {
      action: 'pan',
      axesHandle: 22,
      dxFrac: 0.1,
      dyFrac: 0,
      historyGroup: expect.any(String),
      compactView: true,
    },
    {
      action: 'pan',
      axesHandle: 22,
      dxFrac: 0.14,
      dyFrac: 0,
      historyGroup: action.mock.calls[0][0].historyGroup,
      compactView: true,
    },
    { action: 'fit' },
  ]);
  await complete(completions[0]);
  paint();
  expect(action).toHaveBeenCalledTimes(3);
});
//=============================================================================
it('compensates a return to the initial pointer position after an intermediate update', () => {
  const { action } = start();
  move(750);
  paint();
  up(700);
  expect(action.mock.calls[1][0].dxFrac).toBe(-0.1);
});
//=============================================================================
it('cancels queued pointer work when the figure is closed or another drag starts', async () => {
  const { action, completions } = start();
  move(750);
  paint();
  move(800);
  cancelDrag.current?.();
  await complete(completions[0]);
  move(850);
  up(850);
  paint();
  expect(action).toHaveBeenCalledTimes(1);
  expect(frames.size).toBe(0);
});
//=============================================================================
it('queues only the final displacement when the interpreter is busy', () => {
  vi.mocked(isEngineBusy).mockReturnValue(true);
  const { action } = start();
  for (let x = 701; x <= 800; x++) {
    move(x);
    paint();
  }
  expect(action).not.toHaveBeenCalled();
  up(800);
  expect(action.mock.calls[0][0].dxFrac).toBe(0.2);
});
//=============================================================================
