//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { createFigureWheelZoom } from '../../src/panels/figureWheelZoom';
import { isEngineBusy } from '../../src/desktop/engineSession';
import type { RotatableAxes } from '../../src/rpc/types';
//=============================================================================
vi.mock('../../src/desktop/engineSession', () => ({ isEngineBusy: vi.fn(() => false) }));
let frames: Map<number, FrameRequestCallback>, dispose: () => void;
beforeEach(() => {
  frames = new Map();
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
  dispose?.();
  vi.unstubAllGlobals();
  vi.useRealTimers();
});
//=============================================================================
function setup() {
  const stage = document.createElement('div'),
    canvas = document.createElement('canvas');
  stage.append(canvas);
  stage.getBoundingClientRect = () => ({ left: 0, top: 0, width: 1000, height: 500 } as DOMRect);
  const completions: (() => void)[] = [];
  const action = vi.fn(
    (_request: Record<string, unknown>) => new Promise<void>((resolve) => completions.push(resolve))
  );
  const zoom = createFigureWheelZoom({
    stage,
    axes: () => [{ handle: 22, rect: [0.5, 0, 0.5, 1], view: [-37.5, 30] }] as RotatableAxes[],
    selectAxes: vi.fn(),
    action,
  });
  stage.addEventListener('wheel', zoom.wheel);
  dispose = () => {
    zoom.dispose();
    stage.removeEventListener('wheel', zoom.wheel);
  };
  const wheel = (deltaY: number, options: WheelEventInit = {}, target: Element = canvas) => {
    const event = new WheelEvent('wheel', {
      deltaY,
      clientX: 700,
      clientY: 200,
      bubbles: true,
      cancelable: true,
      ...options,
    });
    target.dispatchEvent(event);
    return event;
  };
  return { wheel, action, zoom, stage, completions };
}
//=============================================================================
function paint() {
  const callbacks = [...frames.values()];
  frames.clear();
  callbacks.forEach((f) => f(0));
}
//=============================================================================
async function complete(resolve: () => void) {
  resolve();
  await Promise.resolve();
  await Promise.resolve();
}
//=============================================================================
it('zooms the pointed subplot and merges wheel movement behind one running action', async () => {
  const { wheel, action, completions } = setup();
  expect(wheel(-100).defaultPrevented).toBe(true);
  paint();
  expect(action.mock.calls[0][0]).toMatchObject({
    action: 'zoom',
    axesHandle: 22,
    factor: Math.exp(0.2),
  });
  expect(action.mock.calls[0][0].focusXFrac).toBeCloseTo(0.4, 12);
  expect(action.mock.calls[0][0].focusYFrac).toBeCloseTo(0.4, 12);
  for (let i = 0; i < 30; i++) {
    wheel(1);
    paint();
  }
  expect(action).toHaveBeenCalledTimes(1);
  await complete(completions[0]);
  paint();
  expect(action.mock.calls[1][0].factor).toBeCloseTo(Math.exp(-0.06), 12);
});
//=============================================================================
it('flushes remaining zoom before a subsequent Fit and cancels it on disposal', () => {
  const { wheel, action, zoom } = setup();
  wheel(-10);
  paint();
  wheel(-20);
  zoom.flush();
  void action({ action: 'fit' });
  expect(action.mock.calls.map(([r]) => r.action)).toEqual(['zoom', 'zoom', 'fit']);
  wheel(100);
  zoom.dispose();
  paint();
  expect(action).toHaveBeenCalledTimes(3);
});
//=============================================================================
it('keeps form scrolling, modified wheel events and plot margins untouched', () => {
  const { wheel, action, stage } = setup();
  const input = document.createElement('input');
  stage.append(input);
  expect(wheel(100, {}, input).defaultPrevented).toBe(false);
  expect(wheel(100, { ctrlKey: true }).defaultPrevented).toBe(false);
  expect(wheel(100, { clientX: 100 }).defaultPrevented).toBe(false);
  paint();
  expect(action).not.toHaveBeenCalled();
});
//=============================================================================
it('normalizes line deltas and queues one zoom after a busy-engine wheel burst', () => {
  vi.useFakeTimers();
  vi.mocked(isEngineBusy).mockReturnValue(true);
  const { wheel, action } = setup();
  for (let i = 0; i < 3; i++) {
    wheel(-1, { deltaMode: 1 });
    vi.advanceTimersByTime(30);
  }
  expect(action).not.toHaveBeenCalled();
  vi.advanceTimersByTime(120);
  expect(action).toHaveBeenCalledTimes(1);
  expect(action.mock.calls[0][0].factor).toBeCloseTo(Math.exp(48 * 0.002), 12);
});
//=============================================================================
