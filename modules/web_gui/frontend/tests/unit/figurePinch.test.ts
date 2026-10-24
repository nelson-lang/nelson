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
import { createFigurePinch } from '../../src/panels/figurePinch';
import type { RotatableAxes } from '../../src/rpc/types';
//=============================================================================
let frames: Map<number, FrameRequestCallback>;
beforeEach(() => {
  frames = new Map();
  let id = 0;
  vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => {
    frames.set(++id, callback);
    return id;
  });
  vi.stubGlobal('cancelAnimationFrame', (id: number) => frames.delete(id));
});
//=============================================================================
afterEach(() => vi.unstubAllGlobals());
function paint() {
  const callbacks = [...frames.values()];
  frames.clear();
  callbacks.forEach((callback) => callback(0));
}
//=============================================================================
const pointer = (pointerId: number, clientX: number, clientY: number) =>
  ({
    pointerId,
    pointerType: 'touch',
    clientX,
    clientY,
    preventDefault: () => undefined,
  } as unknown as PointerEvent);
//=============================================================================
function setup() {
  const stage = document.createElement('div');
  stage.getBoundingClientRect = () => ({ left: 0, top: 0, width: 1000, height: 600 } as DOMRect);
  const action = vi.fn((_request: Record<string, unknown>) => Promise.resolve());
  const selectAxes = vi.fn();
  const pinch = createFigurePinch({
    stage,
    axes: () => [{ handle: 5, rect: [0, 0, 1, 1], view: [0, 90] }] as RotatableAxes[],
    selectAxes,
    action,
  });
  return { pinch, action, selectAxes };
}
//=============================================================================
it('zooms by the pinch ratio anchored at the finger midpoint', () => {
  const { pinch, action, selectAxes } = setup();
  pinch.down(pointer(1, 400, 300));
  pinch.down(pointer(2, 600, 300));
  expect(selectAxes).toHaveBeenCalledWith(5);
  pinch.move(pointer(2, 700, 300));
  paint();
  expect(action).toHaveBeenCalledTimes(1);
  const request = action.mock.calls[0][0] as Record<string, number>;
  expect(request.action as unknown as string).toBe('zoom');
  expect(request.axesHandle).toBe(5);
  expect(request.factor).toBeCloseTo(1.5, 10);
  expect(request.focusXFrac).toBeCloseTo(0.55, 10);
  expect(request.focusYFrac).toBeCloseTo(0.5, 10);
});
//=============================================================================
it('ignores a single finger and stops when a finger lifts', () => {
  const { pinch, action } = setup();
  pinch.down(pointer(1, 400, 300));
  pinch.move(pointer(1, 500, 300));
  paint();
  expect(action).not.toHaveBeenCalled();
  pinch.down(pointer(2, 600, 300));
  pinch.up(pointer(2, 600, 300));
  pinch.move(pointer(1, 450, 300));
  paint();
  expect(action).not.toHaveBeenCalled();
});
//=============================================================================
it('ignores mouse pointers so the wheel path keeps zoom', () => {
  const { pinch, action } = setup();
  const mouse = {
    pointerId: 9,
    pointerType: 'mouse',
    clientX: 10,
    clientY: 10,
    preventDefault: () => undefined,
  } as unknown as PointerEvent;
  pinch.down(mouse);
  pinch.down(mouse);
  pinch.move(mouse);
  paint();
  expect(action).not.toHaveBeenCalled();
});
//=============================================================================
