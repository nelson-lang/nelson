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
import { startFigureZoomRegion } from '../../src/panels/figureZoomRegion';
import type { FigureDisplayListResult } from '../../src/rpc/types';
//=============================================================================
let cancelDrag: { current: (() => void) | null };
beforeEach(() => {
  cancelDrag = { current: null };
});
//=============================================================================
afterEach(() => {
  cancelDrag.current?.();
});
//=============================================================================
function start(options: { is3d?: boolean; shiftKey?: boolean } = {}) {
  const overlay = document.createElement('div');
  overlay.getBoundingClientRect = () => ({ left: 0, top: 0, width: 1000, height: 600 } as DOMRect);
  document.body.appendChild(overlay);
  const action = vi.fn((_request: Record<string, unknown>) => Promise.resolve());
  const selectAxes = vi.fn();
  const dl = {
    is3d: !!options.is3d,
    rotatableAxes: [{ handle: 7, rect: [0.5, 0, 0.5, 1], view: [0, 90] }],
  } as FigureDisplayListResult;
  startFigureZoomRegion(
    {
      button: 0,
      clientX: 600,
      clientY: 150,
      shiftKey: !!options.shiftKey,
      currentTarget: overlay,
      preventDefault: () => undefined,
    } as unknown as ReactMouseEvent,
    { dl, axesHandle: 0, selectAxes, action, cancelDrag }
  );
  return { overlay, action, selectAxes };
}
//=============================================================================
const move = (x: number, y: number) =>
  window.dispatchEvent(new MouseEvent('mousemove', { clientX: x, clientY: y }));
const up = (x: number, y: number) =>
  window.dispatchEvent(new MouseEvent('mouseup', { clientX: x, clientY: y }));
it('maps a rubber-band box to axes-relative fractions', () => {
  const { overlay, action, selectAxes } = start();
  expect(selectAxes).toHaveBeenCalledWith(7);
  expect(overlay.querySelector('.figure-panel__zoomband')).not.toBeNull();
  move(850, 450);
  up(850, 450);
  expect(action).toHaveBeenCalledTimes(1);
  const request = action.mock.calls[0][0] as Record<string, number[]>;
  expect(request.action).toBe('zoom');
  expect(request.axesHandle).toBe(7);
  expect((request.regionXFrac as number[])[0]).toBeCloseTo(0.2, 12);
  expect((request.regionXFrac as number[])[1]).toBeCloseTo(0.7, 12);
  expect((request.regionYFrac as number[])[0]).toBeCloseTo(0.25, 12);
  expect((request.regionYFrac as number[])[1]).toBeCloseTo(0.75, 12);
  expect(overlay.querySelector('.figure-panel__zoomband')).toBeNull();
});
//=============================================================================
it('treats a click as a point zoom-in anchored under the cursor', () => {
  const { action } = start();
  up(601, 151);
  expect(action).toHaveBeenCalledTimes(1);
  const request = action.mock.calls[0][0] as Record<string, number>;
  expect(request.factor).toBe(2);
  expect(request.regionXFrac).toBeUndefined();
  expect(request.focusXFrac).toBeCloseTo(0.202, 2);
});
//=============================================================================
it('zooms out on a Shift-click', () => {
  const { action } = start({ shiftKey: true });
  up(600, 150);
  expect((action.mock.calls[0][0] as Record<string, number>).factor).toBe(0.5);
});
//=============================================================================
it('never sends a region for a 3D axes, only a point zoom factor', () => {
  const { action } = start({ is3d: true });
  move(850, 450);
  up(850, 450);
  const request = action.mock.calls[0][0] as Record<string, unknown>;
  expect(request.regionXFrac).toBeUndefined();
  expect(request.factor).toBe(2);
});
//=============================================================================
