//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act, useRef } from 'react';
import { createRoot } from 'react-dom/client';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import { useFigureWheelZoom } from '../../src/panels/useFigureWheelZoom';
import type { FigureDisplayListResult } from '../../src/rpc/types';
//=============================================================================
vi.mock('../../src/desktop/engineSession', () => ({ isEngineBusy: vi.fn(() => false) }));
let dispose: () => void;
beforeEach(() => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  vi.stubGlobal('requestAnimationFrame', (callback: FrameRequestCallback) => {
    callback(0);
    return 1;
  });
  vi.stubGlobal('cancelAnimationFrame', () => undefined);
});
//=============================================================================
afterEach(() => {
  dispose?.();
  vi.unstubAllGlobals();
});
//=============================================================================
function mount(dl: FigureDisplayListResult) {
  const host = document.createElement('div');
  document.body.append(host);
  const root = createRoot(host);
  const action = vi.fn(async (_request: Record<string, unknown>) => undefined);
  function Stage() {
    const stageRef = useRef<HTMLDivElement | null>(null);
    useFigureWheelZoom(7, stageRef, dl, vi.fn(), action);
    return (
      <div ref={stageRef} data-stage>
        <canvas />
      </div>
    );
  }
  act(() => root.render(<Stage />));
  dispose = () => {
    act(() => root.unmount());
    host.remove();
  };
  const stage = host.querySelector('[data-stage]') as HTMLDivElement;
  stage.getBoundingClientRect = () => ({ left: 0, top: 0, width: 1000, height: 500 } as DOMRect);
  const wheel = () => {
    const event = new WheelEvent('wheel', {
      deltaY: -100,
      clientX: 500,
      clientY: 250,
      bubbles: true,
      cancelable: true,
    });
    act(() => {
      stage.querySelector('canvas')!.dispatchEvent(event);
    });
    return event;
  };
  return { wheel, action };
}
//=============================================================================
const axes = { handle: 22, rect: [0, 0, 1, 1], view: [0, 90] } as const;
it('zooms the axes under the wheel on an ordinary figure', () => {
  const { wheel, action } = mount({ rotatableAxes: [axes] } as unknown as FigureDisplayListResult);
  expect(wheel().defaultPrevented).toBe(true);
  expect(action).toHaveBeenCalledTimes(1);
  expect(action.mock.calls[0][0]).toMatchObject({ action: 'zoom', axesHandle: 22 });
});
//=============================================================================
it('leaves the wheel to a figure that defines WindowScrollWheelFcn', () => {
  const { wheel, action } = mount({
    rotatableAxes: [axes],
    hasScrollWheelCallback: true,
  } as unknown as FigureDisplayListResult);
  expect(wheel().defaultPrevented).toBe(false);
  expect(action).not.toHaveBeenCalled();
});
//=============================================================================
