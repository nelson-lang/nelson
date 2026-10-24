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
import { useFigureQuickActions } from '../../src/panels/useFigureQuickActions';
import { FigureAxesIndicator } from '../../src/panels/FigureAxesIndicator';
import type { FigureDisplayListResult } from '../../src/rpc/types';
import type { FigureAxesState, FigureDataPoint, FigureTool } from '../../src/rpc/figureTypes';
//=============================================================================
vi.mock('../../src/rpc/client', () => ({ rpc: vi.fn() }));
let dispose: () => void;
beforeEach(() => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  vi.mocked(rpc).mockResolvedValue({ ok: false });
});
//=============================================================================
afterEach(() => {
  dispose?.();
  vi.mocked(rpc).mockReset();
  vi.unstubAllGlobals();
});
//=============================================================================
function workspace(opts: { tool?: FigureTool; point?: FigureDataPoint } = {}) {
  const host = document.createElement('div');
  document.body.append(host);
  const root = createRoot(host);
  const action = vi.fn(async (_request: Record<string, unknown>) => undefined);
  const setTool = vi.fn(),
    selectAxes = vi.fn();
  const dl = {
    is3d: true,
    width: 1000,
    height: 600,
    rotatableAxes: [
      { handle: 11, rect: [0, 0, 0.4, 1], view: [0, 90] },
      { handle: 22, rect: [0.6, 0, 0.4, 1], view: [-37.5, 30] },
    ],
  } as FigureDisplayListResult;
  function Workspace() {
    const quick = useFigureQuickActions({
      handle: 0,
      dl,
      axes: { handle: 11 } as FigureAxesState,
      action,
      setTool,
      selectAxes,
      tool: opts.tool,
      point: opts.point,
    });
    return (
      <div
        tabIndex={0}
        data-root
        onKeyDown={(e) => quick.key(e, true)}
        onKeyUp={(e) => quick.key(e, false)}
      >
        <div
          data-stage
          onMouseMoveCapture={quick.move}
          onMouseLeave={quick.leave}
          onMouseDownCapture={quick.down}
          onDoubleClick={quick.doubleClick}
        >
          <canvas /> <input />{' '}
          <select>
            <option>p</option>
          </select>
          <div contentEditable suppressContentEditableWarning>
            <span>edit</span>
          </div>
          {quick.axis && <FigureAxesIndicator axis={quick.axis} label="Target" />}
        </div>
      </div>
    );
  }
  act(() => root.render(<Workspace />));
  const panel = host.querySelector<HTMLElement>('[data-root]')!;
  const stage = host.querySelector<HTMLElement>('[data-stage]')!;
  stage.getBoundingClientRect = () => ({ left: 0, top: 0, width: 1000, height: 600 } as DOMRect);
  panel.focus();
  dispose = () => {
    act(() => root.unmount());
    host.remove();
  };
  return { host, panel, action, setTool, selectAxes };
}
//=============================================================================
async function key(target: Element, value: string, options: KeyboardEventInit = {}) {
  await act(async () => {
    target.dispatchEvent(
      new KeyboardEvent('keydown', {
        key: value,
        bubbles: true,
        cancelable: true,
        ...options,
      })
    );
  });
}
//=============================================================================
it('fits only the subplot under a double-click, leaving margins and controls alone', () => {
  const { host, action, selectAxes } = workspace();
  const click = (target: Element, clientX: number) =>
    act(() => {
      target.dispatchEvent(
        new MouseEvent('dblclick', { button: 0, clientX, clientY: 200, bubbles: true })
      );
    });
  click(host.querySelector('canvas')!, 700);
  expect(selectAxes).toHaveBeenCalledWith(22);
  expect(action.mock.calls).toEqual([[{ action: 'fit', axesHandle: 22 }]]);
  click(host.querySelector('canvas')!, 500);
  click(host.querySelector('input')!, 700);
  expect(action).toHaveBeenCalledTimes(1);
});
//=============================================================================
it('keeps the indicator on the dragged axes when the pointer crosses another subplot', () => {
  const { host } = workspace();
  const canvas = host.querySelector('canvas')!;
  const mouse = (type: string, clientX: number, buttons = 0) =>
    act(() => {
      canvas.dispatchEvent(new MouseEvent(type, { clientX, clientY: 200, buttons, bubbles: true }));
    });
  mouse('mousemove', 700);
  expect(host.querySelector('.figure-axes-indicator')?.getAttribute('data-axes-handle')).toBe('22');
  mouse('mousedown', 700, 1);
  mouse('mousemove', 200, 1);
  expect(host.querySelector('.figure-axes-indicator')?.getAttribute('data-axes-handle')).toBe('22');
  mouse('mouseup', 200);
  mouse('mousemove', 200);
  expect(host.querySelector('.figure-axes-indicator')?.getAttribute('data-axes-handle')).toBe('11');
  mouse('mousemove', 500);
  expect(host.querySelector('.figure-axes-indicator')).toBeNull();
});
//=============================================================================
it('runs P/R/F for the focused figure without intercepting form fields or modified keys', async () => {
  const { host, panel, action, setTool } = workspace();
  await key(panel, 'p');
  await key(panel, 'R');
  await key(panel, 'f');
  expect(setTool.mock.calls).toEqual([['pan'], ['rotate']]);
  expect(action).toHaveBeenCalledWith({ action: 'fit', axesHandle: 11 });
  vi.mocked(rpc).mockClear();
  for (const selector of ['input', 'select', '[contenteditable] span'])
    await key(host.querySelector(selector)!, 'p');
  expect(rpc).not.toHaveBeenCalled();
  await key(panel, 'p', { ctrlKey: true });
  await key(panel, 'f', { repeat: true });
  expect(setTool).toHaveBeenCalledTimes(2);
  expect(action).toHaveBeenCalledTimes(1);
});
//=============================================================================
it('gives Nelson keyboard callbacks priority over navigation shortcuts', async () => {
  vi.mocked(rpc).mockResolvedValue({ ok: true });
  const { panel, action, setTool } = workspace();
  await key(panel, 'p');
  await key(panel, 'r');
  await key(panel, 'f');
  expect(rpc).toHaveBeenCalledWith(
    'figure.keyEvent',
    expect.objectContaining({ character: 'p', pressed: true })
  );
  expect(setTool).not.toHaveBeenCalled();
  expect(action).not.toHaveBeenCalled();
});
//=============================================================================
it('ignores held-key auto-repeat once a figure callback owns the key, still sending the release', async () => {
  vi.mocked(rpc).mockResolvedValue({ ok: true });
  const { panel } = workspace();
  await key(panel, 'w');
  await key(panel, 'w', { repeat: true });
  await key(panel, 'w', { repeat: true });
  const pressed = (payload: unknown) => (payload as { pressed: boolean }).pressed;
  expect(vi.mocked(rpc).mock.calls.filter(([, p]) => pressed(p))).toHaveLength(1);
  await act(async () => {
    panel.dispatchEvent(new KeyboardEvent('keyup', { key: 'w', bubbles: true, cancelable: true }));
  });
  expect(vi.mocked(rpc).mock.calls.filter(([, p]) => !pressed(p))).toHaveLength(1);
});
//=============================================================================
it('keeps forwarding auto-repeat when no figure callback owns the key', async () => {
  vi.mocked(rpc).mockResolvedValue({ ok: false });
  const { panel } = workspace();
  await key(panel, 'ArrowRight');
  await key(panel, 'ArrowRight', { repeat: true });
  const pressed = (payload: unknown) => (payload as { pressed: boolean }).pressed;
  expect(vi.mocked(rpc).mock.calls.filter(([, p]) => pressed(p))).toHaveLength(2);
});
//=============================================================================
it('drops a delayed shortcut when focus moves into an inspector field', async () => {
  let resolve!: (value: { ok: boolean }) => void;
  vi.mocked(rpc).mockImplementation(
    () =>
      new Promise((r) => {
        resolve = r;
      }) as never
  );
  const { host, panel, setTool } = workspace();
  await key(panel, 'p');
  host.querySelector('input')!.focus();
  await act(async () => resolve({ ok: false }));
  expect(setTool).not.toHaveBeenCalled();
});
//=============================================================================
it('arrows pan the selected subplot in pan mode', async () => {
  const { panel, action } = workspace({ tool: 'pan' });
  await key(panel, 'ArrowRight');
  const call = action.mock.calls.at(-1)![0];
  expect(call.action).toBe('pan');
  expect(call.axesHandle).toBe(11);
  expect(call.dxFrac).toBeCloseTo(0.01, 10);
  expect(call.dyFrac).toBe(0);
});
//=============================================================================
it('arrows orbit the selected subplot in rotate mode', async () => {
  const { panel, action } = workspace({ tool: 'rotate' });
  await key(panel, 'ArrowUp');
  expect(action).toHaveBeenLastCalledWith({
    action: 'view',
    axesHandle: 11,
    azimuth: 0,
    elevation: 86,
  });
});
//=============================================================================
it('up/down arrows zoom in zoom mode while left/right do nothing', async () => {
  const { panel, action } = workspace({ tool: 'zoom' });
  await key(panel, 'ArrowUp');
  expect(action).toHaveBeenLastCalledWith({ action: 'zoom', axesHandle: 11, factor: 1.15 });
  await key(panel, 'ArrowLeft');
  expect(action).toHaveBeenCalledTimes(1);
});
//=============================================================================
