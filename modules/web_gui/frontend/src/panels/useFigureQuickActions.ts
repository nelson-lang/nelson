//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, useState } from 'react';
import type { KeyboardEvent as ReactKeyboardEvent, MouseEvent as ReactMouseEvent } from 'react';
import { rpc } from '../rpc/client';
import type { FigureDisplayListResult } from '../rpc/types';
import type { FigureAxesState, FigureTool, FigureDataPoint } from '../rpc/figureTypes';
import { hitFigureAxes } from './figureAxes';
import { figureKeyEventPayload } from './figureKeyEvents';
import { figureShortcut, isFigureDrawingTarget, isFigureFormTarget } from './figureShortcuts';
//=============================================================================
interface Options {
  handle: number;
  dl: FigureDisplayListResult | null;
  axes: FigureAxesState | null;
  setTool: (tool: FigureTool) => void;
  selectAxes: (handle: number) => void;
  action: (request: Record<string, unknown>) => Promise<void>;
  tool?: FigureTool;
  point?: FigureDataPoint | null;
}
//=============================================================================
export function useFigureQuickActions(options: Options) {
  const [hover, setHover] = useState<number | null>(null);
  const [drag, setDrag] = useState<number | null>(null);
  const [keyboard, setKeyboard] = useState<number | null>(null);
  const generation = useRef(0);
  // Whether the figure's own key callbacks (WindowKeyPressFcn) consume keys,
  // learnt from the last keydown the engine answered. Held keys auto-repeat
  // keydown at the OS rate; those repeats are redundant for edge-triggered
  // callbacks and flood the cooperative command channel the WebAssembly backend
  // serializes key events through, so a key-release stacks up behind them and
  // motion keeps going after the key is let go. Repeats are dropped only once the
  // figure is known to own the key, which leaves arrow auto-repeat point-stepping
  // (it runs only when no callback owns the key) intact.
  const callbackOwnsKeys = useRef(false);
  useEffect(() => {
    generation.current++;
    callbackOwnsKeys.current = false;
    setHover(null);
    setDrag(null);
    setKeyboard(null);
    const release = () => setDrag(null);
    const blur = () => {
      release();
      setHover(null);
      setKeyboard(null);
    };
    window.addEventListener('mouseup', release);
    window.addEventListener('blur', blur);
    return () => {
      generation.current++;
      window.removeEventListener('mouseup', release);
      window.removeEventListener('blur', blur);
    };
  }, [options.handle]);
  const pick = (event: ReactMouseEvent) =>
    isFigureDrawingTarget(event.target)
      ? hitFigureAxes(
          options.dl?.rotatableAxes,
          event.currentTarget.getBoundingClientRect(),
          event.clientX,
          event.clientY
        )
      : null;
  const move = (event: ReactMouseEvent) => {
    if (event.buttons !== 0) return;
    setKeyboard(null);
    setHover(pick(event)?.handle ?? null);
  };
  const down = (event: ReactMouseEvent) => {
    const axis = pick(event);
    if (event.button !== 0 || !axis) return;
    setKeyboard(null);
    setHover(axis.handle);
    setDrag(axis.handle);
    options.selectAxes(axis.handle);
  };
  const doubleClick = (event: ReactMouseEvent) => {
    const axis = pick(event);
    if (event.button !== 0 || !axis) return;
    event.preventDefault();
    options.selectAxes(axis.handle);
    void options.action({ action: 'fit', axesHandle: axis.handle });
  };
  const key = (event: ReactKeyboardEvent, pressed: boolean) => {
    if (event.defaultPrevented || isFigureFormTarget(event.target)) return;
    // Skip auto-repeat keydown once the figure is known to consume the key: the
    // first keydown already set the callback state, and the release is always
    // forwarded. See callbackOwnsKeys above.
    if (pressed && event.nativeEvent.repeat && callbackOwnsKeys.current) return;
    const root = event.currentTarget;
    const shortcut = pressed ? figureShortcut(event.nativeEvent, !!options.dl?.is3d) : null;
    const ticket = generation.current;
    if (shortcut) event.preventDefault();
    // The engine reports whether a figure callback owns the key. Keep its priority.
    void rpc<{ ok: boolean }>(
      'figure.keyEvent',
      figureKeyEventPayload(options.handle, event.nativeEvent, pressed)
    )
      .then((result) => {
        if (pressed) callbackOwnsKeys.current = result.ok;
        if (
          result.ok ||
          !shortcut ||
          ticket !== generation.current ||
          !root.contains(document.activeElement) ||
          isFigureFormTarget(document.activeElement)
        )
          return;
        const axis =
          drag ?? hover ?? options.axes?.handle ?? options.dl?.rotatableAxes?.[0]?.handle;
        setKeyboard(axis ?? null);
        if (axis !== undefined) options.selectAxes(axis);
        if (shortcut === 'undo' || shortcut === 'redo') void options.action({ action: shortcut });
        else if (
          shortcut === 'arrowLeft' ||
          shortcut === 'arrowRight' ||
          shortcut === 'arrowUp' ||
          shortcut === 'arrowDown'
        ) {
          // Match the native desktop: arrows pan (4px), orbit (4°) or zoom (1.15)
          // depending on the active mode, and step data points while inspecting.
          const dl = options.dl;
          const ax = dl?.rotatableAxes?.find((a) => a.handle === axis);
          const horizontal = shortcut === 'arrowLeft' ? -4 : shortcut === 'arrowRight' ? 4 : 0;
          const vertical = shortcut === 'arrowUp' ? -4 : shortcut === 'arrowDown' ? 4 : 0;
          if (options.tool === 'pan' && dl && ax) {
            const width = ax.rect[2] * dl.width;
            const height = ax.rect[3] * dl.height;
            if (width > 0 && height > 0)
              void options.action({
                action: 'pan',
                axesHandle: axis,
                dxFrac: horizontal / width,
                dyFrac: vertical / height,
              });
          } else if (options.tool === 'rotate' && ax) {
            void options.action({
              action: 'view',
              axesHandle: axis,
              azimuth: ax.view[0] - horizontal,
              elevation: ax.view[1] + vertical,
            });
          } else if (options.tool === 'zoom') {
            if (vertical !== 0)
              void options.action({
                action: 'zoom',
                axesHandle: axis,
                factor: vertical < 0 ? 1.15 : 1 / 1.15,
              });
          } else {
            const point = options.point;
            const forward = shortcut === 'arrowRight' || shortcut === 'arrowUp';
            if (options.tool === 'inspect' && point?.found && point.index && point.objectHandle)
              void options.action({
                action: 'step',
                objectHandle: point.objectHandle,
                index: Math.max(
                  1,
                  Math.min(point.count ?? Infinity, point.index + (forward ? 1 : -1))
                ),
              });
          }
        } else if (shortcut === 'fit') void options.action({ action: 'fit', axesHandle: axis });
        else options.setTool(shortcut);
      })
      .catch(() => undefined);
  };
  const axis = options.dl?.rotatableAxes?.find(
    (axis) => axis.handle === (drag ?? hover ?? keyboard)
  );
  return {
    axis,
    down,
    move,
    doubleClick,
    key,
    leave: () => {
      setHover(null);
      setKeyboard(null);
    },
    blur: (event: React.FocusEvent) => {
      if (!event.currentTarget.contains(event.relatedTarget as Node | null)) setKeyboard(null);
    },
  };
}
//=============================================================================
