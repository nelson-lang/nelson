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
import { isEngineBusy } from '../desktop/engineSession';
import type { FigureDisplayListResult } from '../rpc/types';
import { pickRotatableAxes } from './figureAxes';
import { figureHistoryGroup } from './figureHistory';
//=============================================================================
interface PanOptions {
  dl: FigureDisplayListResult;
  axesHandle: number;
  selectAxes: (handle: number) => void;
  action: (request: Record<string, unknown>) => Promise<void>;
  cancelDrag: { current: (() => void) | null };
}
//=============================================================================
export function startFigurePan(event: ReactMouseEvent, options: PanOptions) {
  if (event.button !== 0) return;
  event.preventDefault();
  startFigurePanAt(
    event.currentTarget.getBoundingClientRect(),
    event.clientX,
    event.clientY,
    options
  );
}
//=============================================================================
// Begin a pan gesture from an explicit origin inside the stage rectangle, so the
// modeless default handler can start it only once a press turns into a drag.
export function startFigurePanAt(
  rect: { left: number; top: number; width: number; height: number },
  originX: number,
  originY: number,
  options: PanOptions
) {
  options.cancelDrag.current?.();
  const picked = pickRotatableAxes(options.dl.rotatableAxes, rect, originX, originY);
  const axesHandle = picked?.handle ?? options.axesHandle;
  const width = rect.width * (picked?.rect[2] ?? 1);
  const height = rect.height * (picked?.rect[3] ?? 1);
  if (width <= 0 || height <= 0) return;
  if (axesHandle) options.selectAxes(axesHandle);
  let x = originX,
    y = originY,
    sentX = x,
    sentY = y;
  let frame: number | null = null;
  const historyGroup = figureHistoryGroup();
  let inFlight = false,
    ended = false,
    cancelled = false;

  const schedule = () => {
    if (frame !== null || inFlight || ended || cancelled || (x === sentX && y === sentY)) return;
    frame = requestAnimationFrame(() => {
      frame = null;
      if (!isEngineBusy()) send();
    });
  };
  const send = () => {
    const dx = x - sentX,
      dy = y - sentY;
    if (cancelled || (!dx && !dy)) return;
    sentX = x;
    sentY = y;
    inFlight = true;
    void options
      .action({
        action: 'pan',
        axesHandle,
        dxFrac: dx / width,
        dyFrac: dy / height,
        historyGroup,
        compactView: true,
      })
      .catch(() => cancel())
      .finally(() => {
        inFlight = false;
        schedule();
      });
  };
  const move = (next: MouseEvent) => {
    x = next.clientX;
    y = next.clientY;
    schedule();
  };
  const detach = () => {
    window.removeEventListener('mousemove', move);
    window.removeEventListener('mouseup', up);
    window.removeEventListener('blur', blur);
    if (frame !== null) cancelAnimationFrame(frame);
    frame = null;
    if (options.cancelDrag.current === cancel) options.cancelDrag.current = null;
  };
  const cancel = () => {
    cancelled = true;
    detach();
  };
  const finish = () => {
    ended = true;
    detach();
    // Queue the remaining delta now, before a subsequent Fit or view edit.
    // While held, only one action runs and newer pointer positions replace one another.
    send();
  };
  const up = (next: MouseEvent) => {
    x = next.clientX;
    y = next.clientY;
    finish();
  };
  const blur = () => finish();
  options.cancelDrag.current = cancel;
  window.addEventListener('mousemove', move);
  window.addEventListener('mouseup', up);
  window.addEventListener('blur', blur);
}
//=============================================================================
