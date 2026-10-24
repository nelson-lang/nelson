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
import type { MouseEvent as ReactMouseEvent } from 'react';
import { rpc } from '../rpc/client';
import { createResponseOrder, withFigureSlot } from '../desktop/figureQueue';
import type { FigureAxesState, FigureTool } from '../rpc/figureTypes';
import type { FigureDisplayListResult } from '../rpc/types';
import { pickRotatableAxes } from './figureAxes';
import { startFigurePanAt } from './figurePan';
import { figureHistoryGroup } from './figureHistory';
//=============================================================================
interface Options {
  handle: number;
  dl: FigureDisplayListResult | null;
  displayListParams: () => Record<string, unknown>;
  apply: (frame: FigureDisplayListResult) => void;
  axes: FigureAxesState | null;
  selectAxes: (handle: number) => void;
  action: (request: Record<string, unknown>) => Promise<void>;
  refreshProperties: () => Promise<void>;
}
//=============================================================================
// Drag distance (px) that turns a modeless press into a pan/rotate instead of a
// data-tip pick.
const DRAG_THRESHOLD = 4;
type Rect = { left: number; top: number; width: number; height: number };
export function useFigureNavigation(options: Options) {
  const { dl, handle, action, selectAxes } = options;
  const [tool, setTool] = useState<FigureTool>('inspect');
  useEffect(() => {
    // A new figure starts modeless: a drag pans in 2D and rotates in 3D without
    // selecting a tool first.
    setTool('inspect');
  }, [handle]);
  const ordered = useRef(createResponseOrder()).current;
  const rotating = useRef(false);
  const rotationWaiters = useRef<((active: boolean) => void)[]>([]);
  const generation = useRef(0);
  const queued = useRef<Record<string, unknown> | null>(null);
  const alive = useRef(true);
  const cancelDrag = useRef<(() => void) | null>(null);
  useEffect(() => {
    alive.current = true;
    rotating.current = false;
    queued.current = null;
    return () => {
      alive.current = false;
      generation.current++;
      queued.current = null;
      cancelDrag.current?.();
      rotationWaiters.current.splice(0).forEach((done) => done(false));
    };
  }, [handle]);

  const setView = (request: Record<string, unknown>) => {
    if (rotating.current) {
      queued.current = request;
      return;
    }
    if (!alive.current) return;
    const ticket = generation.current;
    rotating.current = true;
    void withFigureSlot(() =>
      ticket === generation.current && alive.current
        ? rpc<FigureDisplayListResult>('figure.setView', {
            ...options.displayListParams(),
            ...request,
          })
        : Promise.resolve(null)
    )
      .then(
        ordered((frame) => {
          if (frame && ticket === generation.current && alive.current) {
            options.apply(frame);
            if (!request.compactView) void options.refreshProperties();
          }
        })
      )
      .catch(() => undefined)
      .finally(() => {
        if (ticket !== generation.current) return;
        rotating.current = false;
        const next = queued.current;
        queued.current = null;
        if (next && alive.current) setView(next);
        else rotationWaiters.current.splice(0).forEach((done) => done(alive.current));
      });
  };

  const rotationEnd = useRef<(() => void) | null>(null);
  const finishRotation = () => {
    // A command ends the held gesture; later pointer motion must not undo it.
    rotationEnd.current?.();
    return rotating.current
      ? new Promise<boolean>((resolve) => rotationWaiters.current.push(resolve))
      : Promise.resolve(alive.current);
  };

  // Rotate the subplot under the gesture origin. Absolute from the start point so
  // the drag cannot drift. Intermediate frames skip historyGroup to take the lean
  // C++ setView fast-path; the first and the final mouseup frame carry it so the
  // whole drag collapses to one undo step.
  const startRotateAt = (rect: Rect, originX: number, originY: number) => {
    if (!dl || !dl.is3d) return;
    cancelDrag.current?.();
    const picked = pickRotatableAxes(dl.rotatableAxes, rect, originX, originY);
    const view = picked?.view ?? dl.view;
    if (!view) return;
    const axesHandle = picked?.handle ?? options.axes?.handle ?? 0;
    if (axesHandle) selectAxes(axesHandle);
    let frame: number | null = null;
    let azimuth = view[0],
      elevation = view[1],
      moved = false;
    const historyGroup = figureHistoryGroup();
    let historyOpened = false;
    const send = (compactView: boolean) => {
      const withHistory = !historyOpened || !compactView;
      historyOpened = true;
      setView({
        axesHandle,
        azimuth,
        elevation,
        compactView,
        ...(withHistory ? { historyGroup } : {}),
      });
    };
    const move = (next: MouseEvent) => {
      azimuth = view[0] - (next.clientX - originX) * 0.5;
      elevation = Math.max(-90, Math.min(90, view[1] + (next.clientY - originY) * 0.5));
      moved = true;
      if (frame === null)
        frame = requestAnimationFrame(() => {
          frame = null;
          send(true);
        });
    };
    const cleanup = () => {
      window.removeEventListener('mousemove', move);
      window.removeEventListener('mouseup', up);
      window.removeEventListener('blur', up);
      if (frame !== null) cancelAnimationFrame(frame);
      cancelDrag.current = null;
      rotationEnd.current = null;
    };
    const up = () => {
      cleanup();
      if (moved && alive.current) send(false);
    };
    cancelDrag.current = up;
    rotationEnd.current = up;
    window.addEventListener('mousemove', move);
    window.addEventListener('mouseup', up);
    window.addEventListener('blur', up);
  };

  const startPanAt = (rect: Rect, originX: number, originY: number) => {
    if (!dl) return;
    startFigurePanAt(rect, originX, originY, {
      dl,
      axesHandle: options.axes?.handle ?? 0,
      selectAxes,
      action,
      cancelDrag,
    });
  };

  // Explicit-mode overlays (Rotate / Pan) start the gesture immediately.
  const startDrag = (event: ReactMouseEvent) => {
    if (!dl || !dl.is3d || event.button !== 0) return;
    event.preventDefault();
    startRotateAt(event.currentTarget.getBoundingClientRect(), event.clientX, event.clientY);
  };
  const startPan = (event: ReactMouseEvent) => {
    if (!dl || event.button !== 0) return;
    event.preventDefault();
    startPanAt(event.currentTarget.getBoundingClientRect(), event.clientX, event.clientY);
  };

  // Modeless default (Inspect tool): a click picks a data point, a drag beyond
  // the threshold pans (2D) or rotates (3D). The gesture starts from the press
  // origin so the first frame has the full delta.
  const beginModeless = (event: ReactMouseEvent, pick: (xFrac: number, yFrac: number) => void) => {
    if (!dl || event.button !== 0) return;
    const rect = event.currentTarget.getBoundingClientRect();
    const originX = event.clientX,
      originY = event.clientY;
    let started = false;
    const onMove = (next: MouseEvent) => {
      if (
        started ||
        (Math.abs(next.clientX - originX) < DRAG_THRESHOLD &&
          Math.abs(next.clientY - originY) < DRAG_THRESHOLD)
      )
        return;
      started = true;
      detach();
      if (dl.is3d) startRotateAt(rect, originX, originY);
      else startPanAt(rect, originX, originY);
    };
    const onUp = () => {
      detach();
      if (started || !alive.current || rect.width <= 0 || rect.height <= 0) return;
      pick((originX - rect.left) / rect.width, (originY - rect.top) / rect.height);
    };
    const detach = () => {
      window.removeEventListener('mousemove', onMove);
      window.removeEventListener('mouseup', onUp);
    };
    window.addEventListener('mousemove', onMove);
    window.addEventListener('mouseup', onUp);
  };

  return {
    tool,
    setTool,
    startDrag,
    startPan,
    startRotateAt,
    startPanAt,
    beginModeless,
    cancelDrag,
    finishRotation,
  };
}
//=============================================================================
