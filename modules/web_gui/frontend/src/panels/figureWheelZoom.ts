//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { RotatableAxes } from '../rpc/types';
import { hitFigureAxes } from './figureAxes';
import { isFigureDrawingTarget } from './figureShortcuts';
import { isEngineBusy } from '../desktop/engineSession';
//=============================================================================
interface Options {
  stage: HTMLElement;
  axes: () => RotatableAxes[] | undefined;
  selectAxes: (handle: number) => void;
  action: (request: Record<string, unknown>) => Promise<void>;
  refresh?: () => void;
}
//=============================================================================
interface PendingZoom {
  exponent: number;
  focusXFrac: number;
  focusYFrac: number;
}
//=============================================================================
export function createFigureWheelZoom(options: Options) {
  const pending = new Map<number, PendingZoom>();
  let frame: number | null = null,
    running = 0,
    disposed = false;
  let timer: ReturnType<typeof setTimeout> | undefined;
  let settleTimer: ReturnType<typeof setTimeout> | undefined;
  const scheduleRefresh = () => {
    if (settleTimer !== undefined) clearTimeout(settleTimer);
    settleTimer = setTimeout(() => {
      settleTimer = undefined;
      options.refresh?.();
    }, 180);
  };
  const send = (axesHandle: number, zoom: PendingZoom) => {
    if (Math.abs(zoom.exponent) < 1e-10) return;
    running++;
    void options
      .action({
        action: 'zoom',
        axesHandle,
        factor: Math.exp(zoom.exponent),
        focusXFrac: zoom.focusXFrac,
        focusYFrac: zoom.focusYFrac,
        compactView: true,
      })
      .catch(() => undefined)
      .finally(() => {
        running--;
        schedule();
      });
  };
  const schedule = () => {
    if (disposed || running || frame !== null || !pending.size) return;
    if (isEngineBusy()) {
      if (timer !== undefined) clearTimeout(timer);
      timer = setTimeout(flush, 120);
      return;
    }
    frame = requestAnimationFrame(() => {
      frame = null;
      const [handle, zoom] = pending.entries().next().value!;
      pending.delete(handle);
      send(handle, zoom);
      schedule();
    });
  };
  const flush = () => {
    if (timer !== undefined) clearTimeout(timer);
    timer = undefined;
    if (frame !== null) cancelAnimationFrame(frame);
    frame = null;
    const entries = [...pending];
    pending.clear();
    for (const [handle, zoom] of entries) send(handle, zoom);
  };
  const wheel = (event: WheelEvent) => {
    if (
      disposed ||
      event.defaultPrevented ||
      event.ctrlKey ||
      event.metaKey ||
      event.altKey ||
      !Number.isFinite(event.deltaY) ||
      !event.deltaY ||
      !isFigureDrawingTarget(event.target)
    )
      return;
    const rect = options.stage.getBoundingClientRect();
    const axis = hitFigureAxes(options.axes(), rect, event.clientX, event.clientY);
    if (!axis) return;
    event.preventDefault();
    options.selectAxes(axis.handle);
    const unit = event.deltaMode === 1 ? 16 : event.deltaMode === 2 ? rect.height : 1;
    // Keep the point under the cursor fixed: anchor the zoom there (fraction of
    // the cursor inside the axes rectangle). Buttons omit the focus and zoom
    // about the centre instead.
    const [left, top, width, height] = axis.rect;
    const fx = rect.width > 0 ? (event.clientX - rect.left) / rect.width : 0.5;
    const fy = rect.height > 0 ? (event.clientY - rect.top) / rect.height : 0.5;
    const focusXFrac = width > 0 ? (fx - left) / width : 0.5;
    const focusYFrac = height > 0 ? (fy - top) / height : 0.5;
    const exponent = (pending.get(axis.handle)?.exponent ?? 0) - event.deltaY * unit * 0.002;
    pending.set(axis.handle, {
      exponent: Math.max(-2, Math.min(2, exponent)),
      focusXFrac,
      focusYFrac,
    });
    scheduleRefresh();
    schedule();
  };
  return {
    wheel,
    flush,
    dispose: () => {
      disposed = true;
      pending.clear();
      if (timer !== undefined) clearTimeout(timer);
      if (settleTimer !== undefined) clearTimeout(settleTimer);
      if (frame !== null) cancelAnimationFrame(frame);
    },
  };
}
//=============================================================================
