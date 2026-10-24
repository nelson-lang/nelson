//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { RendererCtx, Block, RendererState } from '../core/renderer-types.js';
import { state } from '../../app/state.js';
import { renderScope } from '../../sim/render-scope.js';
import { renderXYScope } from '../../sim/render-xy-scope.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
interface InteractionExtCtx extends RendererCtx {
  clientToSvg: (x: number, y: number) => { x: number; y: number };
  updateCommentLayout?: (b: Block) => void;
  updateScopeLayout?: (b: Block) => void;
  updateDisplayLayout?: (b: Block) => void;
  updateToggleLayout?: (b: Block) => void;
  updateConnections?: (force?: boolean) => void;
  selectBlock?: (id: string) => void;
  onOpenSubsystem?: (b: Block) => void;
  state: RendererState & Record<string, any>;
}
//=============================================================================
const LONG_PRESS_MS = 550;
const MOVE_THRESHOLD = 6;
export function makeInteractionHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as InteractionExtCtx;
  function enableResize(block: Block, handle: Element) {
    let resizing = false;
    let startPoint: { x: number; y: number } | null = null;
    let startSize: { width: number; height: number } | null = null;
    const beginResize = (event: any) => {
      if (state.locked) return;
      if (state.simulationActive && String(block.type ?? '').startsWith('dashboard')) return;
      event.preventDefault();
      event.stopPropagation();
      resizing = true;
      const point = C.clientToSvg(event.clientX, event.clientY);
      startPoint = { x: point.x, y: point.y };
      startSize = { width: (block as any).width, height: (block as any).height };
      (handle as HTMLElement).setPointerCapture?.(event.pointerId);
    };
    handle.addEventListener('pointerdown', beginResize);
    handle.addEventListener('pointermove', (event: any) => {
      if (!resizing || !startPoint || !startSize) return;
      const point = C.clientToSvg(event.clientX, event.clientY);
      (block as any).width = startSize.width + (point.x - startPoint.x);
      (block as any).height = startSize.height + (point.y - startPoint.y);
      if ((block as any).type === 'comment') C.updateCommentLayout?.(block as any);
      else if ((block as any).type === 'display') C.updateDisplayLayout?.(block as any);
      else if ((block as any).type === FieldNames.kToggleSwitch)
        C.updateToggleLayout?.(block as any);
      else C.updateScopeLayout?.(block as any);
      (C.state as any).fastRouting = true;
      (C.state as any).routingDirty = true;
      if ((C.state as any).dirtyBlocks) (C.state as any).dirtyBlocks.add((block as any).id);
      C.updateConnections?.();
    });
    const finishResize = () => {
      if (!resizing) return;
      resizing = false;
      startPoint = null;
      startSize = null;
      (C.state as any).fastRouting = false;
      (C.state as any).routingDirty = true;
      C.updateConnections?.(true);
    };
    handle.addEventListener('pointerup', finishResize);
    handle.addEventListener('pointercancel', finishResize);
  }

  function enableSelection(block: Block, handle: Element) {
    handle.addEventListener('click', (event: any) => {
      event.stopPropagation();
      if (!event.ctrlKey) C.selectBlock?.((block as any).id);
    });
  }

  function enableSubsystemOpen(block: Block, handle: Element | null) {
    if (!block || (block as any).type !== FieldNames.kSubsystem || !handle) return;
    let timer: any = null;
    let start: { x: number; y: number } | null = null;
    let pointerId: number | null = null;
    let fired = false;
    const cancelTimer = () => {
      if (timer) {
        clearTimeout(timer);
        timer = null;
      }
      start = null;
      pointerId = null;
      fired = false;
    };
    handle.addEventListener('dblclick', (event: any) => {
      event.preventDefault();
      event.stopPropagation();
      if ((block as any).params?.masked === true) return;
      if (typeof C.onOpenSubsystem === 'function') C.onOpenSubsystem(block as any);
    });
    handle.addEventListener('pointerdown', (event: any) => {
      if (event.button !== 0) return;
      cancelTimer();
      start = { x: event.clientX, y: event.clientY };
      pointerId = event.pointerId;
      timer = setTimeout(() => {
        timer = null;
        fired = true;
        (C.state as any).suppressNextCanvasClick = true;
        if ((block as any).params?.masked !== true) {
          if (typeof C.onOpenSubsystem === 'function') C.onOpenSubsystem(block as any);
        }
      }, LONG_PRESS_MS);
    });
    handle.addEventListener('pointermove', (event: any) => {
      if (!timer || pointerId !== event.pointerId || !start) return;
      if (Math.hypot(event.clientX - start.x, event.clientY - start.y) > MOVE_THRESHOLD)
        cancelTimer();
    });
    const finish = (event: any) => {
      if (pointerId != null && pointerId !== event.pointerId) return;
      if (fired) (C.state as any).suppressNextCanvasClick = true;
      cancelTimer();
    };
    handle.addEventListener('pointerup', finish);
    handle.addEventListener('pointercancel', finish);
  }

  function enableScopeHover(block: Block) {
    if ((block as any).type !== 'scope') return;
    (block as any).group.addEventListener('pointermove', (event: any) => {
      const point = C.clientToSvg(event.clientX, event.clientY);
      (block as any).scopeHoverX = point.x - (block as any).x;
      renderScope(block as any);
    });
    (block as any).group.addEventListener('pointerleave', () => {
      (block as any).scopeHoverX = null;
      renderScope(block as any);
    });
  }

  function enableXyScopeInteraction(block: Block) {
    if ((block as any).type !== 'xyScope') return;
    const group: Element = (block as any).group;
    const plot: Element | undefined = (block as any).scopePlot;
    if (!plot) return;

    group.addEventListener(
      'wheel',
      (event: any) => {
        event.preventDefault();
        const computedLimits = (block as any).computedLimits || {};
        const xMin0: number =
          (block as any).params?.XMin != null
            ? Number((block as any).params.XMin)
            : computedLimits.xMin ?? -1.2;
        const xMax0: number =
          (block as any).params?.XMax != null
            ? Number((block as any).params.XMax)
            : computedLimits.xMax ?? 1.2;
        const yMin0: number =
          (block as any).params?.YMin != null
            ? Number((block as any).params.YMin)
            : computedLimits.yMin ?? -1.2;
        const yMax0: number =
          (block as any).params?.YMax != null
            ? Number((block as any).params.YMax)
            : computedLimits.yMax ?? 1.2;

        const pt = C.clientToSvg(event.clientX, event.clientY);
        const bx = (block as any).x ?? 0;
        const by = (block as any).y ?? 0;
        const plotX = Number(plot.getAttribute('x'));
        const plotY = Number(plot.getAttribute('y'));
        const plotW = Number(plot.getAttribute('width'));
        const plotH = Number(plot.getAttribute('height'));
        const rx = Math.max(0, Math.min(1, (pt.x - bx - plotX) / plotW));
        const ry = Math.max(0, Math.min(1, (pt.y - by - plotY) / plotH));

        const factor = event.deltaY > 0 ? 1.15 : 1 / 1.15;
        const xCenter = xMin0 + rx * (xMax0 - xMin0);
        const yCenter = yMax0 - ry * (yMax0 - yMin0);
        (block as any).params.XMin = xCenter - rx * (xMax0 - xMin0) * factor;
        (block as any).params.XMax = xCenter + (1 - rx) * (xMax0 - xMin0) * factor;
        (block as any).params.YMin = yCenter - (1 - ry) * (yMax0 - yMin0) * factor;
        (block as any).params.YMax = yCenter + ry * (yMax0 - yMin0) * factor;

        renderXYScope(block as any);
        C.state.dirtyBlocks?.add((block as any).id);
      },
      { passive: false }
    );

    let dragging = false;
    let dragStart: { x: number; y: number } | null = null;
    let limitAtDragStart: { xMin: number; xMax: number; yMin: number; yMax: number } | null = null;

    group.addEventListener('pointerdown', (event: any) => {
      if (event.button !== 0) return;
      event.preventDefault();
      (group as HTMLElement).setPointerCapture?.(event.pointerId);
      dragging = true;
      const pt = C.clientToSvg(event.clientX, event.clientY);
      dragStart = { x: pt.x, y: pt.y };
      const computedLimits = (block as any).computedLimits || {};
      limitAtDragStart = {
        xMin:
          (block as any).params?.XMin != null
            ? Number((block as any).params.XMin)
            : computedLimits.xMin ?? -1.2,
        xMax:
          (block as any).params?.XMax != null
            ? Number((block as any).params.XMax)
            : computedLimits.xMax ?? 1.2,
        yMin:
          (block as any).params?.YMin != null
            ? Number((block as any).params.YMin)
            : computedLimits.yMin ?? -1.2,
        yMax:
          (block as any).params?.YMax != null
            ? Number((block as any).params.YMax)
            : computedLimits.yMax ?? 1.2,
      };
    });

    group.addEventListener('pointermove', (event: any) => {
      if (!dragging || !dragStart || !limitAtDragStart) return;
      const plotW = Number(plot.getAttribute('width'));
      const plotH = Number(plot.getAttribute('height'));
      const pt = C.clientToSvg(event.clientX, event.clientY);
      const dx = pt.x - dragStart.x;
      const dy = pt.y - dragStart.y;
      const xRange = limitAtDragStart.xMax - limitAtDragStart.xMin;
      const yRange = limitAtDragStart.yMax - limitAtDragStart.yMin;
      const dxData = -(dx / plotW) * xRange;
      const dyData = (dy / plotH) * yRange;
      (block as any).params.XMin = limitAtDragStart.xMin + dxData;
      (block as any).params.XMax = limitAtDragStart.xMax + dxData;
      (block as any).params.YMin = limitAtDragStart.yMin + dyData;
      (block as any).params.YMax = limitAtDragStart.yMax + dyData;
      renderXYScope(block as any);
    });

    const stopDrag = () => {
      if (!dragging) return;
      dragging = false;
      dragStart = null;
      limitAtDragStart = null;
      C.state.dirtyBlocks?.add((block as any).id);
    };
    group.addEventListener('pointerup', stopDrag);
    group.addEventListener('pointercancel', stopDrag);
  }

  return {
    enableResize,
    enableSelection,
    enableSubsystemOpen,
    enableScopeHover,
    enableXyScopeInteraction,
  };
}
//=============================================================================
