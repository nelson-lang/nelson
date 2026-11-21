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
import { captureGenericResize, resizeGenericBlock, usesGenericResize } from './generic-resize.js';
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
    let genericStart: any = null;
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
      genericStart = usesGenericResize(block.type) ? captureGenericResize(block) : null;
      (handle as HTMLElement).setPointerCapture?.(event.pointerId);
    };
    handle.addEventListener('pointerdown', beginResize);
    const resizeTo = (width: number, height: number, generic: any) => {
      (block as any).width = width;
      (block as any).height = height;
      if (generic) {
        resizeGenericBlock(block, generic, C);
      } else if (
        [
          'constant',
          'gain',
          'sum',
          'mult',
          'trigFunction',
          'mathFunction',
          'busCreator',
          'busSelector',
        ].includes(String((block as any).type))
      ) {
        const b = block as any;
        const isOperator = b.type === 'sum' || b.type === 'mult';
        const isFunction = b.type === 'trigFunction' || b.type === 'mathFunction';
        const isBus = b.type === 'busCreator' || b.type === 'busSelector';
        const inputs = (b.ports || [])
          .filter((p: any) => p.type === 'in')
          .sort((a: any, c: any) => a.index - c.index);
        b.width = Math.max(isOperator ? 20 : 40, b.width);
        const outputs = (b.ports || [])
          .filter((p: any) => p.type === 'out')
          .sort((a: any, c: any) => a.index - c.index);
        b.height = Math.max(
          isOperator || isFunction || isBus
            ? Math.max(40, inputs.length * 16, outputs.length * 16)
            : 24,
          b.height
        );
        b.params.width = b.width;
        b.params.height = b.height;
        const body = b.group.querySelector('rect.nf-block');
        body?.setAttribute('width', String(b.width));
        body?.setAttribute('height', String(b.height));
        b.resizeHandle?.setAttribute('x', String(b.width - 16));
        b.resizeHandle?.setAttribute('y', String(b.height - 16));
        if (b.dragRect) {
          const minSize = isOperator ? 0 : 80;
          const w = Math.max(minSize, b.width);
          const h = Math.max(minSize, b.height);
          b.dragRect.setAttribute('x', String((b.width - w) / 2));
          b.dragRect.setAttribute('y', String((b.height - h) / 2));
          b.dragRect.setAttribute('width', String(w));
          b.dragRect.setAttribute('height', String(h));
        }
        if (isOperator || isBus) {
          b.dynamicInputs = inputs.map((p: any, index: number) => {
            p.x = 0;
            p.y = ((index + 0.5) * b.height) / inputs.length;
            p.wireX = p.x;
            p.wireY = p.y;
            (C as any).updatePortElement?.(p);
            return { x: p.x, y: p.y, side: 'left' };
          });
          b.group
            .querySelectorAll('.sum-sign, .nf-product-sign')
            .forEach((el: Element, index: number) => {
              el.setAttribute('y', String(((index + 0.5) * b.height) / inputs.length));
            });
        }
        if (isFunction || b.type === 'gain') {
          b.dynamicInputs = inputs.map((p: any, index: number) => {
            p.x = 0;
            p.y = b.height / 2 + (index - (inputs.length - 1) / 2) * 20;
            p.wireX = p.x;
            p.wireY = p.y;
            (C as any).updatePortElement?.(p);
            return { x: p.x, y: p.y, side: 'left' };
          });
          if (b.type === 'gain') {
            const points = [
              [6, 66],
              [6, 6],
              [66, 36],
            ]
              .map(
                ([x, y]) =>
                  `${Math.max(1, Math.round((x * b.width) / 72))},${Math.max(
                    1,
                    Math.round((y * b.height) / 72)
                  )}`
              )
              .join(' ');
            b.group.querySelector('.gain-triangle')?.setAttribute('points', points);
          }
          const symbol = b.group.querySelector('.nf-function-symbol');
          symbol?.setAttribute('x', String(b.width / 2));
          symbol?.setAttribute('y', String(b.height / 2));
        }
        if (isBus) {
          const label = b.group.querySelector('.nf-block-formula text');
          if (label) {
            const lines = Array.from(label.querySelectorAll('tspan')) as SVGElement[];
            const lineHeight = Number(lines[1]?.getAttribute('dy')) || 0;
            label.setAttribute('x', String(b.width / 2));
            label.setAttribute('y', String(b.height / 2 - ((lines.length - 1) * lineHeight) / 2));
            lines.forEach((line) => line.setAttribute('x', String(b.width / 2)));
          }
        }
        b.dynamicOutputs = outputs.map((_p: any, index: number) => ({
          x: b.width,
          y: isBus
            ? ((index + 0.5) * b.height) / outputs.length
            : b.height / 2 + (index - (outputs.length - 1) / 2) * 20,
          side: 'right',
        }));
        for (const [index, p] of outputs.entries()) {
          p.x = b.width;
          p.y = b.dynamicOutputs[index].y;
          p.wireX = p.x;
          p.wireY = p.y;
          (C as any).updatePortElement?.(p);
        }
        (C as any).updateBlockLabel?.(b);
        (C as any).updateBlockTransform?.(b);
      } else if ((block as any).type === 'comment') C.updateCommentLayout?.(block as any);
      else if ((block as any).type === 'display') C.updateDisplayLayout?.(block as any);
      else if ((block as any).type === FieldNames.kToggleSwitch)
        C.updateToggleLayout?.(block as any);
      else C.updateScopeLayout?.(block as any);
      (C.state as any).fastRouting = true;
      (C.state as any).routingDirty = true;
      if ((C.state as any).dirtyBlocks) (C.state as any).dirtyBlocks.add((block as any).id);
      C.updateConnections?.();
    };
    handle.addEventListener('pointermove', (event: any) => {
      if (!resizing || !startPoint || !startSize) return;
      const point = C.clientToSvg(event.clientX, event.clientY);
      resizeTo(
        startSize.width + (point.x - startPoint.x),
        startSize.height + (point.y - startPoint.y),
        genericStart
      );
    });
    handle.addEventListener('nflow:resize-to', (event: Event) => {
      if (
        state.locked ||
        (state.simulationActive && String(block.type ?? '').startsWith('dashboard'))
      )
        return;
      const { width, height } = (event as CustomEvent<{ width: number; height: number }>).detail;
      if (!Number.isFinite(width) || !Number.isFinite(height)) return;
      resizeTo(width, height, usesGenericResize(block.type) ? captureGenericResize(block) : null);
      (C.state as any).fastRouting = false;
      C.updateConnections?.(true);
      window.dispatchEvent(new CustomEvent('diagramChanged'));
    });
    const finishResize = () => {
      if (!resizing) return;
      resizing = false;
      startPoint = null;
      startSize = null;
      genericStart = null;
      (C.state as any).fastRouting = false;
      (C.state as any).routingDirty = true;
      C.updateConnections?.(true);
      if (typeof window !== 'undefined') window.dispatchEvent(new CustomEvent('diagramChanged'));
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
