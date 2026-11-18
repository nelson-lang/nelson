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
import { createSvgElement } from '../svg-elements.js';
import type { RendererCtx, Port, RendererState } from '../core/renderer-types.js';
//=============================================================================
interface ExtCtx {
  svg: SVGSVGElement | null;
  onConnectionError?: (message: string) => void;
  createConnection?: (
    fromBlockId: string,
    toBlockId: string,
    toIndex: number,
    fromIndex: number
  ) => void;
}
//=============================================================================
interface ExtState extends RendererState {
  selectedId?: string | null;
  suppressNextPortClick?: boolean;
}
//=============================================================================
const DRAG_CONNECT_THRESHOLD_PX = 4; // client-space travel before a port press becomes a drag
//=============================================================================
export function makePortHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as ExtCtx;
  const S = ctx.state as unknown as ExtState;
  let previewPath: SVGPathElement | null = null;
  let previewSource: { x: number; y: number } | null = null;

  const clientToSvg = (clientX: number, clientY: number): { x: number; y: number } => {
    const svg = C.svg;
    const matrix = svg?.getScreenCTM?.();
    if (!svg || !matrix) return { x: clientX, y: clientY };
    const point = svg.createSVGPoint();
    point.x = clientX;
    point.y = clientY;
    return point.matrixTransform(matrix.inverse());
  };

  const updatePreview = (event: PointerEvent): void => {
    if (!previewPath || !previewSource) return;
    const target = clientToSvg(event.clientX, event.clientY);
    const dx = Math.max(30, Math.abs(target.x - previewSource.x) * 0.45);
    previewPath.setAttribute(
      'd',
      `M ${previewSource.x} ${previewSource.y} C ${previewSource.x + dx} ${previewSource.y} ${
        target.x - dx
      } ${target.y} ${target.x} ${target.y}`
    );
  };

  const startPreview = (portEl: Element): void => {
    const rect = portEl.getBoundingClientRect();
    previewSource = clientToSvg(rect.left + rect.width / 2, rect.top + rect.height / 2);
    previewPath = createSvgElement('path', { class: 'connection-preview' }) as SVGPathElement;
    C.svg?.querySelector('#overlayLayer')?.appendChild(previewPath);
    C.svg?.addEventListener('pointermove', updatePreview);
  };

  const stopPreview = (): void => {
    C.svg?.removeEventListener('pointermove', updatePreview);
    previewPath?.remove();
    previewPath = null;
    previewSource = null;
  };

  function createPortCircle(
    blockId: string,
    type: 'in' | 'out',
    index: number,
    port: Port,
    blockType?: string
  ) {
    void blockType;
    const group = createSvgElement('g', {
      class: 'port-group',
      'data-block-id': blockId,
      'data-port-type': type,
      'data-port-index': index,
    });

    const hitX = port.x;
    const hitY = port.y;
    const hit = createSvgElement('circle', { cx: hitX, cy: hitY, r: 12, class: 'port-hit' });
    const shape =
      type === 'in'
        ? createSvgElement('rect', {
            x: port.x - 6,
            y: port.y - 6,
            width: 12,
            height: 12,
            class: `port ${type} port-square`,
          })
        : createSvgElement('polygon', {
            points: `${port.x - 6},${port.y - 6} ${port.x - 6},${port.y + 6} ${port.x + 6},${
              port.y
            }`,
            class: `port ${type} port-triangle`,
          });

    const onClick = (event: Event) => {
      event.stopPropagation();
      handlePortClick(group);
    };

    group.addEventListener('pointerenter', () => group.classList.add('hovered'));
    group.addEventListener('pointerleave', () => group.classList.remove('hovered'));
    group.addEventListener('pointerdown', (event) =>
      onPortPointerDown(event as PointerEvent, group)
    );
    hit.addEventListener('click', onClick);
    shape.addEventListener('click', onClick);
    group.appendChild(hit);
    group.appendChild(shape);
    return group;
  }

  // Begin the pending state from either port side: highlight the opposite side as
  // compatible targets, mark the source and show the ghost wire that follows the pointer.
  function beginPending(portEl: Element) {
    const blockId = portEl.getAttribute('data-block-id') ?? '';
    const portType = (portEl.getAttribute('data-port-type') as 'in' | 'out') ?? 'in';
    const portIndex = Number(portEl.getAttribute('data-port-index'));
    S.pendingPort = { blockId, portType, portIndex } as unknown;
    const dot = portEl.querySelector('.port');
    if (dot) dot.classList.add('pending');
    C.svg?.classList.add('connection-pending');
    const targetType = portType === 'out' ? 'in' : 'out';
    C.svg
      ?.querySelectorAll(`.port-group[data-port-type="${targetType}"]`)
      .forEach((port) => port.classList.add('compatible'));
    C.svg
      ?.querySelectorAll(`.port-group[data-port-type="${portType}"]`)
      .forEach((port) => port.classList.add('incompatible'));
    startPreview(portEl);
    updatePortVisibility();
  }

  // Create a connection between the pending port and `portEl`, resolving which
  // end is the output and which is the input so either drag direction works.
  function completeConnectionTo(portEl: Element) {
    const from = S.pendingPort as {
      blockId: string;
      portType: 'in' | 'out';
      portIndex?: number;
    } | null;
    if (!from) return;
    const blockId = portEl.getAttribute('data-block-id') ?? '';
    const portType = (portEl.getAttribute('data-port-type') as 'in' | 'out') ?? 'in';
    const portIndex = Number(portEl.getAttribute('data-port-index'));
    if (from.blockId === blockId) return;
    if (portType === from.portType) {
      C.onConnectionError?.('Connect an output port to an input port.');
      return;
    }
    const out = from.portType === 'out' ? from : { blockId, portIndex };
    const inp = from.portType === 'in' ? from : { blockId, portIndex };
    C.createConnection?.(out.blockId, inp.blockId, inp.portIndex ?? 0, out.portIndex ?? 0);
  }

  // Press on a port: arm a drag-to-connect. A real drag (past the threshold) draws
  // the ghost and creates the wire on release over a port; a press with no travel
  // falls through to the click handler so the click-then-click flow still works.
  function onPortPointerDown(startEvent: PointerEvent, portEl: Element) {
    if (startEvent.button !== 0) return;
    // Drop any stale suppression from a previous drag so this fresh press is honored.
    S.suppressNextPortClick = false;
    if (S.pendingPort) return; // a connection is already being composed via clicks
    const pointerId = startEvent.pointerId;
    const startX = startEvent.clientX;
    const startY = startEvent.clientY;
    let armed = false;

    const cleanup = () => {
      window.removeEventListener('pointermove', onMove, true);
      window.removeEventListener('pointerup', onUp, true);
      window.removeEventListener('pointercancel', onCancel, true);
    };

    const onMove = (event: PointerEvent) => {
      if (event.pointerId !== pointerId) return;
      if (!armed) {
        if (Math.hypot(event.clientX - startX, event.clientY - startY) < DRAG_CONNECT_THRESHOLD_PX)
          return;
        armed = true;
        beginPending(portEl);
      }
      updatePreview(event);
    };

    const onUp = (event: PointerEvent) => {
      if (event.pointerId !== pointerId) return;
      cleanup();
      if (!armed) return; // no drag: let the trailing click compose the connection
      // A drag completed: neutralize the synthetic click that may follow.
      S.suppressNextPortClick = true;
      const target = (event.target as Element | null)?.closest?.('.port-group') ?? null;
      if (target) completeConnectionTo(target);
      clearPending();
    };

    const onCancel = (event: PointerEvent) => {
      if (event.pointerId !== pointerId) return;
      cleanup();
      if (armed) clearPending();
    };

    window.addEventListener('pointermove', onMove, true);
    window.addEventListener('pointerup', onUp, true);
    window.addEventListener('pointercancel', onCancel, true);
  }

  function handlePortClick(portEl: Element) {
    if (S.suppressNextPortClick) {
      S.suppressNextPortClick = false;
      return;
    }
    const blockId = portEl.getAttribute('data-block-id') ?? '';

    // First click: start pending from this port (either side is accepted, so a
    // press on an input gives immediate feedback and routes input -> output).
    if (!S.pendingPort) {
      beginPending(portEl);
      return;
    }

    const from = S.pendingPort as { blockId: string };
    if (from.blockId === blockId) {
      clearPending();
      return;
    }
    completeConnectionTo(portEl);
    clearPending();
  }

  function clearPending() {
    const pending = C.svg?.querySelector('.port.pending');
    if (pending) pending.classList.remove('pending');
    S.pendingPort = null;
    C.svg?.classList.remove('connection-pending');
    C.svg
      ?.querySelectorAll('.port-group.compatible, .port-group.incompatible')
      .forEach((port) => port.classList.remove('compatible', 'incompatible'));
    stopPreview();
    updatePortVisibility();
  }

  function updatePortVisibility() {
    const show = Boolean(
      (S.selectedIds && S.selectedIds.size > 0) || S.selectedId || S.pendingPort
    );
    if (!C.svg) return;
    C.svg.classList.toggle('ports-visible', show);
  }

  // beginPending/clearPending are shared with wire-endpoint re-targeting (P5) so
  // the ghost wire and compatible-target highlighting are not duplicated there.
  return {
    createPortCircle,
    handlePortClick,
    beginPending,
    clearPending,
    updatePortVisibility,
  };
}
//=============================================================================
