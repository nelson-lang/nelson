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
  ) => unknown;
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
      role: 'img',
      'aria-label': `${type === 'in' ? 'input' : 'output'} port ${index + 1}`,
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
      handlePortClick(group, (event as MouseEvent).shiftKey);
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

  // Connect the pending port to portEl, resolving which end is output and which
  // is input. Returns the output end on success (for Shift chaining), else null.
  function completeConnectionTo(portEl: Element): { blockId: string; index: number } | null {
    const from = S.pendingPort as {
      blockId: string;
      portType: 'in' | 'out';
      portIndex?: number;
    } | null;
    if (!from) return null;
    const blockId = portEl.getAttribute('data-block-id') ?? '';
    const portType = (portEl.getAttribute('data-port-type') as 'in' | 'out') ?? 'in';
    const portIndex = Number(portEl.getAttribute('data-port-index'));
    if (from.blockId === blockId) return null;
    if (portType === from.portType) {
      C.onConnectionError?.('Connect an output port to an input port.');
      return null;
    }
    const out = from.portType === 'out' ? from : { blockId, portIndex };
    const inp = from.portType === 'in' ? from : { blockId, portIndex };
    const created = C.createConnection?.(
      out.blockId,
      inp.blockId,
      inp.portIndex ?? 0,
      out.portIndex ?? 0
    );
    if (!created) return null;
    return { blockId: out.blockId, index: Number(out.portIndex ?? 0) };
  }

  // The DOM group of a given output port (used to re-arm a Shift chain).
  function outputPortEl(blockId: string, index: number): Element | null {
    return (
      C.svg?.querySelector(
        `.port-group[data-block-id="${blockId}"][data-port-type="out"][data-port-index="${index}"]`
      ) ?? null
    );
  }

  // Finish a connection. With Shift held and on success, keep the source output
  // pending so the next target chains from the same source.
  function finishConnection(targetEl: Element, chain: boolean) {
    const out = completeConnectionTo(targetEl);
    clearPending();
    if (chain && out) {
      const el = outputPortEl(out.blockId, out.index);
      if (el) beginPending(el);
    }
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

    // Press+drag on an already-connected input detaches that wire's end and
    // re-targets it (grab the input port, drag to another input), instead of
    // starting a new connection the occupied input would reject. The ghost is
    // anchored at the wire's source output so the input end follows the pointer.
    // This is the discoverable twin of the wire-endpoint grab band in
    // renderer-wire-reconnect.ts; both end in reconnectWireEndpoint.
    const portType = portEl.getAttribute('data-port-type');
    const blockId = portEl.getAttribute('data-block-id') ?? '';
    const portIndex = Number(portEl.getAttribute('data-port-index'));
    const detachConn =
      portType === 'in'
        ? ((S as any).connections as any[]).find(
            (c) => c.to === blockId && Number(c.toIndex ?? 0) === portIndex
          ) ?? null
        : null;

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
        if (detachConn) {
          // Anchor the ghost at the wire's source output; the input end moves.
          const anchor = outputPortEl(detachConn.from, Number(detachConn.fromIndex ?? 0));
          if (anchor) beginPending(anchor);
        } else {
          beginPending(portEl);
        }
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
      if (detachConn) {
        // Re-target the detached end onto an input port; anything else (an
        // output, or empty space) snaps the wire back to its original input.
        if (target && target.getAttribute('data-port-type') === 'in') {
          (C as any).reconnectWireEndpoint?.(
            detachConn,
            'to',
            target.getAttribute('data-block-id') ?? '',
            Number(target.getAttribute('data-port-index'))
          );
        }
        clearPending();
        return;
      }
      if (target) finishConnection(target, event.shiftKey);
      else clearPending();
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

  function handlePortClick(portEl: Element, shiftKey = false) {
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
    finishConnection(portEl, shiftKey);
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

  // beginPending and clearPending are shared with wire-endpoint re-targeting so
  // the ghost wire and target highlighting are not duplicated there.
  return {
    createPortCircle,
    handlePortClick,
    beginPending,
    clearPending,
    updatePortVisibility,
  };
}
//=============================================================================
