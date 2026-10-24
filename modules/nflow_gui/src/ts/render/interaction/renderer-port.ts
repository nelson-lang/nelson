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
    hit.addEventListener('click', onClick);
    shape.addEventListener('click', onClick);
    group.appendChild(hit);
    group.appendChild(shape);
    return group;
  }

  function handlePortClick(portEl: Element) {
    if (S.suppressNextPortClick) {
      S.suppressNextPortClick = false;
      return;
    }
    const blockId = portEl.getAttribute('data-block-id') ?? '';
    const portType = (portEl.getAttribute('data-port-type') as 'in' | 'out') ?? 'in';
    const portIndex = Number(portEl.getAttribute('data-port-index'));

    if (!S.pendingPort) {
      if (portType !== 'out') return;
      S.pendingPort = { blockId, portType, portIndex } as unknown;
      const dot = portEl.querySelector('.port');
      if (dot) dot.classList.add('pending');
      C.svg?.classList.add('connection-pending');
      C.svg
        ?.querySelectorAll('.port-group[data-port-type="in"]')
        .forEach((port) => port.classList.add('compatible'));
      C.svg
        ?.querySelectorAll('.port-group[data-port-type="out"]')
        .forEach((port) => port.classList.add('incompatible'));
      startPreview(portEl);
      updatePortVisibility();
      return;
    }

    const from = S.pendingPort as { blockId: string; portType: string; portIndex?: number };
    if (from.blockId === blockId) {
      clearPending();
      return;
    }
    if (portType === 'in') {
      C.createConnection?.(from.blockId, blockId, portIndex, from.portIndex ?? 0);
    } else {
      C.onConnectionError?.('Connect an output port to an input port.');
    }
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

  return { createPortCircle, handlePortClick, clearPending, updatePortVisibility };
}
//=============================================================================
