//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getRotatedBounds, segmentHitsRect } from '../geometry.js';
import type { RendererCtx, Conn, Block } from '../core/renderer-types.js';
import type { Point } from '../geometry.js';
import { emitAppEvent } from '../../app/app-events.js';
//=============================================================================
export function makeSelectionHandlers(ctx: RendererCtx) {
  type StateExt = {
    selectedId?: string | null;
    selectedConnection?: Conn | null;
    selectedIds?: Set<string>;
    selectedConnections?: Set<Conn>;
    suppressNextCanvasClick?: boolean;
    blocks: Map<string, Block>;
    connections: Conn[];
  };
  const S = ctx.state as unknown as StateExt;
  const C = ctx as unknown as {
    clientToSvg: (x: number, y: number) => Point;
    marqueeState: { active?: boolean; pointerId?: number | null; start?: Point | null };
    svg?: SVGSVGElement | null;
    buildFastDragPathFromPorts?: (conn: Conn) => Point[];
    applyWireOffsets?: (conn: Conn, p: Point[]) => Point[];
    onSelectBlock: (arg: unknown) => void;
    onSelectConnection: (arg: unknown) => void;
    updatePortVisibility?: () => void;
    updateSelectionBox?: () => void;
    updateWireCornerHandles?: () => void;
  };

  function selectBlock(blockId: string | null) {
    S.selectedId = blockId;
    S.selectedConnection = null;
    if (S.selectedIds) {
      S.selectedIds.clear();
      if (blockId) S.selectedIds.add(blockId);
    }
    if (S.selectedConnections) S.selectedConnections.clear();
    S.blocks.forEach((block) => {
      if (block && block.group) block.group.classList.toggle('selected', block.id === blockId);
    });
    S.connections.forEach((conn) => {
      if (conn && conn.path) conn.path.classList.toggle('selected', false);
    });
    C.onSelectBlock(blockId ? S.blocks.get(blockId) : null);
    C.updatePortVisibility?.();
    C.updateSelectionBox?.();
    C.updateWireCornerHandles?.();
    // Report the current block selection so gcb reflects the editor.
    emitAppEvent('nf-block-selected', { blockId });
  }

  function selectConnection(conn: Conn | null) {
    S.selectedConnection = conn;
    S.selectedId = null;
    if (S.selectedConnections) {
      S.selectedConnections.clear();
      if (conn) S.selectedConnections.add(conn);
    }
    if (S.selectedIds) S.selectedIds.clear();
    S.blocks.forEach((block) => {
      if (block && block.group) block.group.classList.toggle('selected', false);
    });
    S.connections.forEach((c) => {
      if (c && c.path) c.path.classList.toggle('selected', c === conn);
    });
    emitAppEvent('nf-block-selected', { blockId: null });
    if (!conn) {
      C.onSelectConnection(null);
      C.updatePortVisibility?.();
      C.updateSelectionBox?.();
      return;
    }
    const fromBlock = S.blocks.get(conn.from);
    const toBlock = S.blocks.get(conn.to);
    C.onSelectConnection({
      kind: 'connection',
      fromId: conn.from,
      toId: conn.to,
      toIndex: conn.toIndex,
      fromType: fromBlock?.type || 'unknown',
      toType: toBlock?.type || 'unknown',
    });
    C.updatePortVisibility?.();
    C.updateSelectionBox?.();
    C.updateWireCornerHandles?.();
  }

  function setMultiSelection(blockIds: Set<string>, connections: Set<Conn>) {
    if (S.selectedIds) {
      S.selectedIds.clear();
      blockIds.forEach((id) => S.selectedIds!.add(id));
    }
    if (S.selectedConnections) {
      S.selectedConnections.clear();
      connections.forEach((conn) => S.selectedConnections!.add(conn));
    }
    S.selectedId = blockIds.size === 1 && connections.size === 0 ? Array.from(blockIds)[0] : null;
    S.selectedConnection =
      connections.size === 1 && blockIds.size === 0 ? (Array.from(connections)[0] as Conn) : null;
    S.blocks.forEach((block) =>
      block.group?.classList.toggle('selected', blockIds.has(block.id ?? ''))
    );
    S.connections.forEach((conn) => conn.path?.classList.toggle('selected', connections.has(conn)));
    if (S.selectedId) {
      C.onSelectBlock(S.blocks.get(S.selectedId));
    } else if (S.selectedConnection) {
      const c = S.selectedConnection as Conn;
      const fromBlock = S.blocks.get(c.from);
      const toBlock = S.blocks.get(c.to);
      C.onSelectConnection({
        kind: 'connection',
        fromId: c.from,
        toId: c.to,
        toIndex: c.toIndex,
        fromType: fromBlock?.type || 'unknown',
        toType: toBlock?.type || 'unknown',
      });
    } else if (blockIds.size || connections.size) {
      C.onSelectBlock({ kind: 'multi', blocks: blockIds.size, connections: connections.size });
    } else {
      C.onSelectBlock(null);
      C.onSelectConnection(null);
    }
    C.updatePortVisibility?.();
    C.updateSelectionBox?.();
    C.updateWireCornerHandles?.();
  }

  function updateMarqueeRect(start: Point, current: Point) {
    const left = Math.min(start.x, current.x);
    const right = Math.max(start.x, current.x);
    const top = Math.min(start.y, current.y);
    const bottom = Math.max(start.y, current.y);
    ctx.marqueeRect!.setAttribute('display', 'block');
    ctx.marqueeRect!.setAttribute('x', String(left));
    ctx.marqueeRect!.setAttribute('y', String(top));
    ctx.marqueeRect!.setAttribute('width', String(right - left));
    ctx.marqueeRect!.setAttribute('height', String(bottom - top));
    return { left, right, top, bottom };
  }

  function startMarqueeSelection(event: PointerEvent) {
    if ((event as PointerEvent).button !== 0) return;
    const point = C.clientToSvg(event.clientX, event.clientY);
    C.marqueeState.active = true;
    C.marqueeState.pointerId = event.pointerId as unknown as number;
    C.marqueeState.start = point;
    updateMarqueeRect(point, point);
    try {
      if (C.svg && C.svg.setPointerCapture) C.svg.setPointerCapture(event.pointerId);
    } catch (_err) {}
  }

  function finishMarqueeSelection(event: PointerEvent) {
    const point = C.clientToSvg(event.clientX, event.clientY);
    const rect = updateMarqueeRect(C.marqueeState.start as Point, point);
    ctx.marqueeRect!.setAttribute('display', 'none');
    C.marqueeState.active = false;
    C.marqueeState.pointerId = null;
    C.marqueeState.start = null;
    S.suppressNextCanvasClick = true;

    const selectedBlocks = new Set<string>();
    S.blocks.forEach((block) => {
      const bounds = getRotatedBounds(block);
      if (
        rect.left <= bounds.right &&
        rect.right >= bounds.left &&
        rect.top <= bounds.bottom &&
        rect.bottom >= bounds.top
      ) {
        if (block.id) selectedBlocks.add(block.id);
      }
    });
    const selectedConnections = new Set<Conn>();
    S.connections.forEach((conn) => {
      let points = conn.points || [];
      if ((!points || points.length < 2) && C.buildFastDragPathFromPorts)
        points = C.buildFastDragPathFromPorts(conn);
      const renderPoints =
        ctx.applyWireOffsets || C.applyWireOffsets
          ? ctx.applyWireOffsets
            ? ctx.applyWireOffsets(conn, points)
            : C.applyWireOffsets!(conn, points)
          : points;
      for (let i = 0; i < renderPoints.length - 1; i += 1) {
        if (segmentHitsRect(renderPoints[i], renderPoints[i + 1], rect)) {
          selectedConnections.add(conn);
          break;
        }
      }
    });
    setMultiSelection(selectedBlocks, selectedConnections);
  }

  return {
    selectBlock,
    selectConnection,
    setMultiSelection,
    updateMarqueeRect,
    startMarqueeSelection,
    finishMarqueeSelection,
  };
}
//=============================================================================
