//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { distancePointToSegment } from '../geometry.js';
import { createSvgElement } from '../svg-elements.js';
import type { RendererCtx, Conn } from '../core/renderer-types.js';
import { state } from '../../app/state.js';
import * as FieldNames from '../../FieldNames.js';
//=============================================================================
export function makeConnectionHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as {
    wireLayer?: Element | null;
    buildFastDragPathFromPorts?: (conn: Conn) => { x: number; y: number }[];
    applyWirePathsFast?: (s: Set<Conn>) => void;
    queueRouteAfterPreview?: () => void;
    applyWirePaths?: (m: Map<unknown, unknown>) => void;
    updateWireCornerHandles?: () => void;
    onConnectionError?: (msg: string) => void;
    selectConnection?: (c: Conn | null) => void;
    lastConnectionError?: unknown;
    updateConnections?: (force?: boolean) => void;
  };
  const S = ctx.state as unknown as RendererCtx['state'] & {
    loadingDiagram?: boolean;
    dirtyConnections?: Set<Conn>;
    routingDirty?: boolean;
  };

  function createConnection(
    fromId: string,
    toId: string,
    toIndex?: number,
    fromIndex = 0
  ): Conn | null {
    if (state.locked) {
      C.lastConnectionError = { reason: FieldNames.kLocked, message: 'Diagram is locked.' };
      if (typeof C.onConnectionError === 'function') C.onConnectionError('Diagram is locked.');
      return null;
    }
    C.lastConnectionError = null;
    const duplicate = S.connections.find(
      (c) =>
        c.from === fromId && c.to === toId && c.toIndex === toIndex && c.fromIndex === fromIndex
    );
    if (duplicate) {
      C.lastConnectionError = { reason: 'duplicate', message: 'Connection already exists.' };
      if (typeof C.onConnectionError === 'function')
        C.onConnectionError('Connection already exists.');
      return null;
    }
    // A physical (acausal) pin is undirected: a 'phys' port on both ends. Such
    // a node accepts many connections (fan-out into one electrical/mechanical
    // node), so the single-source "input occupied" rule and the directed
    // arrowhead do not apply.
    const isPhysPort = (blockId: string, index: number): boolean => {
      const blk = S.blocks.get(blockId) as { ports?: Array<{ type?: string; index?: number }> };
      return !!blk?.ports?.some((p) => p.type === 'phys' && Number(p.index ?? -1) === index);
    };
    const physical =
      isPhysPort(fromId, Number(fromIndex ?? 0)) && isPhysPort(toId, Number(toIndex ?? 0));
    if (!physical) {
      const occupied = ctx.state.connections.find(
        (c) => c.to === toId && Number(c.toIndex ?? 0) === Number(toIndex ?? 0)
      );
      if (occupied) {
        const message = `Input ${toId}.in${Number(toIndex ?? 0)} already has a connection from ${
          occupied.from
        }.out${Number(occupied.fromIndex ?? 0)}.`;
        C.lastConnectionError = { reason: 'input_occupied', message };
        if (typeof C.onConnectionError === 'function') C.onConnectionError(message);
        return null;
      }
    }

    const path = createSvgElement('path', {
      class: physical ? 'wire physical-wire' : 'wire',
      ...(physical ? {} : { 'marker-end': 'url(#wire-arrow)' }),
    });
    path.setAttribute('stroke', '#007acc');
    path.setAttribute('stroke-width', '2');
    const hitPath = createSvgElement('path', { class: 'wire-hit' });
    C.wireLayer?.appendChild(hitPath);
    C.wireLayer?.appendChild(path);
    const conn: Conn = {
      from: fromId,
      to: toId,
      toIndex,
      fromIndex,
      path,
      hitPath,
      points: [],
    } as unknown as Conn;
    const onSelect = (event: Event) => {
      event.stopPropagation();
      C.selectConnection?.(conn);
    };
    path.addEventListener('click', onSelect);
    hitPath.addEventListener('click', onSelect);
    S.connections.push(conn);
    if (S.loadingDiagram) {
      conn.path.setAttribute('d', '');
      if (conn.hitPath) conn.hitPath.setAttribute('d', '');
    } else {
      conn.points = C.buildFastDragPathFromPorts ? C.buildFastDragPathFromPorts(conn) : [];
      C.applyWirePathsFast?.(new Set([conn]));
      S.routingDirty = true;
      if (!S.dirtyConnections) S.dirtyConnections = new Set();
      S.dirtyConnections.add(conn);
      if (S.autoRoute !== false) {
        C.queueRouteAfterPreview?.();
      } else {
        S.routingDirty = false;
        C.applyWirePaths?.(new Map());
        C.updateWireCornerHandles?.();
      }
    }
    if (typeof window !== 'undefined') {
      window.dispatchEvent(new CustomEvent('diagramChanged'));
      window.dispatchEvent(new CustomEvent('updateStatusBar'));
    }
    return conn;
  }

  function findNearestConnection(point: { x: number; y: number }, threshold = 12) {
    let best: Conn | null = null;
    let bestDist = Infinity;
    ctx.state.connections.forEach((conn) => {
      const points = conn.points || [];
      for (let i = 0; i < points.length - 1; i += 1) {
        const dist = distancePointToSegment(point, points[i], points[i + 1]);
        if (dist < bestDist) {
          bestDist = dist;
          best = conn;
        }
      }
    });
    return bestDist <= threshold ? best : null;
  }

  function deleteConnection(conn: Conn) {
    if (state.locked) return;
    const idx = ctx.state.connections.indexOf(conn);
    if (idx >= 0) {
      ctx.state.connections[idx].path.remove();
      ctx.state.connections[idx].hitPath?.remove();
      ctx.state.connections.splice(idx, 1);
    }
    S.routingDirty = true;
    C.updateConnections?.();
    if (typeof window !== 'undefined') {
      window.dispatchEvent(new CustomEvent('diagramChanged'));
      window.dispatchEvent(new CustomEvent('updateStatusBar'));
    }
  }

  function deleteBlock(blockId: string) {
    if (state.locked) return;
    const block = ctx.state.blocks.get(blockId as string);
    if (!block) return;
    const related = ctx.state.connections.filter(
      (conn) => conn.from === blockId || conn.to === blockId
    );
    related.forEach((c) => deleteConnection(c));
    block.group?.remove();
    ctx.state.blocks.delete(blockId);
    S.routingDirty = true;
    C.updateConnections?.();
    if (typeof window !== 'undefined') {
      window.dispatchEvent(new CustomEvent('diagramChanged'));
      window.dispatchEvent(new CustomEvent('updateStatusBar'));
    }
  }

  return { createConnection, findNearestConnection, deleteConnection, deleteBlock };
}
//=============================================================================
