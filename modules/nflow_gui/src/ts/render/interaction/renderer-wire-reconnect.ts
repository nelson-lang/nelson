//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Wire-endpoint re-targeting: grab a wire near one of its ends and drop it on
// another compatible port to move that endpoint, instead of deleting and
// recreating the connection. The ghost wire and compatible-target highlighting
// are shared with the port drag-to-connect flow (beginPending/clearPending).
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { RendererCtx, Conn } from '../core/renderer-types.js';
//=============================================================================
type Point = { x: number; y: number };
type End = 'from' | 'to';
type Active = {
  conn: Conn;
  end: End;
  pointerId: number;
  startX: number;
  startY: number;
  armed: boolean;
};
//=============================================================================
// Grab radius around a wire end. Ports render above wires and their 12px hit
// circle covers the tip, so this must clear it to expose a graspable band of
// wire just past the port (~12..22px from the tip).
const ENDPOINT_HIT_PX = 22;
const ARM_THRESHOLD_PX = 4; // client-space travel before a press becomes a drag
// Forgiving drop: when a release misses the small target port, snap to the
// nearest compatible port within this radius (SVG units), Simulink-style.
const ENDPOINT_SNAP_PX = 24;
//=============================================================================
export function makeWireReconnectHandlers(ctx: RendererCtx) {
  const C = ctx as any;
  const S = ctx.state as any;
  let active: Active | null = null;
  let hoverCursorSet = false;

  const editingAllowed = (): boolean => !S.locked;

  // The end being moved is driven by whichever wire tip the press is near:
  // points[0] is the source (output) tip, the last point the target (input) tip.
  function endpointAt(point: Point): { conn: Conn; end: End } | null {
    let best: { conn: Conn; end: End } | null = null;
    let bestDist = Infinity;
    for (const conn of S.connections as Conn[]) {
      const pts = conn.points as Point[] | undefined;
      if (!Array.isArray(pts) || pts.length < 2) continue;
      const ends: Array<[End, Point]> = [
        ['from', pts[0]],
        ['to', pts[pts.length - 1]],
      ];
      for (const [end, p] of ends) {
        const dist = Math.hypot(point.x - p.x, point.y - p.y);
        if (dist < bestDist) {
          bestDist = dist;
          best = { conn, end };
        }
      }
    }
    return bestDist <= ENDPOINT_HIT_PX ? best : null;
  }

  // The fixed (anchor) port is the opposite tip; its DOM group drives the ghost.
  function fixedPortEl(conn: Conn, movingEnd: End): Element | null {
    const anchorIsFrom = movingEnd === 'to';
    const blockId = anchorIsFrom ? conn.from : conn.to;
    const type = anchorIsFrom ? 'out' : 'in';
    const index = anchorIsFrom ? Number(conn.fromIndex ?? 0) : Number(conn.toIndex ?? 0);
    return (
      C.svg?.querySelector(
        `.port-group[data-block-id="${blockId}"][data-port-type="${type}"][data-port-index="${index}"]`
      ) ?? null
    );
  }

  function isPhysPort(blockId: string, index: number): boolean {
    const blk = S.blocks.get(blockId) as { ports?: Array<{ type?: string; index?: number }> };
    return !!blk?.ports?.some((p) => p.type === 'phys' && Number(p.index ?? -1) === index);
  }

  // Validate against the same rules as createConnection (locked, self, duplicate,
  // single-source input), excluding the edited wire, then move the endpoint and
  // reroute. Returns false (with an error surfaced) when the move is rejected.
  function reconnect(conn: Conn, end: End, targetBlock: string, targetIndex: number): boolean {
    if (!editingAllowed()) {
      C.onConnectionError?.('Diagram is locked.');
      return false;
    }
    // The endpoints as they stand are the pair being removed. Remember it so the
    // align-connect hint does not immediately re-propose the wire the user just
    // detached (Simulink leaves a re-targeted port silently unconnected).
    const removedFrom = conn.from;
    const removedFromIndex = Number(conn.fromIndex ?? 0);
    const removedTo = conn.to;
    const removedToIndex = Number(conn.toIndex ?? 0);
    const newFrom = end === 'from' ? targetBlock : conn.from;
    const newFromIndex = end === 'from' ? targetIndex : Number(conn.fromIndex ?? 0);
    const newTo = end === 'to' ? targetBlock : conn.to;
    const newToIndex = end === 'to' ? targetIndex : Number(conn.toIndex ?? 0);

    const unchanged =
      newFrom === conn.from &&
      newFromIndex === Number(conn.fromIndex ?? 0) &&
      newTo === conn.to &&
      newToIndex === Number(conn.toIndex ?? 0);
    if (unchanged) return false;

    if (newFrom === newTo) {
      C.onConnectionError?.('Connect an output port to an input port.');
      return false;
    }
    const duplicate = (S.connections as Conn[]).some(
      (c) =>
        c !== conn &&
        c.from === newFrom &&
        c.to === newTo &&
        Number(c.toIndex ?? 0) === newToIndex &&
        Number(c.fromIndex ?? 0) === newFromIndex
    );
    if (duplicate) {
      C.onConnectionError?.('Connection already exists.');
      return false;
    }
    const physical = isPhysPort(newFrom, newFromIndex) && isPhysPort(newTo, newToIndex);
    if (!physical) {
      const occupied = (S.connections as Conn[]).find(
        (c) => c !== conn && c.to === newTo && Number(c.toIndex ?? 0) === newToIndex
      );
      if (occupied) {
        C.onConnectionError?.(
          `Input ${newTo}.in${newToIndex} already has a connection from ${
            occupied.from
          }.out${Number(occupied.fromIndex ?? 0)}.`
        );
        return false;
      }
    }

    conn.from = newFrom;
    conn.fromIndex = newFromIndex;
    conn.to = newTo;
    conn.toIndex = newToIndex;
    // Endpoints moved: drop any pinned route and reflect phys/causal styling.
    conn.points = C.buildFastDragPathFromPorts ? C.buildFastDragPathFromPorts(conn) : [];
    const path = conn.path as Element | undefined;
    if (path) {
      path.setAttribute('class', physical ? 'wire physical-wire' : 'wire');
      if (physical) path.removeAttribute('marker-end');
      else path.setAttribute('marker-end', 'url(#wire-arrow)');
    }
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
    S.suppressedAlignPair = {
      from: removedFrom,
      fromIndex: removedFromIndex,
      to: removedTo,
      toIndex: removedToIndex,
    };
    // The endpoints moved, so the selected wire's handles follow to the new ports.
    C.updateWireCornerHandles?.();
    if (typeof window !== 'undefined') {
      window.dispatchEvent(new CustomEvent('diagramChanged'));
      window.dispatchEvent(new CustomEvent('diagramCommitted'));
      window.dispatchEvent(new CustomEvent('updateStatusBar'));
    }
    return true;
  }

  function teardown(): void {
    window.removeEventListener('pointermove', onMove, true);
    window.removeEventListener('pointerup', onUp, true);
    window.removeEventListener('pointercancel', onCancel, true);
  }

  function onMove(event: PointerEvent): void {
    if (!active || event.pointerId !== active.pointerId) return;
    if (!active.armed) {
      if (
        Math.hypot(event.clientX - active.startX, event.clientY - active.startY) < ARM_THRESHOLD_PX
      )
        return;
      active.armed = true;
      C.clearWireEndpointHandles?.();
      const anchor = fixedPortEl(active.conn, active.end);
      if (anchor) C.beginPending?.(anchor);
    }
    // The ghost follows the pointer via the svg pointermove listener installed by
    // beginPending, so no extra preview update is needed here.
  }

  // Nearest port of the wanted kind within the snap radius of an SVG-space point,
  // so a drop that misses the small port still lands on the obvious target.
  function nearestCompatiblePort(
    point: Point,
    wantType: 'in' | 'out'
  ): { blockId: string; index: number } | null {
    let best: { blockId: string; index: number } | null = null;
    let bestDist = Infinity;
    for (const [blockId, blk] of S.blocks as Map<
      string,
      { ports?: Array<{ type?: string; index?: number }> }
    >) {
      for (const p of blk?.ports ?? []) {
        if (p.type !== wantType) continue;
        const index = Number(p.index ?? 0);
        const pos = C.getPortPosition?.(blockId, wantType, index);
        if (!pos) continue;
        const dist = Math.hypot(point.x - pos.x, point.y - pos.y);
        if (dist < bestDist) {
          bestDist = dist;
          best = { blockId, index };
        }
      }
    }
    return best && bestDist <= ENDPOINT_SNAP_PX ? best : null;
  }

  function onUp(event: PointerEvent): void {
    if (!active || event.pointerId !== active.pointerId) return;
    const current = active;
    teardown();
    active = null;
    if (!current.armed) return; // no drag: the wire's click handler selects it
    const wantType = current.end === 'from' ? 'out' : 'in';
    const el =
      typeof document !== 'undefined'
        ? document.elementFromPoint(event.clientX, event.clientY)
        : null;
    const portGroup = (el as Element | null)?.closest?.('.port-group') ?? null;
    const directType = portGroup?.getAttribute('data-port-type') ?? null;
    if (portGroup && directType === wantType) {
      // Released squarely on a compatible port.
      reconnect(
        current.conn,
        current.end,
        portGroup.getAttribute('data-block-id') ?? '',
        Number(portGroup.getAttribute('data-port-index'))
      );
    } else {
      // Missed the port (or hit one of the wrong kind): snap to the nearest
      // compatible port within range, else leave the wire unchanged.
      const near = nearestCompatiblePort(C.clientToSvg(event.clientX, event.clientY), wantType);
      if (near) {
        reconnect(current.conn, current.end, near.blockId, near.index);
      } else if (portGroup) {
        C.onConnectionError?.('Connect an output port to an input port.');
      }
    }
    C.clearPending?.();
  }

  function onCancel(event: PointerEvent): void {
    if (!active || event.pointerId !== active.pointerId) return;
    const wasArmed = active.armed;
    teardown();
    active = null;
    if (wasArmed) C.clearPending?.();
  }

  // Arm a re-target drag when pressing on a wire near one of its endpoints. Takes
  // priority over segment dragging; returns true when it consumed the press.
  function armWireEndpointDrag(event: PointerEvent): boolean {
    if (!editingAllowed() || active || event.button !== 0) return false;
    const target = event.target as Element | null;
    if (!target || !(target.classList?.contains('wire-hit') || target.classList?.contains('wire')))
      return false;
    const point = C.clientToSvg(event.clientX, event.clientY);
    const hit = endpointAt(point);
    if (!hit) return false;
    active = {
      conn: hit.conn,
      end: hit.end,
      pointerId: event.pointerId,
      startX: event.clientX,
      startY: event.clientY,
      armed: false,
    };
    window.addEventListener('pointermove', onMove, true);
    window.addEventListener('pointerup', onUp, true);
    window.addEventListener('pointercancel', onCancel, true);
    return true;
  }

  // Arm a re-target drag from a visible endpoint handle (shown on a selected
  // wire), given the connection and which end the handle sits on. Same ghost and
  // drop-to-reconnect flow as the wire-band grab, so both ends behave the same.
  function armWireEndpointFromHandle(event: PointerEvent, conn: Conn, end: End): boolean {
    if (!editingAllowed() || active || event.button !== 0) return false;
    active = {
      conn,
      end,
      pointerId: event.pointerId,
      startX: event.clientX,
      startY: event.clientY,
      armed: false,
    };
    window.addEventListener('pointermove', onMove, true);
    window.addEventListener('pointerup', onUp, true);
    window.addEventListener('pointercancel', onCancel, true);
    return true;
  }

  function cancelWireEndpointDrag(): boolean {
    if (!active) return false;
    const wasArmed = active.armed;
    teardown();
    active = null;
    if (wasArmed) C.clearPending?.();
    return true;
  }

  // Hover feedback near a wire endpoint. Returns true (claiming the cursor) while
  // over an endpoint or during a drag, so segment hover yields the end zones.
  function handleWireEndpointHover(event: PointerEvent): boolean {
    if (active) return true;
    if (!C.svg) return false;
    const clearCursor = () => {
      if (hoverCursorSet) {
        C.svg.style.cursor = '';
        hoverCursorSet = false;
      }
    };
    if (!editingAllowed()) {
      clearCursor();
      return false;
    }
    const target = event.target as Element | null;
    const onWire =
      target && (target.classList?.contains('wire-hit') || target.classList?.contains('wire'));
    if (!onWire) {
      clearCursor();
      return false;
    }
    const point = C.clientToSvg(event.clientX, event.clientY);
    if (endpointAt(point)) {
      C.svg.style.cursor = 'crosshair';
      hoverCursorSet = true;
      return true;
    }
    clearCursor();
    return false;
  }

  return {
    armWireEndpointDrag,
    armWireEndpointFromHandle,
    cancelWireEndpointDrag,
    handleWireEndpointHover,
    reconnectWireEndpoint: reconnect,
    resolveWireEndpointAt: endpointAt,
  };
}
//=============================================================================
