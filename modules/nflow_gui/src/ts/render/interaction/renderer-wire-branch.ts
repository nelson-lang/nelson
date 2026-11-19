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
// Wire fan-out: Ctrl or Cmd drag from a wire onto an input port adds a new
// connection from the source output of that wire.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { RendererCtx, Conn } from '../core/renderer-types.js';
//=============================================================================
type Point = { x: number; y: number };
type Active = {
  conn: Conn;
  pointerId: number;
  startX: number;
  startY: number;
  armed: boolean;
};
//=============================================================================
const ARM_THRESHOLD_PX = 4; // client-space travel before a press becomes a drag
//=============================================================================
export function makeWireBranchHandlers(ctx: RendererCtx) {
  const C = ctx as any;
  const S = ctx.state as any;
  let active: Active | null = null;

  const editingAllowed = (): boolean => !S.locked;

  const hasBranchModifier = (event: PointerEvent): boolean => !!(event.ctrlKey || event.metaKey);

  // The source output port whose DOM group drives the ghost while branching.
  function sourcePortEl(conn: Conn): Element | null {
    const index = Number(conn.fromIndex ?? 0);
    return (
      C.svg?.querySelector(
        `.port-group[data-block-id="${conn.from}"][data-port-type="out"][data-port-index="${index}"]`
      ) ?? null
    );
  }

  // Add a connection from the wire source to the target input port.
  // createConnection checks the locked, duplicate and single-source rules.
  function branch(conn: Conn, targetBlock: string, targetIndex: number): boolean {
    if (!editingAllowed()) {
      C.onConnectionError?.('Diagram is locked.');
      return false;
    }
    if (targetBlock === conn.from) {
      C.onConnectionError?.('Connect an output port to an input port.');
      return false;
    }
    return !!C.createConnection?.(conn.from, targetBlock, targetIndex, Number(conn.fromIndex ?? 0));
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
      const anchor = sourcePortEl(active.conn);
      if (anchor) C.beginPending?.(anchor);
    }
    // The ghost follows the pointer via the svg pointermove listener installed
    // by beginPending, so no extra preview update is needed here.
  }

  function onUp(event: PointerEvent): void {
    if (!active || event.pointerId !== active.pointerId) return;
    const current = active;
    teardown();
    active = null;
    if (!current.armed) return; // no drag: the wire's click handler selects it
    const el =
      typeof document !== 'undefined'
        ? document.elementFromPoint(event.clientX, event.clientY)
        : null;
    const portGroup = (el as Element | null)?.closest?.('.port-group') ?? null;
    if (portGroup) {
      if (portGroup.getAttribute('data-port-type') === 'in') {
        branch(
          current.conn,
          portGroup.getAttribute('data-block-id') ?? '',
          Number(portGroup.getAttribute('data-port-index'))
        );
      } else {
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

  // Arm a fan-out drag on a Ctrl or Cmd press over a wire. Returns true when it
  // takes the press, before endpoint re-targeting and segment dragging.
  function armWireBranchDrag(event: PointerEvent): boolean {
    if (!editingAllowed() || active || event.button !== 0) return false;
    if (!hasBranchModifier(event)) return false;
    const target = event.target as Element | null;
    if (!target || !(target.classList?.contains('wire-hit') || target.classList?.contains('wire')))
      return false;
    const point: Point = C.clientToSvg(event.clientX, event.clientY);
    const conn = C.findNearestConnection?.(point) as Conn | null;
    if (!conn) return false;
    active = {
      conn,
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

  // Arm a fan-out drag from a visible branch handle (the "+" on a selected
  // wire), no modifier needed. Same ghost and drop-to-connect flow as the
  // Ctrl/Cmd drag, so the gesture is discoverable without a shortcut.
  function armWireBranchFromHandle(event: PointerEvent, conn: Conn): boolean {
    if (!editingAllowed() || active || event.button !== 0) return false;
    active = {
      conn,
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

  function cancelWireBranchDrag(): boolean {
    if (!active) return false;
    const wasArmed = active.armed;
    teardown();
    active = null;
    if (wasArmed) C.clearPending?.();
    return true;
  }

  return {
    armWireBranchDrag,
    armWireBranchFromHandle,
    cancelWireBranchDrag,
    branchWireFromSource: branch,
  };
}
//=============================================================================
