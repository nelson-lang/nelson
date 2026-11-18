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
// Insert a block on a wire. Dropping a block on a wire cuts it and wires the
// block between the two endpoints. A failed splice restores the original wire.
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { RendererCtx, Conn, Block } from '../core/renderer-types.js';
//=============================================================================
type Point = { x: number; y: number };
//=============================================================================
// Input/output counts, from the block's direct counters with a ports fallback.
function portCounts(block: any): { inputs: number; outputs: number } {
  const ports = Array.isArray(block?.ports) ? block.ports : [];
  const inputs = Number.isFinite(block?.inputs)
    ? Number(block.inputs)
    : ports.filter((p: any) => p?.type === 'in').length;
  const outputs = Number.isFinite(block?.outputs)
    ? Number(block.outputs)
    : ports.filter((p: any) => p?.type === 'out').length;
  return { inputs, outputs };
}
//=============================================================================
// First input index of `blockId` without an incoming connection, or -1.
function firstFreeInput(connections: Conn[], blockId: string, inputCount: number): number {
  const occupied = new Set(
    connections.filter((c) => String(c.to) === String(blockId)).map((c) => Number(c.toIndex ?? 0))
  );
  for (let i = 0; i < inputCount; i += 1) {
    if (!occupied.has(i)) return i;
  }
  return -1;
}
//=============================================================================
export function makeInsertOnWireHandlers(ctx: RendererCtx) {
  const C = ctx as any;
  const S = ctx.state as any;

  const editingAllowed = (): boolean => !S.locked;

  // A physical or acausal wire has no direction, so do not splice a block in.
  const isPhysicalWire = (conn: Conn): boolean =>
    !!(conn.path as Element)?.classList?.contains?.('physical-wire');

  function blockCenter(block: Block): Point {
    const w = (block as any).width ?? 80;
    const h = (block as any).height ?? 80;
    return { x: block.x + w / 2, y: block.y + h / 2 };
  }

  // Try to splice `blockId` onto the wire under `point` (block center if
  // omitted). Returns true when the block was inserted.
  function insertBlockOnWire(blockId: string, point?: Point): boolean {
    if (!editingAllowed()) return false;
    const block = S.blocks.get(blockId) as Block | undefined;
    if (!block) return false;

    const { inputs, outputs } = portCounts(block);
    if (inputs < 1 || outputs < 1) return false;

    const center = point ?? blockCenter(block);
    const w = (block as any).width ?? 80;
    const h = (block as any).height ?? 80;
    const threshold = Math.max(24, Math.min(w, h) / 2);
    const conn = C.findNearestConnection?.(center, threshold) as Conn | null;
    if (!conn) return false;

    // Skip wires already touching this block and physical/acausal wires.
    if (conn.from === blockId || conn.to === blockId) return false;
    if (isPhysicalWire(conn)) return false;

    const inIndex = firstFreeInput(S.connections, blockId, inputs);
    if (inIndex < 0) return false;

    // Remember the original endpoints, then cut and re-wire through the block.
    const from = conn.from;
    const fromIndex = Number(conn.fromIndex ?? 0);
    const to = conn.to;
    const toIndex = Number(conn.toIndex ?? 0);

    C.deleteConnection?.(conn);
    const inbound = C.createConnection?.(from, blockId, inIndex, fromIndex) as Conn | null;
    const outbound = inbound
      ? (C.createConnection?.(blockId, to, toIndex, 0) as Conn | null)
      : null;

    if (!inbound || !outbound) {
      // Rollback: drop any partial wire and restore the original connection.
      if (inbound) C.deleteConnection?.(inbound);
      if (outbound) C.deleteConnection?.(outbound);
      C.createConnection?.(from, to, toIndex, fromIndex);
      return false;
    }

    S.routingDirty = true;
    C.updateConnections?.(true);
    return true;
  }

  return { insertBlockOnWire };
}
//=============================================================================
