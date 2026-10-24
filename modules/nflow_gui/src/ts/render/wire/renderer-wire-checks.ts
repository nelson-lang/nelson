//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { alignToGrid, GRID_SIZE } from '../geometry.js';
import {
  rotatePoint,
  getPortSide,
  blockBounds,
  segmentHitsRect,
  segmentsOverlap,
} from '../geometry.js';
import type { RendererCtx, Conn, Block } from '../core/renderer-types.js';
import type { Segment } from './wire-paths.js';
//=============================================================================
export function makeWireCheckHandlers(ctx: RendererCtx) {
  const C = ctx as unknown as {
    getWirePosition?: (
      id: string,
      type: 'in' | 'out' | 'phys',
      index: number
    ) => { x: number; y: number } | null;
  };
  function checkPortDirections(conn: Conn, points: { x: number; y: number }[], issues: string[]) {
    const fromBlock = ctx.state.blocks.get(conn.from);
    const toBlock = ctx.state.blocks.get(conn.to);
    if (!fromBlock || !toBlock) return false;
    const fromIndex = conn.fromIndex ?? 0;
    const fromPorts = (fromBlock.ports ?? []) as {
      type: 'in' | 'out' | 'phys';
      index: number;
      x: number;
      y: number;
    }[];
    const toPorts = (toBlock.ports ?? []) as {
      type: 'in' | 'out' | 'phys';
      index: number;
      x: number;
      y: number;
    }[];
    const fromPort = fromPorts.find(
      (p) => (p.type === 'out' || p.type === 'phys') && p.index === fromIndex
    );
    const toIndex = conn.toIndex ?? 0;
    const toPort = toPorts.find(
      (p) => (p.type === 'in' || p.type === 'phys') && p.index === toIndex
    );
    if (!fromPort || !toPort) return false;
    const fromPos = rotatePoint(
      { x: fromBlock.x + fromPort.x, y: fromBlock.y + fromPort.y },
      fromBlock
    );
    const toPos = rotatePoint({ x: toBlock.x + toPort.x, y: toBlock.y + toPort.y }, toBlock);
    const fromPortPos = alignToGrid(fromPos);
    const toPortPos = alignToGrid(toPos);
    const fromWirePos = C.getWirePosition ? C.getWirePosition(conn.from, 'out', fromIndex) : null;
    const toWirePos = C.getWirePosition
      ? C.getWirePosition(conn.to, 'in', conn.toIndex ?? 0)
      : null;
    const start = points[0];
    const next = points[1];
    const last = points[points.length - 1];
    const prev = points[points.length - 2];
    const fromSide = getPortSide(fromBlock, fromPos);
    const toSide = getPortSide(toBlock, toPos);
    const startPos =
      fromWirePos && start.x === fromWirePos.x && start.y === fromWirePos.y
        ? fromWirePos
        : fromPortPos;
    const endPos =
      toWirePos && last.x === toWirePos.x && last.y === toWirePos.y ? toWirePos : toPortPos;
    if (start.x !== fromPortPos.x || start.y !== fromPortPos.y) {
      if (!fromWirePos || start.x !== fromWirePos.x || start.y !== fromWirePos.y) {
        issues.push('start port mismatch');
        return false;
      }
    }
    if (last.x !== toPortPos.x || last.y !== toPortPos.y) {
      if (!toWirePos || last.x !== toWirePos.x || last.y !== toWirePos.y) {
        issues.push('end port mismatch');
        return false;
      }
    }
    if (!isValidStub(startPos, next, fromSide)) {
      issues.push(
        `bad start stub (${fromSide}) port=${startPos.x},${startPos.y} next=${next.x},${next.y}`
      );
      return false;
    }
    if (!isValidStub(endPos, prev, toSide)) {
      issues.push(`bad end stub (${toSide}) port=${endPos.x},${endPos.y} prev=${prev.x},${prev.y}`);
      return false;
    }
    return true;
  }

  function isValidStub(
    port: { x: number; y: number },
    other: { x: number; y: number },
    side: string
  ) {
    if (side === 'left') return other.y === port.y && other.x <= port.x - GRID_SIZE;
    if (side === 'right') return other.y === port.y && other.x >= port.x + GRID_SIZE;
    if (side === 'top') return other.x === port.x && other.y <= port.y - GRID_SIZE;
    return other.x === port.x && other.y >= port.y + GRID_SIZE;
  }

  function segmentHitsAnyBlock(segments: Segment[], conn: Conn, issues: string[]) {
    void issues;
    const fromBlock = ctx.state.blocks.get(conn.from);
    const toBlock = ctx.state.blocks.get(conn.to);
    const keepouts: { left: number; right: number; top: number; bottom: number }[] = [];
    ctx.state.blocks.forEach((block) => {
      if (block === fromBlock || block === toBlock) return;
      keepouts.push(blockBounds(block as Block));
    });
    for (let i = 0; i < segments.length; i += 1) {
      const seg = segments[i];
      for (let r = 0; r < keepouts.length; r += 1) {
        if (segmentHitsRect(seg.a, seg.b, keepouts[r])) return true;
      }
    }
    return false;
  }

  function segmentOverlapsOtherWire(segments: Segment[], conn: Conn, issues: string[]) {
    void issues;
    for (let i = 0; i < ctx.state.connections.length; i += 1) {
      const other = ctx.state.connections[i];
      if (other === conn) continue;
      if (shareSamePort(conn, other)) continue;
      const otherPts = (other.points || []) as { x: number; y: number }[];
      const otherSegs = otherPts
        .map((_, idx, arr) => {
          if (idx >= arr.length - 1) return null;
          const a = arr[idx];
          const b = arr[idx + 1];
          if (a.x !== b.x && a.y !== b.y) return null;
          return { a, b, orientation: a.x === b.x ? 'V' : 'H' } as Segment;
        })
        .filter(Boolean) as Segment[];
      for (let s = 0; s < segments.length; s += 1) {
        for (let o = 0; o < otherSegs.length; o += 1) {
          if (segmentsOverlap(segments[s], otherSegs[o])) return true;
        }
      }
    }
    return false;
  }

  function shareSamePort(a: Conn, b: Conn) {
    if (a.from === b.from && (a.fromIndex ?? 0) === (b.fromIndex ?? 0)) return true;
    if (a.to === b.to && (a.toIndex ?? 0) === (b.toIndex ?? 0)) return true;
    return false;
  }

  return {
    checkPortDirections,
    isValidStub,
    segmentHitsAnyBlock,
    segmentOverlapsOtherWire,
    shareSamePort,
  };
}
//=============================================================================
