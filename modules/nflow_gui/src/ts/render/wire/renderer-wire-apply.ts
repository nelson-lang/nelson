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
import { hasInvalidPoint } from '../create-renderer.partial.js';
import { rotatePoint, getPortSide } from '../geometry.js';
import type { Point as GPoint, Block as GeoBlock } from '../geometry.js';
import { buildSegments, buildPathWithHops } from './wire-paths.js';
import { desnapEndpoints } from './wire-endpoints.js';
import type { Segment } from './wire-paths.js';
import * as FieldNames from '../../FieldNames.js';
import { getUiPref } from '../../app/ui-prefs.js';
import { buildPhysicalNetTreePaths } from '../../router/physical-net.js';
//=============================================================================
const DEBUG_WIRE_CHECKS = false;
type Point = GPoint;
type Port = { type: 'in' | 'out' | 'phys'; index: number; x: number; y: number };
type Block = GeoBlock & {
  type?: string;
  ports: Port[];
  group?: Element;
};
//=============================================================================
type Conn = {
  from: string;
  to: string;
  fromIndex?: number;
  toIndex?: number;
  points?: Point[];
  path: Element;
  hitPath?: Element;
  /** Segments of the last rendered path (incl. stubs), reused by the fast
   *  drag pass so hop detection sees stationary wires' true geometry. */
  renderSegments?: Segment[];
  routeFailed?: boolean;
  debugRenderPoints?: Point[];
  debugRenderSegments?: number;
  debugSegmentPreview?: string[];
  debugPath?: string;
  debugIssues?: string[];
};
//=============================================================================
type State = {
  blocks: Map<string, Block>;
  connections: Conn[];
  fastRouting?: boolean;
  autoRoute?: boolean;
  debugOverlapCount?: number;
};
//=============================================================================
type RenderCtx = {
  state: State;
  isValidStub: (portPos: Point, next: Point | undefined, side: string) => boolean;
  simplifyOrthogonalPath: (p: Point[]) => Point[];
  dedupePoints: (p: Point[]) => Point[];
  removeColinearPoints?: (p: Point[]) => Point[];
  tryShorterOrthogonalPath: (conn: Conn, p: Point[]) => Point[];
  applyWireOffsets: (conn: Conn, p: Point[]) => Point[];
  buildFallbackPathFromPorts: (conn: Conn) => Point[];
  buildDragRenderPoints: (conn: Conn, p: Point[]) => Point[];
  renderSharedSourceJunctionDots: () => void;
  checkWireIssues?: (conn: Conn, log?: Element | null) => boolean;
  debugLog?: Element | null;
};
//=============================================================================
function buildStubPoint(portPos: Point, side: string): Point {
  if (side === 'left') return { x: portPos.x - GRID_SIZE, y: portPos.y };
  if (side === 'right') return { x: portPos.x + GRID_SIZE, y: portPos.y };
  if (side === 'top') return { x: portPos.x, y: portPos.y - GRID_SIZE };
  return { x: portPos.x, y: portPos.y + GRID_SIZE };
}
//=============================================================================
// True when no directed port shares the pin index (a sensor carries both a
// 'phys' pin and an 'out' at index 0, and a signal wire off its output is not a
// physical bus).
function isPurePhysicalPair(
  fromBlock: Block,
  toBlock: Block,
  fromIndex: number,
  toIndex: number
): boolean {
  const noneOfType = (block: Block, idx: number, dir: 'out' | 'in') =>
    !((block.ports || []) as Port[]).some((p) => p.type === dir && p.index === idx);
  return noneOfType(fromBlock, fromIndex, 'out') && noneOfType(toBlock, toIndex, 'in');
}
//=============================================================================
// Two collinear pins on the SAME side are drawn as a bracket: a short
// perpendicular stub out of each pin and one straight trunk between them. The
// autorouter leaves a per-pin bump there that looks like a hook at a shared
// physical node.
//
// The general router skips physical wires (its port lookup only knows directed
// in/out ports), so a facing pair on OPPOSITE horizontal edges (a joint's right
// flange to the next body's left frame) would fall back to a path draped over
// the block tops. Those get a straight line when the pins share a row, else a
// single mid-span jog. Returns null for anything else. Pure and exported for
// headless geometry tests.
export function computePhysicalBusPath(
  fromBlock: Block,
  toBlock: Block,
  fromIndex: number,
  toIndex: number
): Point[] | null {
  const fromPort = ((fromBlock.ports || []) as Port[]).find(
    (p) => p.type === 'phys' && p.index === fromIndex
  );
  const toPort = ((toBlock.ports || []) as Port[]).find(
    (p) => p.type === 'phys' && p.index === toIndex
  );
  if (!fromPort || !toPort) return null;
  const a = alignToGrid(
    rotatePoint({ x: fromBlock.x + fromPort.x, y: fromBlock.y + fromPort.y }, fromBlock)
  );
  const b = alignToGrid(rotatePoint({ x: toBlock.x + toPort.x, y: toBlock.y + toPort.y }, toBlock));
  const fromSide = getPortSide(fromBlock, a);
  const toSide = getPortSide(toBlock, b);
  // Facing pins across the gap between two side-by-side blocks.
  const facing =
    (fromSide === 'right' && toSide === 'left') || (fromSide === 'left' && toSide === 'right');
  if (facing && isPurePhysicalPair(fromBlock, toBlock, fromIndex, toIndex)) {
    const gap = fromSide === 'right' ? b.x - a.x : a.x - b.x;
    if (gap > GRID_SIZE) {
      if (a.y === b.y) return [a, b]; // aligned rows -> one straight segment
      const mx = alignToGrid({ x: (a.x + b.x) / 2, y: 0 }).x;
      return [a, { x: mx, y: a.y }, { x: mx, y: b.y }, b]; // single mid-span jog
    }
    return null; // back-to-back / overlapping: leave it to the fallback
  }
  if (fromSide !== toSide) return null;
  if (fromSide === 'left' || fromSide === 'right') {
    if (a.x !== b.x || a.y === b.y) return null; // not collinear vertical
    const tx = fromSide === 'left' ? a.x - GRID_SIZE : a.x + GRID_SIZE;
    return [a, { x: tx, y: a.y }, { x: tx, y: b.y }, b];
  }
  if (a.y !== b.y || a.x === b.x) return null; // not collinear horizontal
  const ty = fromSide === 'top' ? a.y - GRID_SIZE : a.y + GRID_SIZE;
  return [a, { x: a.x, y: ty }, { x: b.x, y: ty }, b];
}
//=============================================================================
export function makeWireApplyHandlers(ctx: RenderCtx) {
  function wireRenderOptions() {
    let roundedCorners = true;
    try {
      roundedCorners = getUiPref('nflow.wireRoundedCorners', '1') !== '0';
    } catch (_err) {
      /* prefs unavailable (tests); keep default */
    }
    return { roundedCorners };
  }

  function applyMarkerEnd(conn: Conn) {
    const toBlock = ctx.state.blocks.get(conn.to);
    const fromBlock = ctx.state.blocks.get(conn.from);
    // A wire between two undirected 'phys' pins has no direction, so no arrowhead
    // (it also stopped a few px short of the pin).
    const fromPhys = (fromBlock?.ports as Port[] | undefined)?.some(
      (p) => p.type === 'phys' && p.index === (conn.fromIndex ?? 0)
    );
    const toPhys = (toBlock?.ports as Port[] | undefined)?.some(
      (p) => p.type === 'phys' && p.index === (conn.toIndex ?? 0)
    );
    if (
      fromPhys ||
      toPhys ||
      toBlock?.type === FieldNames.kLabelSink ||
      fromBlock?.type === FieldNames.kLabelSource
    ) {
      conn.path.removeAttribute('marker-end');
    } else {
      conn.path.setAttribute('marker-end', 'url(#wire-arrow)');
    }
    const hasPins =
      Array.isArray(conn.points) && conn.points.some((p) => (p as { pin?: boolean }).pin);
    (conn.path as { classList?: DOMTokenList }).classList?.toggle('wire-pinned', hasPins);
  }
  function enforcePortStubs(
    state: State,
    conn: Conn,
    points?: Point[],
    options: { light?: boolean } = {}
  ) {
    if (!points || points.length < 2) return points || [];
    const fromBlock = state.blocks.get(conn.from);
    const toBlock = state.blocks.get(conn.to);
    if (!fromBlock || !toBlock) return points;
    const fromIndex = conn.fromIndex ?? 0;
    const toIndex = conn.toIndex ?? 0;
    const fromPorts = (fromBlock.ports || []) as Port[];
    const toPorts = (toBlock.ports || []) as Port[];
    // A physical pin is undirected, so a 'phys' port serves as either endpoint.
    const fromPort = fromPorts.find(
      (p) => (p.type === 'out' || p.type === 'phys') && p.index === fromIndex
    );
    const toPort = toPorts.find(
      (p) => (p.type === 'in' || p.type === 'phys') && p.index === toIndex
    );
    if (!fromPort || !toPort) return points;
    const fromPos = rotatePoint(
      { x: fromBlock.x + fromPort.x, y: fromBlock.y + fromPort.y },
      fromBlock
    );
    const toPos = rotatePoint({ x: toBlock.x + toPort.x, y: toBlock.y + toPort.y }, toBlock);
    const fromPortPos = alignToGrid(fromPos);
    const toPortPos = alignToGrid(toPos);
    const fromSide = getPortSide(fromBlock, fromPos);
    const toSide = getPortSide(toBlock, toPos);
    const result = points.slice();
    if (result[0].x !== fromPortPos.x || result[0].y !== fromPortPos.y) result.unshift(fromPortPos);
    const next = result[1];
    const startStub = buildStubPoint(fromPortPos, fromSide);
    if (!ctx.isValidStub(fromPortPos, next, fromSide)) {
      result.splice(1, 0, startStub);
      const afterStub = result[2];
      // Bridge only when afterStub is diagonal to the stub; the bridge shares the
      // stub's axis coordinate to keep segments orthogonal.
      if (afterStub && startStub.x !== afterStub.x && startStub.y !== afterStub.y) {
        if (fromSide === 'left' || fromSide === 'right') {
          result.splice(2, 0, { x: startStub.x, y: afterStub.y });
        } else {
          result.splice(2, 0, { x: afterStub.x, y: startStub.y });
        }
      }
    }
    const last = result[result.length - 1];
    if (last.x !== toPortPos.x || last.y !== toPortPos.y) result.push(toPortPos);
    const prev = result[result.length - 2];
    const endStub = buildStubPoint(toPortPos, toSide);
    if (!ctx.isValidStub(toPortPos, prev, toSide)) {
      if (prev && endStub.x !== prev.x && endStub.y !== prev.y) {
        if (toSide === 'left' || toSide === 'right') {
          result.splice(result.length - 1, 0, { x: endStub.x, y: prev.y });
        } else {
          result.splice(result.length - 1, 0, { x: prev.x, y: endStub.y });
        }
      }
      result.splice(result.length - 1, 0, endStub);
    }
    // The router's endpoints are grid-snapped; put the end runs back on the
    // exact ports so a wire meets its port and aligned ports get a straight line.
    const exact = desnapEndpoints(result, fromPos, fromSide, toPos, toSide);
    if (exact !== result) {
      result.length = 0;
      result.push(...exact);
    }
    if (options.light) {
      // Drag preview: only dedupe + strict colinear merge. The full shared
      // simplifier (diagonal-corner insertion, loop removal) can visibly
      // re-shape a path frame-to-frame, which reads as flicker mid-drag.
      const deduped = ctx.dedupePoints(result);
      return ctx.removeColinearPoints ? ctx.removeColinearPoints(deduped) : deduped;
    }
    return ctx.simplifyOrthogonalPath(ctx.dedupePoints(result));
  }

  function isValidPathString(d?: string | null) {
    if (!d) return true;
    if (d.includes('NaN') || d.includes('undefined')) return false;
    return true;
  }

  function applyWirePaths(
    paths: Map<unknown, Segment[]> | null,
    options: { preservePathShape?: boolean } = {}
  ) {
    void paths;
    const preservePathShape = Boolean(options?.preservePathShape);
    // Physical nets are drawn as ONE trunk with a perpendicular tap per pin, so
    // the branches of a shared node coincide. Neither router knows 'phys' ports;
    // the fallback path plus the separation pass left near-parallel offset
    // segments at every shared node. Skipped while dragging and when a pinned
    // shape must be preserved; a net that cannot be drawn cleanly is left as is.
    const netTreePaths =
      !preservePathShape && !ctx.state.fastRouting
        ? buildPhysicalNetTreePaths(ctx.state.connections, ctx.state.blocks)
        : null;

    // Collect segments for ALL connections before rendering any, so hop
    // detection sees final geometry and not stale cached data.
    const segmentMap = new Map<Conn, Segment[]>();
    ctx.state.connections.forEach((conn) => {
      let points = conn.points || [];
      if (!points || points.length < 2) {
        points = ctx.buildFallbackPathFromPorts(conn);
        conn.points = points;
        conn.routeFailed = true;
      } else {
        conn.routeFailed = false;
      }
      applyMarkerEnd(conn);
      // Physical bus bracket instead of the autorouter's per-pin bump (see
      // computePhysicalBusPath). Skipped during a drag and for pinned shapes.
      const fromBlockPB = ctx.state.blocks.get(conn.from);
      const toBlockPB = ctx.state.blocks.get(conn.to);
      const treePath = netTreePaths?.get(conn) ?? null;
      const busPath =
        treePath ??
        (!preservePathShape && !ctx.state.fastRouting && fromBlockPB && toBlockPB
          ? computePhysicalBusPath(fromBlockPB, toBlockPB, conn.fromIndex ?? 0, conn.toIndex ?? 0)
          : null);
      let finalPoints: Point[];
      if (busPath) {
        conn.points = busPath;
        finalPoints = busPath;
      } else {
        const routedPoints = ctx.state.fastRouting
          ? ctx.buildDragRenderPoints(conn, points)
          : points;
        const workingPoints = preservePathShape
          ? routedPoints
          : enforcePortStubs(ctx.state, conn, routedPoints);
        if (!preservePathShape && workingPoints !== routedPoints) conn.points = workingPoints;
        finalPoints = preservePathShape
          ? ctx.applyWireOffsets(conn, workingPoints)
          : ctx.state.autoRoute !== false
          ? ctx.simplifyOrthogonalPath(ctx.applyWireOffsets(conn, workingPoints))
          : ctx.applyWireOffsets(conn, workingPoints);
        if (!preservePathShape && ctx.state.autoRoute !== false)
          finalPoints = ctx.tryShorterOrthogonalPath(conn, finalPoints);
      }

      const segments = buildSegments(finalPoints, conn);
      conn.debugRenderPoints = finalPoints.slice();
      conn.debugRenderSegments = segments.length;
      conn.debugSegmentPreview = segments
        .slice(0, 6)
        .map(
          (seg: Segment) =>
            `${seg.orientation}:${seg.a.x},${seg.a.y}->${seg.b.x},${seg.b.y}${
              seg.isStub ? ':stub' : ''
            }`
        );
      segmentMap.set(conn, segments);
    });

    // Hop ownership is a pure function of the two crossing wires' keys (ownsHop
    // in wire-paths.ts): every wire sees ALL other segments, so the result does
    // not depend on iteration order.
    const allSegments: Segment[] = [];
    segmentMap.forEach((segs) => {
      segs.forEach((seg) => {
        if (!seg.isStub) allSegments.push(seg);
      });
    });
    let overlapCount = 0;
    ctx.state.connections.forEach((conn) => {
      const finalPoints = conn.debugRenderPoints;
      if (!finalPoints || !finalPoints.length || hasInvalidPoint(finalPoints)) {
        conn.path.setAttribute('d', '');
        if (conn.hitPath) conn.hitPath.setAttribute('d', '');
        return;
      }
      const segments = segmentMap.get(conn) || [];
      const d = buildPathWithHops(segments, allSegments, null, wireRenderOptions());
      if (!isValidPathString(d)) return;
      conn.renderSegments = segments;
      conn.debugPath = d;
      conn.path.setAttribute('d', d);
      if (conn.hitPath) conn.hitPath.setAttribute('d', d);
      if (DEBUG_WIRE_CHECKS) {
        const bad = ctx.checkWireIssues ? ctx.checkWireIssues(conn, ctx.debugLog || null) : false;
        conn.path.classList.toggle('wire-error', bad);
        if (conn.debugIssues && conn.debugIssues.includes('overlaps another wire'))
          overlapCount += 1;
      }
    });
    ctx.state.debugOverlapCount = overlapCount;
    ctx.renderSharedSourceJunctionDots();
  }

  function segmentsCross(a: Segment, b: Segment): boolean {
    const h = a.orientation === 'H' ? a : b.orientation === 'H' ? b : null;
    const v = a.orientation === 'V' ? a : b.orientation === 'V' ? b : null;
    if (!h || !v || h === v) return false;
    const y = h.y as number;
    const x = v.x as number;
    return (
      x > (h.minX as number) &&
      x < (h.maxX as number) &&
      y > (v.minY as number) &&
      y < (v.maxY as number)
    );
  }

  function applyWirePathsFast(connections: Map<unknown, Conn>) {
    if (!connections || connections.size === 0) return;

    const freshSegments = new Map<Conn, Segment[]>();
    connections.forEach((conn) => {
      let points = conn.points || [];
      if (!points || points.length < 2) {
        points = ctx.buildFallbackPathFromPorts(conn);
        conn.points = points;
        conn.routeFailed = true;
      } else {
        conn.routeFailed = false;
      }
      applyMarkerEnd(conn);
      const fastDrag = Boolean(ctx.state.fastRouting);
      const routedPoints = ctx.buildDragRenderPoints(conn, points);
      const stubbedPoints = enforcePortStubs(ctx.state, conn, routedPoints, { light: fastDrag });
      if (stubbedPoints !== routedPoints) conn.points = stubbedPoints;
      let finalPoints: Point[];
      if (fastDrag) {
        // Drag preview: render the repaired shape as-is. Re-simplification
        // and L/straight shortcuts would flap the shape while dragging.
        finalPoints = ctx.applyWireOffsets(conn, stubbedPoints);
      } else if (ctx.state.autoRoute !== false) {
        finalPoints = ctx.simplifyOrthogonalPath(ctx.applyWireOffsets(conn, stubbedPoints));
        finalPoints = ctx.tryShorterOrthogonalPath(conn, finalPoints);
      } else {
        finalPoints = ctx.applyWireOffsets(conn, stubbedPoints);
      }
      if (!finalPoints.length || hasInvalidPoint(finalPoints)) return;
      conn.debugRenderPoints = finalPoints.slice();
      freshSegments.set(conn, buildSegments(finalPoints, conn));
    });

    // Fresh segments for dirty wires, cached renderSegments for stationary ones:
    // hop ownership then matches the full pass pixel for pixel.
    const allSegments: Segment[] = [];
    const segmentsFor = (conn: Conn): Segment[] =>
      freshSegments.get(conn) ?? conn.renderSegments ?? [];
    ctx.state.connections.forEach((conn) => {
      segmentsFor(conn).forEach((seg) => {
        if (!seg.isStub) allSegments.push(seg);
      });
    });

    // Stationary wires crossing a dirty wire must re-render: they may own a hop
    // that moved or disappeared. Compare against the NEW and the OLD (cached)
    // dirty segments so vacated hops are cleaned up.
    const dirtyNonStub: Segment[] = [];
    freshSegments.forEach((segs, conn) => {
      segs.forEach((seg) => {
        if (!seg.isStub) dirtyNonStub.push(seg);
      });
      (conn.renderSegments || []).forEach((seg) => {
        if (!seg.isStub) dirtyNonStub.push(seg);
      });
    });
    const renderSet = new Set<Conn>(freshSegments.keys());
    ctx.state.connections.forEach((conn) => {
      if (renderSet.has(conn)) return;
      const cached = conn.renderSegments;
      if (!cached || !cached.length) return;
      const touches = cached.some(
        (seg) => !seg.isStub && dirtyNonStub.some((dseg) => segmentsCross(seg, dseg))
      );
      if (touches) renderSet.add(conn);
    });

    renderSet.forEach((conn) => {
      const segments = segmentsFor(conn);
      if (!segments.length) return;
      const d = buildPathWithHops(segments, allSegments, null, wireRenderOptions());
      if (!isValidPathString(d)) return;
      conn.renderSegments = segments;
      conn.path.setAttribute('d', d);
      if (conn.hitPath) conn.hitPath.setAttribute('d', d);
    });
    ctx.renderSharedSourceJunctionDots();
  }

  return { applyWirePaths, applyWirePathsFast, isValidPathString };
}
//=============================================================================
