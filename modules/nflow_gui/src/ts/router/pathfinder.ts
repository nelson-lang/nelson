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
import { GRID_SIZE } from '../utils/geometry.js';
import { MinHeap } from '../data-structures/min-heap.js';
import {
  buildBlockOccupancyMap,
  buildObstacleNearMap,
  buildWireNearMap,
  buildOccupancy,
  occupancyAddWire,
  occupancyEdgeBlocked,
  occupancyPointInfo,
  pointKeyInt,
  cellIndex,
  isBlocked,
  edgeKeyFor,
  buildObstacleOwners,
  wireNearMapMarkPoints,
} from './grid.js';
import {
  proximityPenalty,
  buildCost,
  nearPortTurnPenalty,
  sharedJunctionPenalty,
  evaluatePathCost,
} from './cost.js';
import { RoutingConfig } from './types.js';
import type { RoutingSettings } from './types.js';
import {
  buildRouter2Input,
  applyRouter2Result,
  analyzeResultWireQuality,
  blockIdFromNodeId,
  densifyGridPath,
} from './post-process.js';
//=============================================================================
type Point = { x: number; y: number };
type RouterNode = { id: string | number; x: number; y: number; dir?: string | number };
type RouterConnection = {
  from: string | number;
  to: string | number;
  fromIndex?: number;
  toIndex?: number;
  key?: string;
  __ref?: any;
  points?: Point[];
};
//=============================================================================
type Obstacle = { x0: number; y0: number; x1: number; y1: number; owner?: string };
type PrevWiresMap = Map<string, Point[]> | Record<string, Point[]>;
type PrevSolution = { wires: PrevWiresMap } | null;
type Bounds = { minX: number; minY: number; maxX: number; maxY: number };
/** Returns { wires, cost, nearBreakdown, failures, settings, durationMs }. */
export function computeWireRoutes({
  nodes = [],
  connections = [],
  obstacles = [],
  prevSolution = null,
  settings = {},
}: {
  nodes?: RouterNode[];
  connections?: RouterConnection[];
  obstacles?: Obstacle[];
  prevSolution?: PrevSolution | null;
  settings?: RoutingSettings | Record<string, any>;
} = {}) {
  const startTime = Date.now();
  const opts = RoutingConfig(settings);

  const nodeMap = new Map<string | number, RouterNode>(
    nodes.map((node: RouterNode) => [node.id, node])
  );
  const connectionKeys = connections.map((conn: RouterConnection, idx: number) =>
    connectionKey(conn, idx)
  );
  const connectionMeta = new Map<
    string,
    { from?: any; to?: any; fromBlock?: string; toBlock?: string }
  >();
  connections.forEach((conn: RouterConnection, idx: number) => {
    const key = connectionKeys[idx];
    connectionMeta.set(key, {
      from: conn.from,
      to: conn.to,
      fromBlock: blockIdFromNodeId(conn.from),
      toBlock: blockIdFromNodeId(conn.to),
    });
  });

  const prevWires = new Map<string, any>();
  if (prevSolution && prevSolution.wires) {
    if (prevSolution.wires instanceof Map) {
      prevSolution.wires.forEach((wire: any, key: any) => prevWires.set(key, wire));
    } else {
      Object.entries(prevSolution.wires).forEach(([key, wire]) => prevWires.set(key, wire));
    }
  }

  // Previous-route points may arrive either as raw point arrays (from
  // buildRouter2Input) or as `{ points }` wire objects; normalize.
  const prevPointsOf = (key: string, minLen = 2): Point[] | null => {
    const wire = prevWires.get(key);
    if (!wire) return null;
    if (Array.isArray(wire)) return wire.length >= minLen ? (wire as Point[]) : null;
    if (Array.isArray(wire.points)) return wire.points.length >= minLen ? wire.points : null;
    return null;
  };

  const dirtyConnections = new Set();
  connections.forEach((conn: any, idx: number) => {
    const key = connectionKeys[idx];
    const dirty =
      !opts.incremental ||
      opts.fullOptimize ||
      opts.changedConnections.has(key) ||
      opts.changedNodes.has(conn.from) ||
      opts.changedNodes.has(conn.to) ||
      !prevWires.has(key);
    if (dirty) dirtyConnections.add(key);
  });

  const bounds = computeBounds(nodes, obstacles, opts.searchPadding ?? 20);
  const obstacleGrid = buildBlockOccupancyMap(obstacles);
  const obstacleNearMap = buildObstacleNearMap(obstacleGrid, bounds);
  const obstacleOwners = buildObstacleOwners(obstacles);
  const routedWires = new Map();

  const staticWires: { points: Point[]; meta: any }[] = [];
  connections.forEach((_: any, idx: number) => {
    const key = connectionKeys[idx];
    if (!dirtyConnections.has(key)) {
      const pts = prevPointsOf(key, 1);
      if (pts) {
        // Preserve the original wire object identity when it already has the
        // `{ points }` shape (raw arrays are wrapped for downstream use).
        const wire = prevWires.get(key);
        routedWires.set(key, Array.isArray(wire) ? { points: pts, key, cost: { total: 0 } } : wire);
        staticWires.push({ points: pts, meta: connectionMeta.get(key) });
      }
    }
  });

  let occupancy = buildOccupancy(staticWires);
  // Shared A* scratch buffers, reused by every planSingleConnection call of
  // this routing pass (main loop, pinned legs, refine).
  const bufferCache: BufferCache = { buf: null, bonus: null };

  // Bounded-region search: try the endpoints' window first (state buffers
  // scale with the window, not the diagram); fall back to the full bounds
  // when no path fits the window (rare pathological detours).
  const planWithBoundedSearch = (args: any): any => {
    const windowBounds = computeSearchBounds(args.from, args.to, bounds);
    const bounded = planSingleConnection({ ...args, searchBounds: windowBounds });
    if (bounded.points.length) return bounded;
    return planSingleConnection(args);
  };

  // Shared wire-near map, updated incrementally as wires land: rebuilding it per
  // connection cost O(diagram area) each time. Codes only strengthen on adds;
  // rip-up rebuilds.
  const mapWidth = bounds.maxX - bounds.minX + 1;
  const mapHeight = bounds.maxY - bounds.minY + 1;
  let sharedWireNearMap: Uint8Array =
    (buildWireNearMap(occupancy, bounds) as Uint8Array | null) ??
    new Uint8Array(mapWidth * mapHeight);
  const rebuildWireNearMap = () => {
    sharedWireNearMap =
      (buildWireNearMap(occupancy, bounds) as Uint8Array | null) ??
      new Uint8Array(mapWidth * mapHeight);
  };
  const costs: {
    total: number;
    length: number;
    turns: number;
    hops: number;
    near: number;
    failed: number;
  } = {
    total: 0,
    length: 0,
    turns: 0,
    hops: 0,
    near: 0,
    failed: 0,
  };
  const nearBreakdown: { wire1: number; wire2: number; obs1: number; obs2: number } = {
    wire1: 0,
    wire2: 0,
    obs1: 0,
    obs2: 0,
  };
  const failures: Array<{
    key: string;
    from?: any;
    to?: any;
    reason?: string;
    start?: Point & { dir?: any };
    end?: Point & { dir?: any };
  }> = [];

  // Sort connections: feedback wires (going backwards, from.x > to.x) first
  // since they need clean routing space; then by Manhattan distance (longest first).
  const routingOrder = connections.map((conn: RouterConnection, idx: number) => {
    const from = nodeMap.get(conn.from) as RouterNode | undefined;
    const to = nodeMap.get(conn.to) as RouterNode | undefined;
    const dist = from && to ? Math.abs(from.x - to.x) + Math.abs(from.y - to.y) : 0;
    const backward = from && to && from.x > to.x ? (from.x - to.x) * 2 : 0;
    return { idx, score: dist + backward };
  });
  // Deterministic order: score descending, then connection key ascending. The key
  // is content-based (not positional), so equal-score connections route in the
  // same order whatever the input array order.
  routingOrder.sort((a: any, b: any) => {
    if (a.score !== b.score) return b.score - a.score;
    const ka = connectionKeys[a.idx];
    const kb = connectionKeys[b.idx];
    return ka < kb ? -1 : ka > kb ? 1 : a.idx - b.idx;
  });

  routingOrder.forEach(({ idx }: { idx: number }) => {
    const conn = connections[idx] as RouterConnection;
    const key = connectionKeys[idx];
    if (!dirtyConnections.has(key)) return;
    if (!opts.deterministic && Date.now() - startTime > (opts.maxTimeMs ?? 200)) {
      costs.failed += 1;
      return;
    }
    const from = nodeMap.get(conn.from) as any;
    const to = nodeMap.get(conn.to) as any;
    if (!from || !to) {
      costs.failed += 1;
      return;
    }
    const wireNearMap = sharedWireNearMap;
    const meta = {
      from: conn.from,
      to: conn.to,
      key,
      fromBlock: blockIdFromNodeId(conn.from),
      toBlock: blockIdFromNodeId(conn.to),
    };

    // Pinned wire: keep the user-anchored chain verbatim and route only the free
    // sub-paths port -> firstPin and lastPin -> port. Falls back to keeping the
    // whole previous shape when a free end cannot be routed.
    const pinnedRaw = prevPointsOf(key, 2);
    if (pinnedRaw && pinnedRaw.some((p: any) => (p as any).pin)) {
      const routeConstrainedPinned = (): any | null => {
        const pts = densifyGridPath(pinnedRaw) as any[];
        let firstPin = -1;
        let lastPin = -1;
        pts.forEach((p, i) => {
          if (p.pin) {
            if (firstPin < 0) firstPin = i;
            lastPin = i;
          }
        });
        // Pins at the very ends leave no free sub-path to route.
        if (firstPin <= 0 || lastPin >= pts.length - 1) return null;
        const dirName = (dx: number, dy: number) =>
          dx === 1 ? 'right' : dx === -1 ? 'left' : dy === 1 ? 'down' : 'up';
        const OPP: Record<string, string> = {
          right: 'left',
          left: 'right',
          down: 'up',
          up: 'down',
        };
        const inDx = Math.sign(pts[firstPin].x - pts[firstPin - 1].x);
        const inDy = Math.sign(pts[firstPin].y - pts[firstPin - 1].y);
        const outDx = Math.sign(pts[lastPin + 1].x - pts[lastPin].x);
        const outDy = Math.sign(pts[lastPin + 1].y - pts[lastPin].y);
        if ((inDx === 0 && inDy === 0) || (outDx === 0 && outDy === 0)) return null;

        const routeLeg = (legFrom: any, legTo: any) => {
          const legNearMap = sharedWireNearMap;
          return planWithBoundedSearch({
            from: legFrom,
            to: legTo,
            obstacles,
            obstacleGrid,
            obstacleNearMap,
            obstacleOwners,
            wireNearMap: legNearMap,
            occupancy,
            bounds: bounds as Bounds,
            opts,
            meta,
            prevPoints: null,
            bufferCache,
          }) as any;
        };

        // Head enters the first anchor colinear with the original approach;
        // tail leaves the last anchor colinear with the original departure.
        const headTo = {
          id: `${key}:pinhead`,
          x: pts[firstPin].x,
          y: pts[firstPin].y,
          dir: OPP[dirName(inDx, inDy)],
        };
        const head = routeLeg(from, headTo);
        if (!head.points.length) return null;
        const tailFrom = {
          id: `${key}:pintail`,
          x: pts[lastPin].x,
          y: pts[lastPin].y,
          dir: dirName(outDx, outDy),
        };
        const tail = routeLeg(tailFrom, to);
        if (!tail.points.length) return null;

        const chain = pts.slice(firstPin, lastPin + 1);
        // Anchors keep their pin flags: the chain's copies win over the
        // unpinned leg endpoints.
        const stitched = [...head.points.slice(0, -1), ...chain, ...tail.points.slice(1)];
        const nb = (w: any) => w.cost.nearBreakdown || {};
        return {
          points: stitched,
          cost: {
            total: head.cost.total + tail.cost.total,
            length: head.cost.length + tail.cost.length,
            turns: head.cost.turns + tail.cost.turns,
            hops: head.cost.hops + tail.cost.hops,
            near: head.cost.near + tail.cost.near,
            nearBreakdown: {
              wire1: (nb(head).wire1 ?? 0) + (nb(tail).wire1 ?? 0),
              wire2: (nb(head).wire2 ?? 0) + (nb(tail).wire2 ?? 0),
              obs1: (nb(head).obs1 ?? 0) + (nb(tail).obs1 ?? 0),
              obs2: (nb(head).obs2 ?? 0) + (nb(tail).obs2 ?? 0),
            },
          },
        };
      };

      const constrained = routeConstrainedPinned();
      const finalWire: any = constrained ?? {
        points: densifyGridPath(pinnedRaw).slice(),
        cost: {
          total: 0,
          length: 0,
          turns: 0,
          hops: 0,
          near: 0,
          nearBreakdown: { wire1: 0, wire2: 0, obs1: 0, obs2: 0 },
        },
        keptPrev: true,
      };
      finalWire.key = key;
      routedWires.set(key, finalWire);
      occupancyAddWire(occupancy, finalWire.points, meta);
      wireNearMapMarkPoints(sharedWireNearMap, bounds, finalWire.points);
      costs.total += finalWire.cost.total;
      costs.length += finalWire.cost.length;
      costs.turns += finalWire.cost.turns;
      costs.hops += finalWire.cost.hops;
      costs.near += finalWire.cost.near;
      nearBreakdown.wire1 += finalWire.cost.nearBreakdown?.wire1 ?? 0;
      nearBreakdown.wire2 += finalWire.cost.nearBreakdown?.wire2 ?? 0;
      nearBreakdown.obs1 += finalWire.cost.nearBreakdown?.obs1 ?? 0;
      nearBreakdown.obs2 += finalWire.cost.nearBreakdown?.obs2 ?? 0;
      return;
    }

    // Route hysteresis: the connection's previous route (densified, endpoints
    // unchanged) biases the search toward its own shape and competes against
    // the fresh result below.
    let hysteresisPrev: Point[] | null = null;
    if ((opts.reuseBonus ?? 0) > 0 || (opts.acceptThreshold ?? 0) > 0) {
      const rawPrev = prevPointsOf(key);
      if (rawPrev) {
        const densified = densifyGridPath(rawPrev);
        const first = densified[0];
        const last = densified[densified.length - 1];
        if (
          first &&
          last &&
          first.x === from.x &&
          first.y === from.y &&
          last.x === to.x &&
          last.y === to.y
        ) {
          hysteresisPrev = densified;
        }
      }
    }

    let result = planWithBoundedSearch({
      from: from as RouterNode,
      to: to as RouterNode,
      obstacles,
      obstacleGrid,
      obstacleNearMap,
      obstacleOwners,
      wireNearMap,
      occupancy,
      bounds: bounds as Bounds,
      opts,
      meta,
      prevPoints: hysteresisPrev,
      bufferCache,
    }) as any;
    if (result.points.length === 0) {
      costs.failed += 1;
      failures.push({
        key,
        from: from.id,
        to: to.id,
        reason: result.reason || 'no_path',
        start: { x: from.x, y: from.y, dir: from.dir },
        end: { x: to.x, y: to.y, dir: to.dir },
      });
      return;
    }

    // Accept-only-if-better: keep the previous shape unless the fresh route
    // improves its true cost by at least acceptThreshold (and the previous
    // route is still feasible under current occupancy/obstacles).
    if (hysteresisPrev && (opts.acceptThreshold ?? 0) > 0) {
      const prevEval = evaluatePathCost(hysteresisPrev, {
        obstacles,
        obstacleGrid,
        obstacleNearMap,
        obstacleOwners,
        wireNearMap,
        occupancy,
        bounds: bounds as any,
        opts: opts as any,
        meta,
      });
      if (
        prevEval.feasible &&
        result.cost.total > (1 - (opts.acceptThreshold as number)) * prevEval.cost.total
      ) {
        result = {
          points: hysteresisPrev.slice(),
          cost: prevEval.cost,
          keptPrev: true,
        };
      }
    }

    result.key = key;
    routedWires.set(key, result);
    occupancyAddWire(occupancy, result.points, meta);
    wireNearMapMarkPoints(sharedWireNearMap, bounds, result.points);
    costs.total += result.cost.total;
    costs.length += result.cost.length;
    costs.turns += result.cost.turns;
    costs.hops += result.cost.hops;
    costs.near += result.cost.near;
    nearBreakdown.wire1 += result.cost.nearBreakdown?.wire1 ?? 0;
    nearBreakdown.wire2 += result.cost.nearBreakdown?.wire2 ?? 0;
    nearBreakdown.obs1 += result.cost.nearBreakdown?.obs1 ?? 0;
    nearBreakdown.obs2 += result.cost.nearBreakdown?.obs2 ?? 0;
  });

  // Rip-up-and-reroute refinement: the worst wires (hops / wire proximity) are
  // removed from occupancy and rerouted with full knowledge of everyone else; the
  // batch is rolled back unless it strictly improves the lexicographic aggregate
  // (hops, near, turns, length).
  const refineIterations = opts.refineIterations ?? (opts.fullOptimize ? 2 : 0);

  const metaFor = (key: string) => {
    const m = connectionMeta.get(key) || {};
    return { ...m, key };
  };

  const rebuildOccupancy = (excluded: Set<string> | null) => {
    const occ = buildOccupancy(staticWires);
    const keys = Array.from(routedWires.keys()).sort();
    for (const key of keys) {
      if (excluded && excluded.has(key)) continue;
      if (!dirtyConnections.has(key)) continue; // statics already included
      const wire = routedWires.get(key);
      if (wire && Array.isArray(wire.points) && wire.points.length) {
        occupancyAddWire(occ, wire.points, metaFor(key));
      }
    }
    return occ;
  };

  const aggregateOf = (keys: string[]) => {
    const agg = { hops: 0, near: 0, turns: 0, length: 0 };
    for (const key of keys) {
      const c = routedWires.get(key)?.cost;
      if (!c) continue;
      agg.hops += c.hops || 0;
      agg.near += c.near || 0;
      agg.turns += c.turns || 0;
      agg.length += c.length || 0;
    }
    return agg;
  };
  const lexBetter = (a: any, b: any) =>
    a.hops !== b.hops
      ? a.hops < b.hops
      : a.near !== b.near
      ? a.near < b.near
      : a.turns !== b.turns
      ? a.turns < b.turns
      : a.length < b.length;

  for (let iter = 0; iter < refineIterations; iter += 1) {
    if (!opts.deterministic && Date.now() - startTime > 0.7 * (opts.maxTimeMs ?? 200)) break;

    // Score badness of routed dirty wires A POSTERIORI: re-cost each route under the
    // FINAL occupancy (evaluatePathCost ignores the wire's own cells), so early
    // wires that later wires crowded (invisible to their at-routing-time cost)
    // become candidates too.
    const refineNearMap = sharedWireNearMap;
    const candidates: Array<{ key: string; badness: number; evalCost: any }> = [];
    for (const key of routedWires.keys()) {
      if (!dirtyConnections.has(key)) continue;
      const wire = routedWires.get(key);
      // Pinned wires are never ripped up; their shape is user-anchored.
      if (wire?.points?.some((p: any) => p.pin)) continue;
      if (!wire?.points?.length) continue;
      const ev = evaluatePathCost(wire.points, {
        obstacles,
        obstacleGrid,
        obstacleNearMap,
        obstacleOwners,
        wireNearMap: refineNearMap,
        occupancy,
        bounds: bounds as any,
        opts: opts as any,
        meta: metaFor(key),
      });
      const c = ev.feasible ? ev.cost : wire.cost;
      if (!c) continue;
      const badness =
        (c.hops || 0) * (opts.hopCost as number) +
        (c.nearBreakdown?.wire1 || 0) * ((opts.nearWirePenalty1 as number) || 0);
      if (badness > 0) candidates.push({ key, badness, evalCost: c });
    }
    if (!candidates.length) break;
    candidates.sort((a, b) => b.badness - a.badness || (a.key < b.key ? -1 : 1));
    // Rip ONE wire per iteration: ripping an interacting pair together just
    // reproduces the original mutual environment (deterministic search) and
    // rolls back; waves of single rips let each wire adapt to the others.
    const ripped = candidates.slice(0, 1).map((c) => c.key);
    const rippedSet = new Set(ripped);

    // "Before" uses the A-POSTERIORI cost (the at-routing-time cost may miss the
    // proximity later wires created) so the comparison with the rerouted cost is
    // like for like.
    const before = {
      hops: candidates[0].evalCost.hops || 0,
      near: candidates[0].evalCost.near || 0,
      turns: candidates[0].evalCost.turns || 0,
      length: candidates[0].evalCost.length || 0,
    };
    const saved = new Map(ripped.map((key) => [key, routedWires.get(key)]));

    occupancy = rebuildOccupancy(rippedSet);
    rebuildWireNearMap();
    let failedRip = false;
    for (const { idx } of routingOrder) {
      const key = connectionKeys[idx];
      if (!rippedSet.has(key)) continue;
      const conn = connections[idx] as RouterConnection;
      const from = nodeMap.get(conn.from) as any;
      const to = nodeMap.get(conn.to) as any;
      if (!from || !to) {
        failedRip = true;
        break;
      }
      const wireNearMap = sharedWireNearMap;
      const rr = planWithBoundedSearch({
        from,
        to,
        obstacles,
        obstacleGrid,
        obstacleNearMap,
        obstacleOwners,
        wireNearMap,
        occupancy,
        bounds: bounds as Bounds,
        opts,
        meta: metaFor(key),
        prevPoints: null, // hysteresis off: these wires must be free to move
        bufferCache,
      }) as any;
      if (!rr.points.length) {
        failedRip = true;
        break;
      }
      rr.key = key;
      routedWires.set(key, rr);
      occupancyAddWire(occupancy, rr.points, metaFor(key));
      wireNearMapMarkPoints(sharedWireNearMap, bounds, rr.points);
    }

    const after = failedRip ? null : aggregateOf(ripped);
    if (failedRip || !after || !lexBetter(after, before)) {
      for (const [key, wire] of saved) routedWires.set(key, wire);
      occupancy = rebuildOccupancy(null);
      rebuildWireNearMap();
      break;
    }
  }

  if (refineIterations > 0) {
    costs.total = 0;
    costs.length = 0;
    costs.turns = 0;
    costs.hops = 0;
    costs.near = 0;
    nearBreakdown.wire1 = 0;
    nearBreakdown.wire2 = 0;
    nearBreakdown.obs1 = 0;
    nearBreakdown.obs2 = 0;
    for (const key of routedWires.keys()) {
      if (!dirtyConnections.has(key)) continue;
      const c = routedWires.get(key)?.cost;
      if (!c) continue;
      costs.total += c.total || 0;
      costs.length += c.length || 0;
      costs.turns += c.turns || 0;
      costs.hops += c.hops || 0;
      costs.near += c.near || 0;
      nearBreakdown.wire1 += c.nearBreakdown?.wire1 ?? 0;
      nearBreakdown.wire2 += c.nearBreakdown?.wire2 ?? 0;
      nearBreakdown.obs1 += c.nearBreakdown?.obs1 ?? 0;
      nearBreakdown.obs2 += c.nearBreakdown?.obs2 ?? 0;
    }
  }

  return {
    wires: routedWires,
    cost: costs,
    nearBreakdown,
    failures,
    settings: opts,
    durationMs: Date.now() - startTime,
  };
}
//=============================================================================
function connectionKey(
  conn:
    | RouterConnection
    | { key?: any; id?: any; from?: any; fromIndex?: any; to?: any; toIndex?: any },
  idx: number
) {
  if (conn.key) return String((conn as any).key);
  if ((conn as any).id) return String((conn as any).id);
  const fromIndex = (conn as any).fromIndex ?? 0;
  const toIndex = (conn as any).toIndex ?? 0;
  return `${(conn as any).from}:${fromIndex}->${(conn as any).to}:${toIndex}:${idx}`;
}
/**
 * Restricted A* window for one connection: the endpoints' bounding box plus a
 * fixed margin, clamped to the global bounds. State buffers scale with the window
 * area, not the diagram. A failed bounded search falls back to the full bounds
 * (see planWithBoundedSearch).
 */
export const SEARCH_WINDOW_MARGIN_CELLS = 40;
function computeSearchBounds(
  from: { x: number; y: number },
  to: { x: number; y: number },
  bounds: Bounds,
  margin = SEARCH_WINDOW_MARGIN_CELLS
): Bounds {
  return {
    minX: Math.max(bounds.minX, Math.min(from.x, to.x) - margin),
    minY: Math.max(bounds.minY, Math.min(from.y, to.y) - margin),
    maxX: Math.min(bounds.maxX, Math.max(from.x, to.x) + margin),
    maxY: Math.min(bounds.maxY, Math.max(from.y, to.y) + margin),
  };
}
//=============================================================================
function computeBounds(nodes: RouterNode[], obstacles: Obstacle[], padding: number): Bounds {
  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;
  nodes.forEach((node: any) => {
    minX = Math.min(minX, node.x);
    minY = Math.min(minY, node.y);
    maxX = Math.max(maxX, node.x);
    maxY = Math.max(maxY, node.y);
  });
  obstacles.forEach((obs: Obstacle) => {
    minX = Math.min(minX, obs.x0);
    minY = Math.min(minY, obs.y0);
    maxX = Math.max(maxX, obs.x1);
    maxY = Math.max(maxY, obs.y1);
  });
  if (!isFinite(minX)) {
    minX = 0;
    minY = 0;
    maxX = 0;
    maxY = 0;
  }
  return {
    minX: minX - padding,
    minY: minY - padding,
    maxX: maxX + padding,
    maxY: maxY + padding,
  };
}
//=============================================================================
function isGoalState(s: any, goal: any, goalDirIdx: number) {
  return s.x === goal.x && s.y === goal.y && s.dir === goalDirIdx;
}
//=============================================================================
function makeStateBuffers(stateCount: number) {
  const gScore = new Float64Array(stateCount);
  gScore.fill(Number.POSITIVE_INFINITY);
  const cameFrom = new Int32Array(stateCount);
  cameFrom.fill(-1);
  const lenArr = new Int32Array(stateCount);
  const turnsArr = new Int16Array(stateCount);
  const hopsArr = new Int16Array(stateCount);
  const nearArr = new Int32Array(stateCount);
  const wire1Arr = new Int32Array(stateCount);
  const wire2Arr = new Int32Array(stateCount);
  const obs1Arr = new Int32Array(stateCount);
  const obs2Arr = new Int32Array(stateCount);
  return {
    gScore,
    cameFrom,
    lenArr,
    turnsArr,
    hopsArr,
    nearArr,
    wire1Arr,
    wire2Arr,
    obs1Arr,
    obs2Arr,
  };
}
//=============================================================================
type BufferCache = {
  buf: (ReturnType<typeof makeStateBuffers> & { capacity: number }) | null;
  bonus: Float64Array | null;
};
/**
 * Reuses the A* state buffers across the connections of one computeWireRoutes
 * call: allocating ten stateCount-sized arrays per connection dominated
 * large-diagram routing time, and a fill() over the used range is far cheaper.
 */
function acquireStateBuffers(cache: BufferCache | null, stateCount: number) {
  if (!cache) return makeStateBuffers(stateCount);
  if (!cache.buf || cache.buf.capacity < stateCount) {
    cache.buf = { ...makeStateBuffers(stateCount), capacity: stateCount };
    return cache.buf;
  }
  const b = cache.buf;
  b.gScore.fill(Number.POSITIVE_INFINITY, 0, stateCount);
  b.cameFrom.fill(-1, 0, stateCount);
  b.lenArr.fill(0, 0, stateCount);
  b.turnsArr.fill(0, 0, stateCount);
  b.hopsArr.fill(0, 0, stateCount);
  b.nearArr.fill(0, 0, stateCount);
  b.wire1Arr.fill(0, 0, stateCount);
  b.wire2Arr.fill(0, 0, stateCount);
  b.obs1Arr.fill(0, 0, stateCount);
  b.obs2Arr.fill(0, 0, stateCount);
  return b;
}
//=============================================================================
function acquireBonusArr(cache: BufferCache | null, stateCount: number): Float64Array {
  if (!cache) return new Float64Array(stateCount);
  if (!cache.bonus || cache.bonus.length < stateCount) {
    cache.bonus = new Float64Array(stateCount);
    return cache.bonus;
  }
  cache.bonus.fill(0, 0, stateCount);
  return cache.bonus;
}
//=============================================================================
function planSingleConnection({
  from,
  to,
  obstacles,
  obstacleGrid,
  obstacleNearMap,
  obstacleOwners,
  wireNearMap,
  occupancy,
  bounds,
  opts,
  meta,
  prevPoints = null,
  bufferCache = null,
  searchBounds = null,
}: any) {
  const startDir = dirToVector(from.dir);
  const endDir = dirToVector(to.dir);
  if (!startDir || !endDir) return makeEmptyResult('bad_direction');
  const endIncoming = { x: -endDir.x, y: -endDir.y };

  const start = { x: from.x, y: from.y };
  const end = { x: to.x, y: to.y };
  const startStep = { x: start.x + startDir.x, y: start.y + startDir.y };
  const endStep = { x: end.x - endIncoming.x, y: end.y - endIncoming.y };
  const allowedSet = new Set([
    pointKeyInt(start.x, start.y),
    pointKeyInt(end.x, end.y),
    pointKeyInt(startStep.x, startStep.y),
    pointKeyInt(endStep.x, endStep.y),
  ]);

  if (isBlocked(startStep, obstacles, allowedSet, obstacleGrid)) {
    return makeEmptyResult('blocked_start_step');
  }
  if (isBlocked(endStep, obstacles, allowedSet, obstacleGrid)) {
    return makeEmptyResult('blocked_end_step');
  }

  // State space runs on the (possibly restricted) search window; near-maps
  // stay indexed on the GLOBAL bounds they were built with.
  const sB: Bounds = (searchBounds as Bounds) ?? bounds;
  const gridW = sB.maxX - sB.minX + 1;
  const gridH = sB.maxY - sB.minY + 1;
  const cellCount = gridW * gridH;
  const stateCount = cellCount * 8;
  const {
    gScore,
    cameFrom,
    lenArr,
    turnsArr,
    hopsArr,
    nearArr,
    wire1Arr,
    wire2Arr,
    obs1Arr,
    obs2Arr,
  } = acquireStateBuffers(bufferCache, stateCount);

  const startDirKey = dirKey(startDir);
  const endIncomingKey = dirKey(endIncoming);
  const startDirIdx = dirIndex(startDirKey);
  const endDirIdx = dirIndex(endIncomingKey);
  const endIncomingIdx = endDirIdx;
  const startCell = cellIndex(start.x, start.y, sB.minX, sB.minY, gridW);
  const startIndex = stateIndex(startCell, startDirIdx, 0);

  // Route hysteresis: edges of the previous route earn a small discount so
  // equal-quality alternatives resolve toward the old shape. The heuristic is
  // scaled down to stay admissible (each step may cost as little as
  // lengthCost - reuseBonus).
  const reuseBonus = prevPoints && prevPoints.length > 1 ? Number(opts.reuseBonus ?? 0) : 0;
  let prevEdgeSet: Set<string> | null = null;
  if (reuseBonus > 0) {
    prevEdgeSet = new Set<string>();
    for (let i = 1; i < prevPoints.length; i += 1) {
      prevEdgeSet.add(edgeKeyFor(prevPoints[i - 1], prevPoints[i]));
    }
  }
  const bonusArr = reuseBonus > 0 ? acquireBonusArr(bufferCache, stateCount) : null;
  const hFactor = reuseBonus > 0 ? Math.max(0, 1 - reuseBonus) : 1;

  gScore[startIndex] = 0;
  const open = new MinHeap();
  const enqueueFrontier = (i: number, f: number) => open.push({ idx: i, f });
  const dequeueFrontier = () => open.pop();
  enqueueFrontier(startIndex, heuristic(start, end) * hFactor);
  const searchStart = Date.now();
  // Deterministic mode replaces the wall-clock cap with an expansion cap so
  // results are machine-independent (parity/golden tests).
  const deterministic = !!opts.deterministic;
  const maxExpansions = opts.maxExpansions ?? 400000;
  let expansions = 0;

  for (;;) {
    if (open.size() === 0) break;
    if (deterministic) {
      if (++expansions > maxExpansions) break;
    } else if (Date.now() - searchStart > opts.maxTimeMs) {
      break;
    }
    const entry = dequeueFrontier();
    if (!entry) break;
    const state = decodeState(entry.idx, sB.minX, sB.minY, gridW);
    if (isGoalState(state, end, endDirIdx)) {
      return buildResultFromArrays(
        entry.idx,
        cameFrom,
        {
          totalArr: gScore,
          lenArr,
          turnsArr,
          hopsArr,
          nearArr,
          wire1Arr,
          wire2Arr,
          obs1Arr,
          obs2Arr,
        },
        sB.minX,
        sB.minY,
        gridW,
        opts
      );
    }

    const curIdx = entry.idx;
    const currentStats = {
      length: lenArr[curIdx],
      turns: turnsArr[curIdx],
      hops: hopsArr[curIdx],
      near: nearArr[curIdx],
      nearBreakdown: {
        wire1: wire1Arr[curIdx],
        wire2: wire2Arr[curIdx],
        obs1: obs1Arr[curIdx],
        obs2: obs2Arr[curIdx],
      },
    };

    const neighbors = gatherNeighbors(
      state,
      start,
      end,
      startStep,
      endStep,
      endIncomingIdx,
      occupancy,
      obstacles,
      obstacleGrid,
      obstacleNearMap,
      obstacleOwners,
      wireNearMap,
      bounds,
      sB,
      opts,
      currentStats,
      meta,
      allowedSet
    );

    const relax = (next: any) => {
      const cell = cellIndex(next.state.x, next.state.y, sB.minX, sB.minY, gridW);
      const nextIdx = stateIndex(cell, next.state.dir, next.state.hopLock);
      // gScore holds the DISCOUNTED total (true total minus accumulated
      // reuse bonuses); the component stats stay clean so the final cost is
      // recomputed from them in buildResultFromArrays.
      const edgeBonus =
        prevEdgeSet && next.edgeKey && prevEdgeSet.has(next.edgeKey) ? reuseBonus : 0;
      const bonus = (bonusArr ? bonusArr[curIdx] : 0) + edgeBonus;
      const tentative = next.cost.total - bonus;
      if (tentative < gScore[nextIdx]) {
        cameFrom[nextIdx] = curIdx;
        gScore[nextIdx] = tentative;
        if (bonusArr) bonusArr[nextIdx] = bonus;
        lenArr[nextIdx] = next.cost.length;
        turnsArr[nextIdx] = next.cost.turns;
        hopsArr[nextIdx] = next.cost.hops;
        nearArr[nextIdx] = next.cost.near;
        wire1Arr[nextIdx] = next.cost.nearBreakdown.wire1;
        wire2Arr[nextIdx] = next.cost.nearBreakdown.wire2;
        obs1Arr[nextIdx] = next.cost.nearBreakdown.obs1;
        obs2Arr[nextIdx] = next.cost.nearBreakdown.obs2;
        enqueueFrontier(nextIdx, tentative + heuristic(next.state, end) * hFactor);
      }
    };

    for (const next of neighbors) relax(next);
  }

  if (deterministic) {
    return makeEmptyResult(expansions > maxExpansions ? 'timeout' : 'no_path');
  }
  if (Date.now() - searchStart > opts.maxTimeMs) return makeEmptyResult('timeout');
  return makeEmptyResult('no_path');
}
//=============================================================================
function gatherNeighbors(
  state: any,
  start: any,
  end: any,
  _startStep: any,
  endStep: any,
  endIncomingIdx: number,
  occupancy: any,
  obstacles: any,
  obstacleGrid: any,
  obstacleNearMap: any,
  obstacleOwners: any,
  wireNearMap: any,
  bounds: any,
  searchBounds: any,
  opts: any,
  _currentStats: any,
  meta: any,
  allowedSet: Set<number>
) {
  const dirVectors = [
    { x: 1, y: 0, idx: 0 },
    { x: -1, y: 0, idx: 1 },
    { x: 0, y: 1, idx: 2 },
    { x: 0, y: -1, idx: 3 },
  ];
  const results: any[] = [];

  for (const dv of dirVectors) {
    if (state.hopLock > 0 && dv.idx !== state.dir) continue;
    if (state.x === start.x && state.y === start.y && dv.idx !== state.dir) continue;
    const candidate = evaluateNeighborCandidate({
      state,
      dv,
      start,
      end,
      startStep: _startStep,
      endStep,
      endIncomingIdx,
      occupancy,
      obstacles,
      obstacleGrid,
      obstacleNearMap,
      obstacleOwners,
      wireNearMap,
      bounds,
      searchBounds,
      opts,
      _currentStats,
      meta,
      allowedSet,
    });
    if (candidate) results.push(candidate);
  }
  return results;
}
//=============================================================================
function evaluateNeighborCandidate(arg: any) {
  const {
    state,
    dv,
    start,
    end,
    endStep,
    endIncomingIdx,
    occupancy,
    obstacles,
    obstacleGrid,
    obstacleNearMap,
    obstacleOwners,
    wireNearMap,
    bounds,
    searchBounds,
    opts,
    _currentStats,
    meta,
    allowedSet,
  } = arg;

  const nx = state.x + dv.x;
  const ny = state.y + dv.y;
  const rb = searchBounds ?? bounds;
  if (nx < rb.minX || nx > rb.maxX || ny < rb.minY || ny > rb.maxY) return null;
  const pNext = { x: nx, y: ny };
  if (nx === end.x && ny === end.y && dv.idx !== endIncomingIdx) return null;
  if (nx === end.x && ny === end.y && (state.x !== endStep.x || state.y !== endStep.y)) return null;
  if (isBlocked(pNext, obstacles, allowedSet, obstacleGrid)) return null;
  if (occupancyEdgeBlocked(state, pNext, occupancy, allowedSet, meta)) return null;
  const eKey = edgeKeyFor(state, pNext);
  const eOcc = occupancy.edges.has(eKey);
  const occInfo = occupancyPointInfo(pNext, occupancy, allowedSet, meta);
  let nextHopLock = Math.max(state.hopLock - 1, 0);
  let hopCost = 0;
  if (occInfo.occupied) {
    const crossesVertical = occInfo.vertical && dv.x !== 0;
    const crossesHorizontal = occInfo.horizontal && dv.y !== 0;
    if (crossesVertical || crossesHorizontal) {
      nextHopLock = Math.max(nextHopLock, 1);
      hopCost = opts.hopCost;
    } else {
      return null;
    }
  }

  const turnCost = state.dir !== dv.idx ? opts.turnCost : 0;
  const portTurnCost = turnCost > 0 ? nearPortTurnPenalty(state, start, end, opts) : 0;
  const junctionCost =
    turnCost > 0 ? sharedJunctionPenalty(state, state.dir, dv.idx, occupancy, meta, opts) : 0;
  const nearData = proximityPenalty(
    pNext,
    obstacleGrid,
    obstacleNearMap,
    obstacleOwners,
    wireNearMap,
    occupancy,
    bounds,
    opts,
    meta,
    _currentStats
  );

  return {
    state: { x: nx, y: ny, dir: dv.idx, hopLock: nextHopLock },
    edgeKey: eKey,
    cost: buildCost(_currentStats, opts, {
      turnCost,
      hopCost,
      nearBreakdown: nearData.breakdown,
      nearCost: nearData.cost + junctionCost + portTurnCost,
      lengthIncrement: eOcc ? 0 : 1,
    }),
  };
}
export function buildResultFromArrays(
  endIdx: number,
  cameFrom: any,
  statsArrays: any,
  minX: number,
  minY: number,
  gridW: number,
  opts: any = null
) {
  const path: any[] = [];
  let currentIdx = endIdx;
  while (currentIdx >= 0) {
    const state = decodeState(currentIdx, minX, minY, gridW);
    path.push({ x: state.x, y: state.y });
    currentIdx = cameFrom[currentIdx];
  }
  path.reverse();
  const stats = {
    total: statsArrays.totalArr ? statsArrays.totalArr[endIdx] : 0,
    length: statsArrays.lenArr[endIdx] ?? 0,
    turns: statsArrays.turnsArr[endIdx] ?? 0,
    hops: statsArrays.hopsArr[endIdx] ?? 0,
    near: statsArrays.nearArr[endIdx] ?? 0,
    nearBreakdown: {
      wire1: statsArrays.wire1Arr[endIdx] ?? 0,
      wire2: statsArrays.wire2Arr[endIdx] ?? 0,
      obs1: statsArrays.obs1Arr[endIdx] ?? 0,
      obs2: statsArrays.obs2Arr[endIdx] ?? 0,
    },
  };
  if (opts) {
    // TRUE total from clean components; gScore may hold a discounted value
    // when route hysteresis was active during the search.
    stats.total =
      stats.length * (Number(opts.lengthCost) || 0) +
      stats.turns * (Number(opts.turnCost) || 0) +
      stats.hops * (Number(opts.hopCost) || 0) +
      stats.near;
  } else {
    stats.total = stats.total || stats.length + stats.turns + stats.hops + stats.near;
  }
  return { points: path, cost: stats };
}
//=============================================================================
function makeEmptyResult(reason: string) {
  return { points: [], cost: { total: 0, length: 0, turns: 0, hops: 0, near: 0 }, reason };
}
//=============================================================================
function heuristic(point: any, end: any) {
  return Math.abs(point.x - end.x) + Math.abs(point.y - end.y);
}
//=============================================================================
function dirToVector(dir: any) {
  if (!dir) return null;
  if (dir === 'right' || dir === 0) return { x: 1, y: 0 };
  if (dir === 'left' || dir === 180) return { x: -1, y: 0 };
  if (dir === 'down' || dir === 90) return { x: 0, y: 1 };
  if (dir === 'up' || dir === 270) return { x: 0, y: -1 };
  return null;
}
//=============================================================================
function dirKey(dir: any) {
  if (dir.x === 1 && dir.y === 0) return 'r';
  if (dir.x === -1 && dir.y === 0) return 'l';
  if (dir.x === 0 && dir.y === 1) return 'd';
  if (dir.x === 0 && dir.y === -1) return 'u';
  return '';
}
//=============================================================================
function dirIndex(key: any) {
  if (key === 'r') return 0;
  if (key === 'l') return 1;
  if (key === 'd') return 2;
  if (key === 'u') return 3;
  return 0;
}
export function stateIndex(cell: number, dirIdx: number, hopLock: number) {
  return (cell * 4 + dirIdx) * 2 + hopLock;
}
export function decodeState(idx: number, minX: number, minY: number, gridW: number) {
  const cell = Math.floor(idx / 8);
  const rem = idx - cell * 8;
  const dir = Math.floor(rem / 2);
  const hopLock = rem % 2;
  const x = (cell % gridW) + minX;
  const y = Math.floor(cell / gridW) + minY;
  return { x, y, dir, hopLock };
}
/**
 * Search padding around the diagram bounds, in grid cells. Optimal detours stay
 * close to the bounding box, so the padding is capped: a canvas-proportional
 * padding inflated the A* state space by an order of magnitude on large diagrams.
 */
export function computeSearchPadding(width: number, height: number) {
  return Math.max(20, Math.min(40, Math.ceil(Math.max(width, height) / GRID_SIZE) + 5));
}
//=============================================================================
export function routeAllConnections(
  state: any,
  width: number,
  height: number,
  _offset = { x: 0, y: 0 },
  timeLimitMs?: number
) {
  const { nodes, connections, obstacles } = buildRouter2Input(state);
  const settings = {
    maxTimeMs: timeLimitMs ?? 400,
    incremental: false,
    fullOptimize: true,
    searchPadding: computeSearchPadding(width, height),
    nearObstaclePenalty1: 2,
    nearObstaclePenalty2: 0,
    nearWirePenalty1: 3,
    nearWirePenalty2: 1,
  };
  const result = computeWireRoutes({ nodes, connections, obstacles, settings });
  if (typeof window !== 'undefined') {
    const quality = analyzeResultWireQuality(connections, result.wires);
    (window as any).__routerLast = {
      nodes,
      connections,
      obstacles,
      result,
      settings,
      stats: {
        nodes: nodes.length,
        connections: connections.length,
        prevWires: 0,
        dirty: connections.length,
        changed: connections.length,
        quality,
      },
    };
  }
  return applyRouter2Result(state, connections, result);
}
//=============================================================================
export function routeDirtyConnections(
  state: any,
  width: number,
  height: number,
  _offset = { x: 0, y: 0 },
  dirtySet: any,
  timeLimitMs?: number
) {
  const expandedDirtySet = expandDirtySet(state, dirtySet);
  const { nodes, connections, obstacles, prevSolution } = buildRouter2Input(
    state,
    expandedDirtySet
  );
  const changedConnections = new Set(
    connections.filter((conn: any) => expandedDirtySet.has(conn.__ref)).map((conn: any) => conn.key)
  );
  const settings = {
    maxTimeMs: timeLimitMs ?? 400,
    incremental: true,
    fullOptimize: false,
    changedConnections: Array.from(changedConnections),
    searchPadding: computeSearchPadding(width, height),
    nearObstaclePenalty1: 2,
    nearObstaclePenalty2: 0,
    nearWirePenalty1: 3,
    nearWirePenalty2: 1,
  };
  const result = computeWireRoutes({ nodes, connections, obstacles, prevSolution, settings });
  if (typeof window !== 'undefined') {
    const quality = analyzeResultWireQuality(connections, result.wires);
    const prevCount = prevSolution?.wires
      ? prevSolution.wires instanceof Map
        ? prevSolution.wires.size
        : Object.keys(prevSolution.wires).length
      : 0;
    (window as any).__routerLast = {
      nodes,
      connections,
      obstacles,
      result,
      settings,
      stats: {
        nodes: nodes.length,
        connections: connections.length,
        prevWires: prevCount,
        dirty: dirtySet?.size ?? 0,
        changed: changedConnections.size,
        quality,
      },
    };
  }
  return applyRouter2Result(state, connections, result);
}
//=============================================================================
function expandDirtySet(state: any, dirtySet: any): Set<any> {
  const base = new Set<any>(dirtySet || []);
  if (!base.size || !Array.isArray(state?.connections)) return base;

  const sourceKeys = new Set();
  const targetKeys = new Set();
  base.forEach((conn: any) => {
    sourceKeys.add(`${conn.from}:${conn.fromIndex ?? 0}`);
    targetKeys.add(`${conn.to}:${conn.toIndex ?? 0}`);
  });

  state.connections.forEach((conn: any) => {
    const fromKey = `${conn.from}:${conn.fromIndex ?? 0}`;
    const toKey = `${conn.to}:${conn.toIndex ?? 0}`;
    if (sourceKeys.has(fromKey) || targetKeys.has(toKey)) {
      base.add(conn);
    }
  });
  return base;
}
//=============================================================================
// compatibility alias (old name)
export const routeConnections2 = computeWireRoutes;
//=============================================================================
