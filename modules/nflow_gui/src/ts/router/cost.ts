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
import {
  pointKey,
  pointKeyInt,
  cellIndex,
  obstacleNear,
  obstacleNearUnfriendly,
  isBlocked,
  edgeKeyFor,
  occupancyEdgeBlocked,
  occupancyPointInfo,
  Point,
  Bounds,
  Occupancy,
} from './grid.js';
//=============================================================================
type CostOpts = {
  preferredPortStubCells?: number;
  nearPortTurnPenalty?: number;
  junctionPenalty?: number;
  hopCost?: number;
  nearWirePenalty1?: number;
  nearWirePenalty2?: number;
  nearObstaclePenalty1?: number;
  nearObstaclePenalty2?: number;
  lengthCost?: number;
  turnCost?: number;
  [k: string]: unknown;
};
//=============================================================================
type Meta = {
  from?: string | number;
  to?: string | number;
  fromBlock?: string;
  toBlock?: string;
  key?: string | number;
};
//=============================================================================
type NearBreakdown = { wire1: number; wire2: number; obs1: number; obs2: number };
/**
 * Extra penalty for turns near the source/destination ports. Uses a squared
 * deficit relative to `preferredPortStubCells` to discourage tight port-adjacent
 * turns.
 */
export function nearPortTurnPenalty(point: Point, start: Point, end: Point, opts: CostOpts) {
  const want = Math.max(1, Number(opts.preferredPortStubCells) || 2);
  const p = Number(opts.nearPortTurnPenalty) || 0;
  if (p <= 0) return 0;
  const ds = Math.abs(point.x - start.x) + Math.abs(point.y - start.y);
  const de = Math.abs(point.x - end.x) + Math.abs(point.y - end.y);
  const deficit = Math.max(0, want - ds) + Math.max(0, want - de);
  return deficit > 0 ? p * deficit * deficit * 8 : 0;
}
//=============================================================================
function dirOrientation(dirIdx: number) {
  // Map 0/1 -> H, 2/3 -> V
  const mapping = ['H', 'H', 'V', 'V'];
  return mapping[dirIdx] ?? null;
}
/**
 * Penalty for a turn that would break a shared junction owned by the current
 * connection endpoints. Applies only when the turn changes the occupied edge
 * orientation.
 */
export function sharedJunctionPenalty(
  statePoint: Point,
  prevDirIdx: number,
  nextDirIdx: number,
  occupancy: Occupancy,
  meta: Meta | undefined,
  opts: CostOpts
) {
  if (!meta) return 0;
  const a = dirOrientation(prevDirIdx);
  const b = dirOrientation(nextDirIdx);
  if (!a || !b || a === b) return 0;

  const info = occupancy.points.get(pointKey(statePoint));
  if (!info) return 0;
  const owns = (id?: string | number) =>
    id != null && (info.owners.has(id as string | number) as boolean);
  if (!owns(meta.from) && !owns(meta.to)) return 0;

  const edgeHas = (orient: 'H' | 'V') =>
    orient === 'H'
      ? Boolean(
          (info.hOwners && (meta.from ? info.hOwners.has(meta.from as string | number) : false)) ||
            (meta.to ? info.hOwners.has(meta.to as string | number) : false)
        )
      : Boolean(
          (info.vOwners && (meta.from ? info.vOwners.has(meta.from as string | number) : false)) ||
            (meta.to ? info.vOwners.has(meta.to as string | number) : false)
        );

  const prevHas = edgeHas(a as 'H' | 'V');
  const nextHas = edgeHas(b as 'H' | 'V');
  return prevHas && !nextHas ? (opts.junctionPenalty as number) || 0 : 0;
}
/**
 * Proximity penalty for a grid point from nearby wires and obstacles. Returns the
 * aggregated cost and a breakdown (wire1, wire2, obs1, obs2). Wires and blocks owned
 * by the connection endpoints are not counted.
 */
export function proximityPenalty(
  point: Point,
  obstacleGrid: Set<number> | null,
  obstacleNearMap: Uint8Array | null,
  obstacleOwners: Map<number, string> | null,
  wireNearMap: Uint8Array | null,
  occupancy: Occupancy,
  bounds: Bounds | null,
  opts: CostOpts,
  meta: Meta | undefined,
  _currentStats: any
) {
  let w1 = 0;
  let w2 = 0;
  let o1 = 0;
  let o2 = 0;

  let wireCode = 0;
  if (wireNearMap && bounds) {
    const w = bounds.maxX - bounds.minX + 1;
    const idx = cellIndex(point.x, point.y, bounds.minX, bounds.minY, w);
    wireCode = wireNearMap[idx] || 0;
  }

  const checkNear = (dxdy: number[][]) => {
    for (const [dx, dy] of dxdy) {
      const cand = { x: point.x + dx, y: point.y + dy } as Point;
      const info = occupancy.points.get(pointKey(cand));
      if (!info || !info.wireIds) continue;
      if (
        meta &&
        info.owners &&
        ((meta.from !== undefined && info.owners.has(meta.from as string | number)) ||
          (meta.to !== undefined && info.owners.has(meta.to as string | number)))
      )
        continue;
      return true;
    }
    return false;
  };

  if (wireCode !== 0) {
    if (wireCode === 1) {
      if (
        checkNear([
          [1, 0],
          [-1, 0],
          [0, 1],
          [0, -1],
        ])
      )
        w1 = 1;
    }
    if (
      w1 === 0 &&
      checkNear([
        [2, 0],
        [-2, 0],
        [0, 2],
        [0, -2],
        [1, 1],
        [1, -1],
        [-1, 1],
        [-1, -1],
      ])
    )
      w2 = 1;
  }

  if (obstacleGrid) {
    const friendly =
      meta && (meta.fromBlock || meta.toBlock)
        ? new Set<string>(
            [meta.fromBlock as string, meta.toBlock as string].filter((b) => b != null && b !== '')
          )
        : null;
    if (friendly && friendly.size > 0 && obstacleOwners) {
      const near = obstacleNearUnfriendly(
        point,
        obstacleGrid as Set<number>,
        obstacleOwners,
        friendly
      );
      o1 = near.obs1;
      o2 = near.obs2;
    } else if (obstacleNearMap && bounds) {
      const w = bounds.maxX - bounds.minX + 1;
      const idx = cellIndex(point.x, point.y, bounds.minX, bounds.minY, w);
      const code = obstacleNearMap[idx] || 0;
      o1 = code === 1 ? 1 : 0;
      o2 = code === 2 ? 1 : 0;
    } else {
      const near = obstacleNear(point, obstacleGrid as Set<number>);
      o1 = near.obs1;
      o2 = near.obs2;
    }
  }

  const totalCost =
    w1 * (opts.nearWirePenalty1 as number) +
    w2 * (opts.nearWirePenalty2 as number) +
    o1 * (opts.nearObstaclePenalty1 as number) +
    o2 * (opts.nearObstaclePenalty2 as number);

  return { cost: totalCost, breakdown: { wire1: w1, wire2: w2, obs1: o1, obs2: o2 } };
}
/**
 * Add one step's cost components to the running stats and return the new stats
 * `{ total, length, turns, hops, near, nearBreakdown }`.
 */
export function buildCost(
  currentStats: {
    length: number;
    turns: number;
    hops: number;
    near: number;
    nearBreakdown?: NearBreakdown;
  },
  opts: CostOpts,
  {
    turnCost,
    hopCost,
    nearCost,
    nearBreakdown,
    lengthIncrement,
  }: {
    turnCost?: number;
    hopCost?: number;
    nearCost?: number;
    nearBreakdown?: NearBreakdown;
    lengthIncrement?: number;
  }
) {
  const L = currentStats.length + (lengthIncrement ?? 1);
  const T = currentStats.turns + (turnCost ? 1 : 0);
  const H = currentStats.hops + (hopCost ? 1 : 0);
  const N = currentStats.near + (nearCost || 0);

  const w1 = (currentStats.nearBreakdown?.wire1 ?? 0) + (nearBreakdown?.wire1 ?? 0);
  const w2 = (currentStats.nearBreakdown?.wire2 ?? 0) + (nearBreakdown?.wire2 ?? 0);
  const o1 = (currentStats.nearBreakdown?.obs1 ?? 0) + (nearBreakdown?.obs1 ?? 0);
  const o2 = (currentStats.nearBreakdown?.obs2 ?? 0) + (nearBreakdown?.obs2 ?? 0);

  const total =
    L * (opts.lengthCost as number) +
    T * (opts.turnCost as number) +
    H * (opts.hopCost as number) +
    N;
  return {
    total,
    length: L,
    turns: T,
    hops: H,
    near: N,
    nearBreakdown: { wire1: w1, wire2: w2, obs1: o1, obs2: o2 },
  };
}
/**
 * Re-cost an existing (densified, orthogonal, grid-space) path under the
 * CURRENT occupancy and obstacles, replaying the exact per-step cost model of
 * the A* search. Used by route hysteresis to decide whether a freshly found
 * route improves enough over the previous one to justify changing shape.
 *
 * Returns `{ feasible, cost }`; `feasible` is false when the path now crosses
 * a blocked cell or a blocked wire edge (mirrored in RouterCost.cpp).
 */
export function evaluatePathCost(
  points: Point[],
  env: {
    obstacles: any[];
    obstacleGrid: Set<number> | null;
    obstacleNearMap: Uint8Array | null;
    obstacleOwners: Map<number, string> | null;
    wireNearMap: Uint8Array | null;
    occupancy: Occupancy;
    bounds: Bounds | null;
    opts: CostOpts;
    meta: Meta | undefined;
  }
): { feasible: boolean; cost: ReturnType<typeof buildCost> } {
  const {
    obstacles,
    obstacleGrid,
    obstacleNearMap,
    obstacleOwners,
    wireNearMap,
    occupancy,
    bounds,
    opts,
    meta,
  } = env;

  let stats = {
    total: 0,
    length: 0,
    turns: 0,
    hops: 0,
    near: 0,
    nearBreakdown: { wire1: 0, wire2: 0, obs1: 0, obs2: 0 },
  };
  const infeasible = { feasible: false, cost: stats };
  if (!points || points.length < 2) return infeasible;

  const start = points[0];
  const end = points[points.length - 1];
  // Same tolerance the search grants around ports.
  const allowedSet = new Set<number>([
    pointKeyInt(start.x, start.y),
    pointKeyInt(end.x, end.y),
    pointKeyInt(points[1].x, points[1].y),
    pointKeyInt(points[points.length - 2].x, points[points.length - 2].y),
  ]);

  const dirOf = (a: Point, b: Point): number | null => {
    if (b.x === a.x + 1 && b.y === a.y) return 0;
    if (b.x === a.x - 1 && b.y === a.y) return 1;
    if (b.x === a.x && b.y === a.y + 1) return 2;
    if (b.x === a.x && b.y === a.y - 1) return 3;
    return null;
  };

  let prevDir: number | null = null;
  for (let i = 1; i < points.length; i += 1) {
    const from = points[i - 1];
    const to = points[i];
    const dir = dirOf(from, to);
    if (dir === null) return infeasible; // not densified/orthogonal

    if (isBlocked(to, obstacles, allowedSet, obstacleGrid)) return infeasible;
    if (occupancyEdgeBlocked(from, to, occupancy, allowedSet, meta)) return infeasible;

    const eOcc = occupancy.edges.has(edgeKeyFor(from, to));
    const occInfo = occupancyPointInfo(to, occupancy, allowedSet, meta);
    let hopCost = 0;
    if (occInfo.occupied) {
      const crossesVertical = occInfo.vertical && (dir === 0 || dir === 1);
      const crossesHorizontal = occInfo.horizontal && (dir === 2 || dir === 3);
      if (crossesVertical || crossesHorizontal) {
        hopCost = (opts.hopCost as number) || 0;
      } else {
        return infeasible;
      }
    }

    const turnCost = prevDir !== null && prevDir !== dir ? (opts.turnCost as number) || 0 : 0;
    const portTurnCost = turnCost > 0 ? nearPortTurnPenalty(from, start, end, opts) : 0;
    const junctionCost =
      turnCost > 0 && prevDir !== null
        ? sharedJunctionPenalty(from, prevDir, dir, occupancy, meta, opts)
        : 0;
    const nearData = proximityPenalty(
      to,
      obstacleGrid,
      obstacleNearMap,
      obstacleOwners,
      wireNearMap,
      occupancy,
      bounds,
      opts,
      meta,
      stats
    );

    stats = buildCost(stats, opts, {
      turnCost,
      hopCost,
      nearBreakdown: nearData.breakdown,
      nearCost: nearData.cost + junctionCost + portTurnCost,
      lengthIncrement: eOcc ? 0 : 1,
    });
    prevDir = dir;
  }

  return { feasible: true, cost: stats };
}
//=============================================================================
