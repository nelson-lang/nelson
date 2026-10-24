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
export type Point = { x: number; y: number };
export type Obstacle = { x0: number; y0: number; x1: number; y1: number; owner?: string };
export type Bounds = { minX: number; minY: number; maxX: number; maxY: number };
export type OccupancyPointInfo = {
  horizontal: boolean;
  vertical: boolean;
  edgeH: boolean;
  edgeV: boolean;
  hOwners: Set<string | number>;
  vOwners: Set<string | number>;
  occupied: boolean;
  owners: Set<string | number>;
  wireIds: Set<string | number>;
};
//=============================================================================
export type Occupancy = {
  points: Map<string, OccupancyPointInfo>;
  edges: Map<string, Set<string | number>>;
};
/** Packed cells covered by the obstacles, or null when there are none. */
export function buildBlockOccupancyMap(obstacles?: Obstacle[] | null): Set<number> | null {
  if (!obstacles || obstacles.length === 0) return null;
  const cells = new Set<number>();
  for (const rect of obstacles) {
    const startX = Math.floor(rect.x0);
    const endX = Math.floor(rect.x1);
    const startY = Math.floor(rect.y0);
    const endY = Math.floor(rect.y1);
    let cx = startX;
    while (cx <= endX) {
      let cy = startY;
      while (cy <= endY) {
        cells.add(pointKeyInt(cx, cy));
        cy += 1;
      }
      cx += 1;
    }
  }
  return cells;
}
/** Packed cell -> owner id ('' when the obstacle has none), or null without obstacles. */
export function buildObstacleOwners(obstacles?: Obstacle[] | null): Map<number, string> | null {
  if (!obstacles || obstacles.length === 0) return null;
  const ownerMap = new Map<number, string>();
  for (const rect of obstacles) {
    const owner = rect.owner ?? '';
    const x0 = Math.floor(rect.x0);
    const x1 = Math.floor(rect.x1);
    let xx = x0;
    while (xx <= x1) {
      let yy = Math.floor(rect.y0);
      const yEnd = Math.floor(rect.y1);
      while (yy <= yEnd) {
        ownerMap.set(pointKeyInt(xx, yy), owner);
        yy += 1;
      }
      xx += 1;
    }
  }
  return ownerMap;
}
/**
 * obs1: a non-friendly obstacle at distance 1; obs2: at distance 2 or diagonal.
 * Cells owned by a friendly owner are ignored.
 */
export function obstacleNearUnfriendly(
  point: Point,
  grid: Set<number>,
  owners: Map<number, string> | null,
  friendly: Set<string> | null
) {
  const friendlyCheck = (tx: number, ty: number) => {
    if (!friendly || !owners) return false;
    const k = pointKeyInt(tx, ty);
    if (!grid.has(k)) return true;
    const own = owners.get(k);
    return own != null && own !== '' && friendly.has(own);
  };

  const d1 = [
    [1, 0],
    [-1, 0],
    [0, 1],
    [0, -1],
  ];
  for (const [dx, dy] of d1) {
    const nx = point.x + dx;
    const ny = point.y + dy;
    const key = pointKeyInt(nx, ny);
    if (grid.has(key) && !friendlyCheck(nx, ny)) return { obs1: 1, obs2: 0 };
  }

  // distance-2 orthogonals + diagonals
  const d2 = [
    [2, 0],
    [-2, 0],
    [0, 2],
    [0, -2],
    [1, 1],
    [1, -1],
    [-1, 1],
    [-1, -1],
  ];
  for (const [dx, dy] of d2) {
    const nx = point.x + dx;
    const ny = point.y + dy;
    const key = pointKeyInt(nx, ny);
    if (grid.has(key) && !friendlyCheck(nx, ny)) return { obs1: 0, obs2: 1 };
  }
  return { obs1: 0, obs2: 0 };
}
/** obs1: obstacle on the point or adjacent to it; obs2: at distance 2 or diagonal. */
export function obstacleNear(point: Point, grid: Set<number>) {
  if (grid.has(pointKeyInt(point.x, point.y))) return { obs1: 1, obs2: 0 };
  const checkOrth = [
    [1, 0],
    [-1, 0],
    [0, 1],
    [0, -1],
  ];
  for (const [dx, dy] of checkOrth) {
    if (grid.has(pointKeyInt(point.x + dx, point.y + dy))) return { obs1: 1, obs2: 0 };
  }
  const checkFar = [
    [2, 0],
    [-2, 0],
    [0, 2],
    [0, -2],
    [1, 1],
    [1, -1],
    [-1, 1],
    [-1, -1],
  ];
  for (const [dx, dy] of checkFar) {
    if (grid.has(pointKeyInt(point.x + dx, point.y + dy))) return { obs1: 0, obs2: 1 };
  }
  return { obs1: 0, obs2: 0 };
}
export function cellIndex(x: number, y: number, minX: number, minY: number, gridW: number) {
  return (y - minY) * gridW + (x - minX);
}
/** Per-cell proximity code: 0 none, 1 adjacent, 2 at distance 2 or diagonal. */
export function buildObstacleNearMap(grid: Set<number> | null, bounds: Bounds | null) {
  if (!grid || !bounds) return null;
  const width = bounds.maxX - bounds.minX + 1;
  const height = bounds.maxY - bounds.minY + 1;
  const out = new Uint8Array(width * height);
  let row = bounds.minY;
  while (row <= bounds.maxY) {
    let col = bounds.minX;
    while (col <= bounds.maxX) {
      const index = cellIndex(col, row, bounds.minX, bounds.minY, width);
      const near = obstacleNear({ x: col, y: row }, grid as Set<number>);
      out[index] = near.obs1 ? 1 : near.obs2 ? 2 : 0;
      col += 1;
    }
    row += 1;
  }
  return out;
}
/** Same 0/1/2 codes as buildObstacleNearMap, measured from occupied wire points. */
export function buildWireNearMap(
  occupancy: Occupancy | { points: Map<string, unknown> } | null,
  bounds: Bounds | null
) {
  if (!occupancy || !bounds || !('points' in occupancy) || occupancy.points.size === 0) return null;
  const width = bounds.maxX - bounds.minX + 1;
  const height = bounds.maxY - bounds.minY + 1;
  const out = new Uint8Array(width * height);

  const safeMark = (x: number, y: number, val: number) => {
    if (x < bounds.minX || x > bounds.maxX || y < bounds.minY || y > bounds.maxY) return;
    const idx = cellIndex(x, y, bounds.minX, bounds.minY, width);
    if (val === 1 && out[idx] !== 1) out[idx] = 1;
    if (val === 2 && out[idx] === 0) out[idx] = 2;
  };

  for (const key of (occupancy.points as Map<string, unknown>).keys()) {
    const parts = key.split(',');
    const cx = Number(parts[0]);
    const cy = Number(parts[1]);
    if (!Number.isFinite(cx) || !Number.isFinite(cy)) continue;
    safeMark(cx + 1, cy, 1);
    safeMark(cx - 1, cy, 1);
    safeMark(cx, cy + 1, 1);
    safeMark(cx, cy - 1, 1);

    safeMark(cx + 2, cy, 2);
    safeMark(cx - 2, cy, 2);
    safeMark(cx, cy + 2, 2);
    safeMark(cx, cy - 2, 2);
    safeMark(cx + 1, cy + 1, 2);
    safeMark(cx + 1, cy - 1, 2);
    safeMark(cx - 1, cy + 1, 2);
    safeMark(cx - 1, cy - 1, 2);
  }
  return out;
}
/**
 * Incremental form of buildWireNearMap for freshly occupied points. Codes only
 * strengthen (1 over 2, 2 over 0), so adding a wire needs no rebuild; only
 * removals (rip-up) do.
 */
export function wireNearMapMarkPoints(
  map: Uint8Array,
  bounds: Bounds,
  points: Point[] | null | undefined
) {
  if (!map || !bounds || !Array.isArray(points)) return;
  const width = bounds.maxX - bounds.minX + 1;
  const safeMark = (x: number, y: number, val: number) => {
    if (x < bounds.minX || x > bounds.maxX || y < bounds.minY || y > bounds.maxY) return;
    const idx = cellIndex(x, y, bounds.minX, bounds.minY, width);
    if (val === 1 && map[idx] !== 1) map[idx] = 1;
    if (val === 2 && map[idx] === 0) map[idx] = 2;
  };
  for (const p of points) {
    const cx = p.x;
    const cy = p.y;
    safeMark(cx + 1, cy, 1);
    safeMark(cx - 1, cy, 1);
    safeMark(cx, cy + 1, 1);
    safeMark(cx, cy - 1, 1);

    safeMark(cx + 2, cy, 2);
    safeMark(cx - 2, cy, 2);
    safeMark(cx, cy + 2, 2);
    safeMark(cx, cy - 2, 2);
    safeMark(cx + 1, cy + 1, 2);
    safeMark(cx + 1, cy - 1, 2);
    safeMark(cx - 1, cy + 1, 2);
    safeMark(cx - 1, cy - 1, 2);
  }
}
export function pointKey(point: Point) {
  return `${point.x},${point.y}`;
}
/** Packs two 16-bit coordinates into one 32-bit integer key. */
export function pointKeyInt(x: number, y: number) {
  return ((x & 0xffff) << 16) | (y & 0xffff);
}
/** Canonical edge key: `v:x:y0:y1` for a vertical edge, `h:x0:x1:y` for a horizontal one. */
export function edgeKeyFor(a: Point, b: Point) {
  if (a.x === b.x) {
    const y0 = a.y < b.y ? a.y : b.y;
    const y1 = a.y < b.y ? b.y : a.y;
    return `v:${a.x}:${y0}:${y1}`;
  }
  const x0 = a.x < b.x ? a.x : b.x;
  const x1 = a.x < b.x ? b.x : a.x;
  return `h:${x0}:${x1}:${a.y}`;
}
/** Cells in allowedSet are never blocked; obstacleGrid, when given, replaces the obstacle scan. */
export function isBlocked(
  point: Point,
  obstacles?: Obstacle[] | null,
  allowedSet?: Set<number>,
  obstacleGrid?: Set<number> | null
) {
  const packed = pointKeyInt(point.x, point.y);
  if (allowedSet && allowedSet.has(packed)) return false;
  if (obstacleGrid) return obstacleGrid.has(packed);
  if (!obstacles || obstacles.length === 0) return false;
  for (const r of obstacles) {
    if (point.x >= r.x0 && point.x <= r.x1 && point.y >= r.y0 && point.y <= r.y1) return true;
  }
  return false;
}
export function buildOccupancy(
  wires: {
    points: Point[];
    meta?: {
      from?: string | number;
      to?: string | number;
      key?: string | number;
      fromBlock?: string;
      toBlock?: string;
    };
  }[]
) {
  const pts = new Map<string, OccupancyPointInfo>();
  const eds = new Map<string, Set<string | number>>();
  const list = Array.isArray(wires) ? wires : [];
  for (const w of list) occupancyAddWire({ points: pts, edges: eds }, w.points, w.meta);
  return { points: pts, edges: eds } as Occupancy;
}
/** Adds a wire's points, edge ownership and wire ids to occupancy (mutates it). */
export function occupancyAddWire(
  occupancy: Occupancy,
  points: Point[],
  meta?: {
    from?: string | number;
    to?: string | number;
    key?: string | number;
    fromBlock?: string;
    toBlock?: string;
  }
) {
  const owners = new Set<string | number>();
  if (meta) {
    if (meta.from !== undefined) owners.add(meta.from);
    if (meta.to !== undefined) owners.add(meta.to);
  }
  const wires = new Set<string | number>();
  if (meta && meta.key !== undefined) wires.add(meta.key);

  const getOrCreate = (p: Point) => {
    const k = pointKey(p);
    const existing = occupancy.points.get(k);
    if (existing) return existing;
    const blank: OccupancyPointInfo = {
      horizontal: false,
      vertical: false,
      edgeH: false,
      edgeV: false,
      hOwners: new Set<string | number>(),
      vOwners: new Set<string | number>(),
      occupied: false,
      owners: new Set<string | number>(),
      wireIds: new Set<string | number>(),
    };
    occupancy.points.set(k, blank);
    return blank;
  };

  const flagEdge = (info: OccupancyPointInfo, orient: 'H' | 'V') => {
    if (orient === 'V') {
      info.edgeV = true;
      for (const id of owners) if (id != null) info.vOwners.add(id);
    } else {
      info.edgeH = true;
      for (const id of owners) if (id != null) info.hOwners.add(id);
    }
  };

  const pts = Array.isArray(points) ? points : [];
  for (let i = 0; i < pts.length; i += 1) {
    const curr = pts[i];
    const info = getOrCreate(curr);
    info.occupied = true;
    for (const id of owners) if (id != null) info.owners.add(id);
    for (const wid of wires) if (wid != null) info.wireIds.add(wid);

    if (i > 0) {
      const prev = pts[i - 1];
      const prevInfo = getOrCreate(prev);
      const orient = prev.x === curr.x ? 'V' : prev.y === curr.y ? 'H' : null;
      if (orient) {
        flagEdge(prevInfo, orient as 'H' | 'V');
        flagEdge(info, orient as 'H' | 'V');
      }
      const eKey = edgeKeyFor(prev, curr);
      const edgeSet = occupancy.edges.get(eKey) ?? new Set<string | number>();
      for (const id of owners) if (id != null) edgeSet.add(id);
      if (meta && meta.key !== undefined) edgeSet.add(meta.key as string | number);
      occupancy.edges.set(eKey, edgeSet);

      if (i < pts.length - 1) {
        const next = pts[i + 1];
        if (prev.x === curr.x && curr.x === next.x) info.vertical = true;
        if (prev.y === curr.y && curr.y === next.y) info.horizontal = true;
      }
    }
  }
}
/** Endpoints in allowedSet and edges owned by meta (from/to/key) are never blocked. */
export function occupancyEdgeBlocked(
  from: Point,
  to: Point,
  occupancy: Occupancy,
  allowedSet: Set<number>,
  meta?: {
    from?: string | number;
    to?: string | number;
    key?: string | number;
    fromBlock?: string;
    toBlock?: string;
  }
) {
  const a = pointKeyInt(from.x, from.y);
  const b = pointKeyInt(to.x, to.y);
  if (allowedSet && (allowedSet.has(a) || allowedSet.has(b))) return false;
  const eKey = edgeKeyFor(from, to);
  const owners = occupancy.edges.get(eKey);
  if (!owners) return false;
  if (!meta) return true;
  if (meta.from !== undefined && owners.has(meta.from as string | number)) return false;
  if (meta.to !== undefined && owners.has(meta.to as string | number)) return false;
  if (meta.key !== undefined && owners.has(meta.key as string | number)) return false;
  return true;
}
/**
 * Empty info for points in allowedSet or absent from occupancy. When meta owns the
 * point, the owners are kept but the occupied and edge flags are cleared.
 */
export function occupancyPointInfo(
  point: Point,
  occupancy: Occupancy,
  allowedSet: Set<number>,
  meta?: {
    from?: string | number;
    to?: string | number;
    key?: string | number;
    fromBlock?: string;
    toBlock?: string;
  }
): OccupancyPointInfo {
  const empty: OccupancyPointInfo = {
    horizontal: false,
    vertical: false,
    edgeH: false,
    edgeV: false,
    hOwners: new Set<string | number>(),
    vOwners: new Set<string | number>(),
    occupied: false,
    owners: new Set<string | number>(),
    wireIds: new Set<string | number>(),
  };

  const packed = pointKeyInt(point.x, point.y);
  if (allowedSet && allowedSet.has(packed)) return empty;

  const info = occupancy.points.get(pointKey(point));
  if (!info) return empty;

  const metaMatches =
    meta &&
    ((meta.from !== undefined && info.owners.has(meta.from as string | number)) ||
      (meta.to !== undefined && info.owners.has(meta.to as string | number)) ||
      (meta.key !== undefined && info.wireIds.has(meta.key as string | number)));

  if (metaMatches) {
    return {
      horizontal: false,
      vertical: false,
      edgeH: false,
      edgeV: false,
      hOwners: info.hOwners,
      vOwners: info.vOwners,
      occupied: false,
      owners: info.owners,
      wireIds: info.wireIds,
    };
  }

  return info;
}
//=============================================================================
// compatibility alias (old name)
export const buildObstacleGrid = buildBlockOccupancyMap;
//=============================================================================
