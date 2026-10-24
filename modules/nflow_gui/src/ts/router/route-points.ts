//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type Point = { x: number; y: number };
type ConnectionLike = { points?: unknown; [key: string]: unknown };
type Snapshot = { byIndex?: unknown; byKey?: unknown };
// Accepts `[x,y]` entries or `{x,y}` objects. Returns null when fewer than 2 valid points remain.
export function cloneRoutePoints(input: unknown): Point[] | null {
  if (!Array.isArray(input) || input.length < 2) return null;
  const normalized: Point[] = [];
  for (const entry of input) {
    if (Array.isArray(entry) && entry.length >= 2) {
      const x = Number(entry[0]);
      const y = Number(entry[1]);
      if (Number.isFinite(x) && Number.isFinite(y)) normalized.push({ x, y });
      continue;
    }
    if (entry && typeof entry === 'object') {
      const obj = entry as { x?: unknown; y?: unknown };
      const x = Number(obj.x);
      const y = Number(obj.y);
      if (Number.isFinite(x) && Number.isFinite(y)) normalized.push({ x, y });
    }
  }
  return normalized.length >= 2 ? normalized : null;
}
export function captureRoutePointsSnapshot(
  conns: ConnectionLike[],
  keyFn: (conn: ConnectionLike) => string
) {
  const byIndex: (Point[] | null)[] = [];
  const byKey: Record<string, Point[][]> = Object.create(null);
  if (!Array.isArray(conns)) return { byIndex, byKey };
  for (const c of conns) {
    const pts = cloneRoutePoints(c.points);
    byIndex.push(pts);
    if (!pts) continue;
    const k = keyFn(c);
    if (!byKey[k]) byKey[k] = [];
    byKey[k].push(pts);
  }
  return { byIndex, byKey };
}
// Returns the number of connections updated.
export function applyRoutePointsSnapshot(
  conns: ConnectionLike[],
  snap: Snapshot,
  keyFn: (conn: ConnectionLike) => string
) {
  if (!Array.isArray(conns) || !snap || typeof snap !== 'object') return 0;
  let applied = 0;

  const rawByIndex = Array.isArray((snap as Snapshot).byIndex)
    ? ((snap as Snapshot).byIndex as unknown[])
    : null;
  const indexMode = rawByIndex && rawByIndex.length === conns.length;
  if (indexMode) {
    for (let i = 0; i < conns.length; i += 1) {
      const candidate = cloneRoutePoints(rawByIndex![i]);
      if (!candidate) continue;
      conns[i].points = candidate;
      applied += 1;
    }
    return applied;
  }

  const byKeyRaw =
    snap &&
    typeof snap === 'object' &&
    (snap as Snapshot).byKey &&
    typeof (snap as Snapshot).byKey === 'object'
      ? (snap as Snapshot).byKey
      : null;
  if (!byKeyRaw) return 0;

  const queues = new Map<string, Point[][]>();
  for (const [k, q] of Object.entries(byKeyRaw)) {
    if (!Array.isArray(q)) continue;
    const parsed: Point[][] = [];
    for (const item of q) {
      const pts = cloneRoutePoints(item);
      if (pts) parsed.push(pts);
    }
    if (parsed.length) queues.set(k, parsed);
  }

  for (const c of conns) {
    const key = keyFn(c);
    const q = queues.get(key);
    if (!q || q.length === 0) continue;
    const next = q.shift();
    if (!next) continue;
    c.points = next;
    applied += 1;
    if (q.length === 0) queues.delete(key);
  }

  return applied;
}
//=============================================================================
