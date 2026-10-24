//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type Port = { id: string | number; name: string };
const toNum = (value: unknown): number => {
  const num = Number(value as unknown);
  return Number.isFinite(num) ? num : 0;
};
//=============================================================================
type Item = { id?: unknown; x?: unknown; y?: unknown; name?: unknown };
function stableSortExternalPorts(items: Item[]) {
  return items.slice().sort((a, b) => {
    const dy = toNum(a.y) - toNum(b.y);
    if (dy !== 0) return dy;
    const dx = toNum(a.x) - toNum(b.x);
    if (dx !== 0) return dx;
    return String(a.id ?? '').localeCompare(String(b.id ?? ''));
  });
}
//=============================================================================
export function collectExternalPorts(blocks: unknown, type: unknown): Port[] {
  if (!Array.isArray(blocks)) return [];
  const filtered: Item[] = (blocks as unknown[])
    .filter((b) => {
      if (!b || typeof b !== 'object') return false;
      const bb = b as Record<string, unknown>;
      return bb.type === type && (bb.params as Record<string, unknown>)?.isExternalPort === true;
    })
    .map((b) => {
      const bb = b as Record<string, unknown>;
      return {
        id: bb.id,
        name: String((bb.params as Record<string, unknown>)?.name ?? bb.id),
        x: toNum(bb.x),
        y: toNum(bb.y),
      };
    });
  const sorted = stableSortExternalPorts(filtered);
  return sorted.map(({ id, name }) => ({ id: id as string | number, name: String(name) }));
}
//=============================================================================
export function stabilizeExternalPortOrder(nextPorts: unknown, previousPorts: unknown): Port[] {
  const next = Array.isArray(nextPorts)
    ? (nextPorts as unknown[]).map((p) => {
        const pp =
          p && typeof p === 'object'
            ? (p as Record<string, unknown>)
            : ({} as Record<string, unknown>);
        return { id: pp.id as string | number, name: String(pp.name ?? pp.id ?? '') };
      })
    : [];
  const previous = Array.isArray(previousPorts) ? (previousPorts as unknown[]) : [];
  if (!next.length || !previous.length) return next;
  const nextMap = new Map(next.map((p) => [String(p.id), p]));
  const sameSet =
    next.length === previous.length &&
    previous.every((p) => nextMap.has(String((p as Record<string, unknown>)?.id)));
  if (!sameSet) return next;
  const ordered: Port[] = [];
  previous.forEach((p) => {
    const idStr = String((p as Record<string, unknown>)?.id);
    const match = nextMap.get(idStr);
    if (match) ordered.push(match);
  });
  return ordered.length === next.length ? ordered : next;
}
//=============================================================================
export function externalPortsChanged(previousPorts: unknown, nextPorts: unknown): boolean {
  const prev = Array.isArray(previousPorts) ? (previousPorts as unknown[]) : [];
  const next = Array.isArray(nextPorts) ? (nextPorts as unknown[]) : [];
  if (prev.length !== next.length) return true;
  for (let i = 0; i < prev.length; i += 1) {
    const a =
      prev[i] && typeof prev[i] === 'object'
        ? (prev[i] as Record<string, unknown>)
        : ({} as Record<string, unknown>);
    const b =
      next[i] && typeof next[i] === 'object'
        ? (next[i] as Record<string, unknown>)
        : ({} as Record<string, unknown>);
    if (String(a.id ?? '') !== String(b.id ?? '')) return true;
    if (String(a.name ?? '') !== String(b.name ?? '')) return true;
  }
  return false;
}
//=============================================================================
