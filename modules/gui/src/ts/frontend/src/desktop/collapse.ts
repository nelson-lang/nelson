//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Collapsible dock groups, mirroring the Qt desktop's dock title-bar chevron:
// a collapsed group is pinned to the height of its tab strip, and expanding
// restores the height it had before.
//
// Dockview has no collapse notion of its own, so it is done with the grid
// constraints + size API on the group. The remembered height lives in a module
// map keyed by group id (a collapsed group's serialized size is its collapsed
// one, so the expanded height would otherwise be lost on reload; it falls
// back to a readable default).
import type { DockviewApi, DockviewGroupPanel } from 'dockview';
//=============================================================================
// The tab strip owns the group height when collapsed.
const HEADER_HEIGHT = 35;
// Used when a group is expanded without a remembered height (restored layout).
const FALLBACK_EXPANDED_HEIGHT = 260;
const expandedHeights = new Map<string, number>();
const collapsed = new Set<string>();
const listeners = new Set<() => void>();
function notify(): void {
  for (const l of listeners) l();
}
//=============================================================================
export function onCollapseChange(cb: () => void): () => void {
  listeners.add(cb);
  return () => listeners.delete(cb);
}
//=============================================================================
export function isGroupCollapsed(group: DockviewGroupPanel): boolean {
  return collapsed.has(group.id);
}
//=============================================================================
export function collapseGroup(group: DockviewGroupPanel): void {
  if (collapsed.has(group.id)) return;
  const height = group.api.height;
  if (height > HEADER_HEIGHT) {
    expandedHeights.set(group.id, height);
  }
  collapsed.add(group.id);
  // Both bounds are pinned: a minimum height set elsewhere would otherwise win
  // over the maximum and leave the group taller than its tab strip.
  group.api.setConstraints({ maximumHeight: HEADER_HEIGHT, minimumHeight: HEADER_HEIGHT });
  group.api.setSize({ height: HEADER_HEIGHT });
  notify();
}
//=============================================================================
export function expandGroup(group: DockviewGroupPanel): void {
  if (!collapsed.has(group.id)) return;
  collapsed.delete(group.id);
  group.api.setConstraints({ maximumHeight: Number.MAX_SAFE_INTEGER, minimumHeight: 0 });
  group.api.setSize({ height: expandedHeights.get(group.id) ?? FALLBACK_EXPANDED_HEIGHT });
  notify();
}
//=============================================================================
export function toggleGroupCollapsed(group: DockviewGroupPanel): void {
  if (collapsed.has(group.id)) {
    expandGroup(group);
  } else {
    collapseGroup(group);
  }
}
//=============================================================================
// A group that goes away (last tab closed, panels moved out) must not keep a
// stale collapsed flag: ids are reused across layouts.
export function forgetGroup(groupId: string): void {
  collapsed.delete(groupId);
  expandedHeights.delete(groupId);
}
//=============================================================================
export function clearCollapsedGroups(): void {
  collapsed.clear();
  expandedHeights.clear();
  notify();
}
//=============================================================================
export function snapshotCollapsedGroups(api: DockviewApi): Record<string, number> {
  const snapshot: Record<string, number> = {};
  for (const group of api.groups) {
    if (collapsed.has(group.id)) {
      snapshot[group.id] = expandedHeights.get(group.id) ?? FALLBACK_EXPANDED_HEIGHT;
    }
  }
  return snapshot;
}
//=============================================================================
export function restoreCollapsedGroups(
  api: DockviewApi,
  snapshot: Record<string, number> | undefined
): void {
  clearCollapsedGroups();
  if (!snapshot) return;
  for (const group of api.groups) {
    if (group.api.location.type === 'floating') continue;
    const expandedHeight = snapshot[group.id];
    if (!Number.isFinite(expandedHeight) || expandedHeight <= HEADER_HEIGHT) continue;
    expandedHeights.set(group.id, expandedHeight);
    collapsed.add(group.id);
    group.api.setConstraints({ maximumHeight: HEADER_HEIGHT, minimumHeight: HEADER_HEIGHT });
    group.api.setSize({ height: HEADER_HEIGHT });
  }
  notify();
}
//=============================================================================
