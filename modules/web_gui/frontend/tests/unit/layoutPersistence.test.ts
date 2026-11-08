//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import type { SerializedDockview } from 'dockview';
import { sanitizeDockviewForPersistence } from '../../src/desktop/layout';
//=============================================================================
// The docked Documentation panel ('help') is the fallback for the dedicated help
// window and is deliberately not persisted, so a restored layout never brings
// back an empty Documentation panel. These tests pin that contract.
function leaf(id: string, views: string[], activeView?: string, size = 500): unknown {
  return { type: 'leaf', data: { id, views, activeView: activeView ?? views[0] }, size };
}
//=============================================================================
function layoutWith(root: unknown, panelIds: string[], activeGroup?: string): SerializedDockview {
  const panels = Object.fromEntries(
    panelIds.map((id) => [id, { id, contentComponent: id, title: id, params: {} }])
  );
  return {
    grid: { root, width: 1000, height: 800, orientation: 'HORIZONTAL' },
    panels,
    activeGroup,
  } as unknown as SerializedDockview;
}
//=============================================================================
describe('sanitizeDockviewForPersistence', () => {
  it('drops the help panel from the persisted panel set', () => {
    const root = {
      type: 'branch',
      data: [leaf('g1', ['files', 'workspace']), leaf('g2', ['console', 'help'], 'help')],
      size: 1000,
    };
    const result = sanitizeDockviewForPersistence(
      layoutWith(root, ['files', 'workspace', 'console', 'help'], 'g2')
    );
    expect(Object.keys(result.panels)).not.toContain('help');
    expect(Object.keys(result.panels)).toEqual(
      expect.arrayContaining(['files', 'workspace', 'console'])
    );
  });

  it('removes help from a mixed group and keeps the other views', () => {
    const root = {
      type: 'branch',
      data: [leaf('g1', ['files']), leaf('g2', ['console', 'help'], 'help')],
      size: 1000,
    };
    const result = sanitizeDockviewForPersistence(
      layoutWith(root, ['files', 'console', 'help'], 'g2')
    );
    const groups = flattenLeaves(result.grid.root as unknown);
    const g2 = groups.find((g) => g.id === 'g2');
    expect(g2?.views).toEqual(['console']);
    // The active view fell back to a surviving one, never the removed 'help'.
    expect(g2?.activeView).toBe('console');
  });

  it('prunes a group whose only view is help', () => {
    const root = {
      type: 'branch',
      data: [leaf('g1', ['console']), leaf('g2', ['help'], 'help')],
      size: 1000,
    };
    const result = sanitizeDockviewForPersistence(layoutWith(root, ['console', 'help'], 'g2'));
    const groups = flattenLeaves(result.grid.root as unknown);
    expect(groups.map((g) => g.id)).toEqual(['g1']);
    // The now-missing active group is reassigned to a surviving one.
    expect(result.activeGroup).toBe('g1');
  });
});
//=============================================================================
interface LeafGroup {
  id: string;
  views: string[];
  activeView?: string;
}
//=============================================================================
function flattenLeaves(node: unknown): LeafGroup[] {
  if (!node || typeof node !== 'object') return [];
  const typed = node as { type?: string; data?: unknown };
  if (typed.type === 'leaf') return [typed.data as LeafGroup];
  if (Array.isArray(typed.data)) {
    return typed.data.flatMap((child) => flattenLeaves(child));
  }
  return [];
}
//=============================================================================
