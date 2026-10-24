//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DockviewApi, DockviewGroupPanel, IDockviewPanel } from 'dockview';
import { expandGroup, isGroupCollapsed, toggleGroupCollapsed } from './collapse';
import { closeDesktopPanel } from './desktop';
//=============================================================================
const previousGroups = new Map<string, string>();
export type PanelActionItem =
  | { separator: true }
  | { label: string; onClick: () => void; disabled?: boolean; danger?: boolean };
export function panelCanClose(panel: IDockviewPanel): boolean {
  return panel.id !== 'console';
}
//=============================================================================
export function togglePanelFloating(panel: IDockviewPanel, api: DockviewApi): void {
  if (panel.api.location.type === 'floating') {
    const remembered = previousGroups.get(panel.id);
    const target =
      api.groups.find((group) => group.id === remembered && group.api.location.type === 'grid') ??
      api.groups.find((group) => group.api.location.type === 'grid' && group.id !== panel.group.id);
    if (target) panel.api.moveTo({ group: target });
    return;
  }
  previousGroups.set(panel.id, panel.group.id);
  if (isGroupCollapsed(panel.group)) expandGroup(panel.group);
  api.addFloatingGroup(panel, { x: 60, y: 60, width: 640, height: 520 });
}
//=============================================================================
function moveTarget(
  panel: IDockviewPanel,
  api: DockviewApi,
  position: 'left' | 'right' | 'bottom'
): DockviewGroupPanel | undefined {
  const groups = api.groups.filter(
    (group) => group.id !== panel.group.id && group.api.location.type === 'grid'
  );
  if (!groups.length) return undefined;
  const edge = (group: DockviewGroupPanel): number => {
    const bounds = group.element.getBoundingClientRect();
    if (position === 'left') return bounds.left;
    if (position === 'right') return -bounds.right;
    return -bounds.bottom;
  };
  return groups.sort((left, right) => edge(left) - edge(right))[0];
}
//=============================================================================
export function movePanel(
  panel: IDockviewPanel,
  api: DockviewApi,
  position: 'left' | 'right' | 'bottom'
): void {
  if (panel.api.location.type === 'floating') return;
  const target = moveTarget(panel, api, position);
  if (target) panel.api.moveTo({ group: target, position });
}
//=============================================================================
export function panelActionItems(panel: IDockviewPanel, api: DockviewApi): PanelActionItem[] {
  const floating = panel.api.location.type === 'floating';
  const items: PanelActionItem[] = [];
  if (!floating) {
    items.push({
      label: isGroupCollapsed(panel.group) ? 'Expand panel' : 'Collapse panel',
      onClick: () => toggleGroupCollapsed(panel.group),
    });
  }
  items.push({
    label: floating ? 'Dock panel' : 'Float panel',
    onClick: () => togglePanelFloating(panel, api),
  });
  if (!floating) {
    items.push(
      { label: 'Move Left', onClick: () => movePanel(panel, api, 'left') },
      { label: 'Move Right', onClick: () => movePanel(panel, api, 'right') },
      { label: 'Move Bottom', onClick: () => movePanel(panel, api, 'bottom') }
    );
  }
  if (panelCanClose(panel)) {
    items.push(
      { separator: true },
      { label: 'Close panel', onClick: () => void closeDesktopPanel(panel) }
    );
  }
  return items;
}
//=============================================================================
