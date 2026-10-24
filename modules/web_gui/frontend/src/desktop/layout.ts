//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DockviewApi, SerializedDockview } from 'dockview';
import { clearCollapsedGroups, restoreCollapsedGroups, snapshotCollapsedGroups } from './collapse';
import { getPersistentItem, removePersistentItem, setPersistentItem } from './persistentStorage';
//=============================================================================
const LAST = 'nelson-webview.layout.last';
const NAMED = 'nelson-webview.layout.named';
const SCHEMA_KEY = 'nelson-webview.layout.schema';
const SCHEMA_VERSION = 3;
interface LayoutEnvelope {
  schema: 2 | 3;
  dockview: SerializedDockview;
  collapsed: Record<string, number>;
}
//=============================================================================
function ensureLayoutSchema(): void {
  const schema = getPersistentItem(SCHEMA_KEY);
  if (schema === String(SCHEMA_VERSION)) return;
  if (schema !== '2') {
    removePersistentItem(LAST);
    removePersistentItem(NAMED);
  }
  setPersistentItem(SCHEMA_KEY, String(SCHEMA_VERSION));
}
//=============================================================================
function envelope(api: DockviewApi): LayoutEnvelope {
  return {
    schema: SCHEMA_VERSION,
    dockview: sanitizeDockviewForPersistence(api.toJSON()),
    collapsed: snapshotCollapsedGroups(api),
  };
}
//=============================================================================
function isEnvelope(value: unknown): value is LayoutEnvelope {
  if (!value || typeof value !== 'object') return false;
  const candidate = value as Partial<LayoutEnvelope>;
  return (candidate.schema === 2 || candidate.schema === SCHEMA_VERSION) && !!candidate.dockview;
}
//=============================================================================
function resetLayout(api: DockviewApi): void {
  clearCollapsedGroups();
  api.clear();
}
//=============================================================================
// Default: Files and Workspace stacked in a 320 px left column, Command Window
// on the right. Secondary browser tabs are created only when requested.
export function buildDefaultLayout(api: DockviewApi): void {
  resetLayout(api);
  api.addPanel({ id: 'console', component: 'console', title: 'Command Window' });
  api.addPanel({
    id: 'files',
    component: 'files',
    title: 'Files',
    minimumWidth: 260,
    initialWidth: 320,
    position: { referencePanel: 'console', direction: 'left' },
  });
  api.addPanel({
    id: 'workspace',
    component: 'workspace',
    title: 'Workspace',
    minimumWidth: 260,
    position: { referencePanel: 'files', direction: 'below' },
  });
  api.getPanel('files')?.api.setSize({ width: 320 });
  api.getPanel('console')?.api.setActive();
}
//=============================================================================
export function buildTwoColumnsLayout(api: DockviewApi): void {
  buildDefaultLayout(api);
}
//=============================================================================
export function buildThreeColumnsLayout(api: DockviewApi): void {
  resetLayout(api);
  api.addPanel({ id: 'console', component: 'console', title: 'Command Window' });
  api.addPanel({
    id: 'files',
    component: 'files',
    title: 'Files',
    minimumWidth: 260,
    initialWidth: 260,
    position: { referencePanel: 'console', direction: 'left' },
  });
  api.addPanel({
    id: 'workspace',
    component: 'workspace',
    title: 'Workspace',
    minimumWidth: 260,
    initialWidth: 320,
    position: { referencePanel: 'console', direction: 'right' },
  });
  api.addPanel({
    id: 'history',
    component: 'history',
    title: 'Command History',
    minimumWidth: 260,
    position: { referencePanel: 'workspace', direction: 'below' },
  });
  api.getPanel('files')?.api.setSize({ width: 260 });
  api.getPanel('workspace')?.api.setSize({ width: 320 });
  api.getPanel('console')?.api.setActive();
}
//=============================================================================
export function buildCommandOnlyLayout(api: DockviewApi): void {
  resetLayout(api);
  api.addPanel({ id: 'console', component: 'console', title: 'Command Window' });
}
//=============================================================================
// Below this viewport width the side-by-side docked layout is unusable (the
// Command Window gets squeezed to a sliver), so the desktop switches to a
// single full-width group where every panel is a tab.
export const MOBILE_MAX_WIDTH = 760;
export function isMobileViewport(): boolean {
  if (typeof window === 'undefined') return false;
  return window.matchMedia(`(max-width: ${MOBILE_MAX_WIDTH}px)`).matches;
}
//=============================================================================
export function buildMobileLayout(api: DockviewApi): void {
  resetLayout(api);
  api.addPanel({ id: 'console', component: 'console', title: 'Command Window' });
  for (const [id, { component, title }] of [
    ['files', { component: 'files', title: 'Files' }],
    ['workspace', { component: 'workspace', title: 'Workspace' }],
    ['history', { component: 'history', title: 'Command History' }],
  ] as const) {
    api.addPanel({
      id,
      component,
      title,
      position: { referencePanel: 'console', direction: 'within' },
    });
  }
  api.getPanel('console')?.api.setActive();
}
//=============================================================================
export function persistLast(api: DockviewApi): void {
  ensureLayoutSchema();
  try {
    setPersistentItem(LAST, JSON.stringify(envelope(api)));
  } catch {
    // Ignore quota and serialization errors; the current desktop stays usable.
  }
}
//=============================================================================
const PERSISTENT_PANEL_IDS = new Set([
  'console',
  'files',
  'workspace',
  'history',
  'variables',
  'profiler',
  'newsfeed',
  'terminal',
  'help',
]);
//=============================================================================
interface SerializedGroupData {
  id: string;
  views: string[];
  activeView?: string;
}
//=============================================================================
interface SerializedGridNode {
  type: 'leaf' | 'branch';
  data: SerializedGroupData | SerializedGridNode[];
  size?: number;
  visible?: boolean;
}
//=============================================================================
function sanitizeGroup(
  data: SerializedGroupData,
  groupIds: Set<string>
): SerializedGroupData | null {
  const views = data.views.filter((id) => PERSISTENT_PANEL_IDS.has(id));
  if (views.length === 0) return null;
  groupIds.add(data.id);
  return {
    ...data,
    views,
    activeView: data.activeView && views.includes(data.activeView) ? data.activeView : views[0],
  };
}
//=============================================================================
function sanitizeNode(node: SerializedGridNode, groupIds: Set<string>): SerializedGridNode | null {
  if (node.type === 'leaf') {
    const data = sanitizeGroup(node.data as SerializedGroupData, groupIds);
    if (!data) return null;
    // a group hidden behind a maximized figure must come back visible once that figure is gone
    const { visible: _visible, ...leaf } = node;
    return { ...leaf, data };
  }
  const children = (node.data as SerializedGridNode[])
    .map((child) => sanitizeNode(child, groupIds))
    .filter((child): child is SerializedGridNode => child !== null);
  if (children.length === 0) return null;
  if (children.length === 1) return { ...children[0], size: node.size ?? children[0].size };
  return { ...node, data: children };
}
//=============================================================================
export function sanitizeDockviewForPersistence(layout: SerializedDockview): SerializedDockview {
  const result = JSON.parse(JSON.stringify(layout)) as SerializedDockview;
  result.panels = Object.fromEntries(
    Object.entries(result.panels).filter(([id]) => PERSISTENT_PANEL_IDS.has(id))
  );
  const groupIds = new Set<string>();
  const root = sanitizeNode(result.grid.root as SerializedGridNode, groupIds);
  if (root) result.grid.root = root as SerializedDockview['grid']['root'];
  result.floatingGroups = result.floatingGroups
    ?.map((group) => {
      const data = sanitizeGroup(group.data, groupIds);
      return data ? { ...group, data } : null;
    })
    .filter((group): group is NonNullable<typeof group> => group !== null);
  result.popoutGroups = result.popoutGroups
    ?.map((group) => {
      const data = sanitizeGroup(group.data, groupIds);
      return data ? { ...group, data } : null;
    })
    .filter((group): group is NonNullable<typeof group> => group !== null);
  if (result.activeGroup && !groupIds.has(result.activeGroup)) {
    result.activeGroup = groupIds.values().next().value;
  }
  return result;
}
//=============================================================================
export function restoreLast(api: DockviewApi): boolean {
  ensureLayoutSchema();
  const raw = getPersistentItem(LAST);
  if (!raw) return false;
  try {
    const saved: unknown = JSON.parse(raw);
    if (!isEnvelope(saved)) return false;
    api.fromJSON(sanitizeDockviewForPersistence(saved.dockview));
    if (!api.getPanel('console')) return false;
    restoreCollapsedGroups(api, saved.collapsed);
    return true;
  } catch {
    return false;
  }
}
//=============================================================================
export function listNamedLayouts(): string[] {
  ensureLayoutSchema();
  return Object.keys(readNamed()).sort((left, right) => left.localeCompare(right));
}
//=============================================================================
export function saveNamedLayout(name: string, api: DockviewApi): void {
  ensureLayoutSchema();
  const all = readNamed();
  all[name] = envelope(api);
  setPersistentItem(NAMED, JSON.stringify(all));
}
//=============================================================================
export function applyNamedLayout(name: string, api: DockviewApi): boolean {
  ensureLayoutSchema();
  const saved = readNamed()[name];
  if (!isEnvelope(saved)) return false;
  try {
    api.fromJSON(sanitizeDockviewForPersistence(saved.dockview));
    restoreCollapsedGroups(api, saved.collapsed);
    return true;
  } catch {
    return false;
  }
}
//=============================================================================
export function removeNamedLayout(name: string): void {
  ensureLayoutSchema();
  const all = readNamed();
  delete all[name];
  setPersistentItem(NAMED, JSON.stringify(all));
}
//=============================================================================
function readNamed(): Record<string, LayoutEnvelope> {
  try {
    return JSON.parse(getPersistentItem(NAMED) ?? '{}') as Record<string, LayoutEnvelope>;
  } catch {
    return {};
  }
}
//=============================================================================
export const layoutStorageKeys = {
  last: LAST,
  named: NAMED,
  schema: SCHEMA_KEY,
} as const;
//=============================================================================
