//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { DockviewApi, IDockviewPanel } from 'dockview';
import { clearDetached, markDetached } from './figureDetach';
import { addRecentFile } from './recentFiles';
import { selectVariable } from './variableHost';
import { getDesktopCapabilities } from './capabilities';
import { isMobileViewport } from './layout';
import { rpc } from '../rpc/client';
//=============================================================================
export const FIGURE_CLOSED_EVENT = 'nelson-webview.figure-closed';
let api: DockviewApi | null = null;
let shellSeq = 0;
let dockActivationSerial = 0;
const listeners = new Set<() => void>();
const subscriptions: { dispose(): void }[] = [];
type OpenPanelOptions = {
  focus?: boolean;
};
//=============================================================================
function notify(): void {
  listeners.forEach((listener) => listener());
}
//=============================================================================
export function subscribeDock(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}
//=============================================================================
export function getDockActivationSerial(): number {
  return dockActivationSerial;
}
//=============================================================================
export function setDockApi(next: DockviewApi): void {
  subscriptions.splice(0).forEach((subscription) => subscription.dispose());
  api = next;
  subscriptions.push(
    next.onDidActivePanelChange(() => {
      dockActivationSerial += 1;
      notify();
    }),
    next.onDidAddPanel(notify),
    next.onDidRemovePanel(notify),
    next.onDidMovePanel(notify)
  );
  notify();
}
//=============================================================================
export function getDockApi(): DockviewApi | null {
  return api;
}
//=============================================================================
function focusPanel(panel: IDockviewPanel): void {
  // A panel can already be the selected tab of an inactive group. Activate the
  // group explicitly first: relying on the panel's selected-tab state alone
  // leaves Dockview's global activePanel pointing at the previous group under
  // concurrent layout work.
  panel.group.api.setActive();
  panel.api.setActive();
  panel.focus();
  // Dockview may consider a just-added tab active before emitting its active
  // change event. Notify the shell explicitly so rail state never lags behind.
  notify();
}
//=============================================================================
function focusPanelWhenReady(panel: IDockviewPanel): void {
  focusPanel(panel);
  // addPanel can return before Dockview has committed the new group/tab to
  // the DOM. A second activation on the next frame closes that race without
  // polling or stealing focus from a later user action.
  window.requestAnimationFrame(() => {
    if (api?.getPanel(panel.id) === panel) focusPanel(panel);
  });
}
//=============================================================================
function focusOrAdd(
  id: string,
  component: string,
  title: string,
  params: Record<string, unknown>,
  position?: Parameters<DockviewApi['addPanel']>[0]['position'],
  options: OpenPanelOptions = {}
): void {
  if (!api) return;
  const shouldFocus = options.focus ?? true;
  const existing = api.getPanel(id);
  if (existing) {
    if (shouldFocus) focusPanel(existing);
    return;
  }
  const previousActive = shouldFocus ? null : api.activePanel;
  const panel = api.addPanel({ id, component, title, params, position });
  if (shouldFocus) {
    focusPanelWhenReady(panel);
  } else if (previousActive && api.getPanel(previousActive.id) === previousActive) {
    focusPanel(previousActive);
  } else {
    notify();
  }
}
//=============================================================================
export function openVariable(name: string): void {
  selectVariable(name);
  ensureCorePanel('variables');
}
//=============================================================================
export function openFigure(handle: number, title: string, options: OpenPanelOptions = {}): void {
  focusOrAdd(`fig:${handle}`, 'figure', title, { handle, title }, undefined, options);
}
//=============================================================================
export function popOutFigure(handle: number, title: string): void {
  const url = new URL(window.location.href);
  url.searchParams.set('figure', String(handle));
  const win = window.open(
    `${url.pathname}${url.search}`,
    `nelson-figure-${handle}`,
    'width=840,height=680,menubar=no,toolbar=no'
  );
  if (!win) return;
  markDetached(handle, title);
  const watch = window.setInterval(() => {
    if (!win.closed) return;
    window.clearInterval(watch);
    clearDetached(handle);
    openFigure(handle, title);
  }, 300);
  closeFigurePanel(handle);
}
//=============================================================================
export function closeFigurePanel(handle: number): void {
  const panel = api?.getPanel(`fig:${handle}`);
  if (panel && api) api.removePanel(panel);
}
//=============================================================================
export function updateFigurePanelTitle(handle: number, title: string): void {
  const panel = api?.getPanel(`fig:${handle}`);
  if (panel) {
    if (panel.title !== title) panel.api.setTitle(title);
    if (panel.params?.title !== title) panel.api.updateParameters({ title });
  }
}
//=============================================================================
// Figures that currently live in their own native window (undocked). The
// desktop must not re-dock these during figure.list reconciliation.
const figuresInWindow = new Set<number>();
export function isFigureInWindow(handle: number): boolean {
  return figuresInWindow.has(handle);
}
//=============================================================================
export function markFigureInWindow(handle: number): void {
  figuresInWindow.add(handle);
}
//=============================================================================
export function unmarkFigureInWindow(handle: number): void {
  figuresInWindow.delete(handle);
}
//=============================================================================
// Open (or focus) a figure in a separate native window. No transient docked panel
// is opened while it launches, so the figure never flashes inside the desktop. If
// the backend cannot open the window it pushes figure.window {open:false} and the
// rpc failure path below re-docks it.
export function openFigureInSeparateWindow(handle: number, title: string): void {
  markFigureInWindow(handle);
  closeFigurePanel(handle);
  void rpc('figure.undock', { handle }).catch(() => {
    unmarkFigureInWindow(handle);
    openFigure(handle, title);
  });
}
//=============================================================================
// The backend confirms the native window close via figure.window.
export function dockFigureFromWindow(handle: number, title: string): void {
  unmarkFigureInWindow(handle);
  openFigure(handle, title);
  void rpc('figure.dock', { handle }).catch(() => undefined);
}
//=============================================================================
// In the packaged WebView desktop the figure becomes a native window; elsewhere
// a browser popup, since a page cannot open a native OS window.
export function undockFigure(handle: number, title: string): void {
  if (getDesktopCapabilities().launchMode === 'webview') {
    openFigureInSeparateWindow(handle, title);
  } else {
    popOutFigure(handle, title);
  }
}
//=============================================================================
export async function closeDesktopPanel(panel: IDockviewPanel): Promise<void> {
  if (!panel.id.startsWith('fig:')) {
    panel.api.close();
    return;
  }
  const handle = Number(panel.id.slice(4));
  if (!Number.isFinite(handle)) {
    panel.api.close();
    return;
  }
  try {
    await rpc('figure.close', { handle });
  } catch {
    // The engine may already have closed the figure through a command. The tab
    // is stale in that case and should still disappear.
  }
  window.dispatchEvent(new CustomEvent(FIGURE_CLOSED_EVENT, { detail: { handle } }));
  if (api?.getPanel(panel.id) === panel) panel.api.close();
}
//=============================================================================
// One tab per Notebook file: a .nlx focuses its existing tab or adds one titled
// by file name. No path opens a fresh untitled document.
let notebookSeq = 0;
export function openNotebook(path?: string): void {
  if (!api) return;
  // Documents stack as tabs in one group: the group that already holds a Live
  // Script, else the console group (next to the Command Window), not the active
  // group (e.g. the Files sidebar, which would cover the file list).
  const existing = api.panels.find((p) => p.id.startsWith('notebook:'));
  const position: Parameters<DockviewApi['addPanel']>[0]['position'] = existing
    ? { referencePanel: existing.id, direction: 'within' }
    : api.getPanel('console')
    ? { referencePanel: 'console', direction: 'within' }
    : undefined;
  if (path) {
    const base = path.split(/[\\/]/).pop() ?? path;
    addRecentFile(path);
    focusOrAdd(`notebook:${path}`, 'notebook', base || 'document.nlx', { path }, position);
  } else {
    notebookSeq += 1;
    focusOrAdd(
      `notebook:untitled:${notebookSeq}`,
      'notebook',
      `Notebook ${notebookSeq}`,
      {},
      position
    );
  }
}
//=============================================================================
export function openEditor(path: string, line?: number): void {
  // A Notebook document goes to the Notebook editor, not the plain text editor.
  if (/\.nlx$/i.test(path)) {
    openNotebook(path);
    return;
  }
  const base = path.split(/[\\/]/).pop() ?? path;
  if (path) addRecentFile(path);
  focusOrAdd(`editor:${path}`, 'editor', base || 'untitled.m', { path });
  if (line && line > 0) {
    setTimeout(
      () =>
        window.dispatchEvent(
          new CustomEvent('nelson-webview.editor-goto', { detail: { path, line } })
        ),
      0
    );
  }
}
//=============================================================================
export function openShell(): void {
  if (!api) return;
  const id = `shell:${++shellSeq}`;
  const panel = api.addPanel({ id, component: 'shell', title: `Terminal ${shellSeq}` });
  focusPanel(panel);
}
//=============================================================================
const CORE_PANELS: Record<string, { component: string; title: string }> = {
  console: { component: 'console', title: 'Command Window' },
  files: { component: 'files', title: 'Files' },
  workspace: { component: 'workspace', title: 'Workspace' },
  history: { component: 'history', title: 'Command History' },
  newsfeed: { component: 'newsfeed', title: 'News feed' },
  variables: { component: 'variables', title: 'Variables Editor' },
  help: { component: 'help', title: 'Documentation' },
  examples: { component: 'examples', title: 'Examples' },
  notebook: { component: 'notebook', title: 'Notebook' },
  packages: { component: 'packages', title: 'Package Manager' },
  profiler: { component: 'profiler', title: 'Profiler' },
  diagnostics: { component: 'diagnostics', title: 'Diagnostics' },
  terminal: { component: 'shell', title: 'Terminal' },
};
//=============================================================================
function corePosition(id: string): Parameters<DockviewApi['addPanel']>[0]['position'] | undefined {
  if (!api) return undefined;
  if (isMobileViewport()) {
    // Mobile: every panel joins the single full-width group as a tab instead
    // of docking beside the others (which would squeeze them off-screen).
    const ref = api.getPanel('console') ?? api.panels.find((p) => p.id !== id);
    if (ref && ref.id !== id) return { referencePanel: ref.id, direction: 'within' };
    return undefined;
  }
  if (id === 'files') {
    if (api.getPanel('workspace')) return { referencePanel: 'workspace', direction: 'above' };
    if (api.getPanel('console')) return { referencePanel: 'console', direction: 'left' };
  }
  if (id === 'workspace') {
    if (api.getPanel('files')) return { referencePanel: 'files', direction: 'below' };
    if (api.getPanel('console')) return { referencePanel: 'console', direction: 'left' };
  }
  if (['history', 'variables', 'profiler', 'newsfeed', 'diagnostics', 'terminal'].includes(id)) {
    if (!api.getPanel('workspace')) ensureCorePanel('workspace');
    return api.getPanel('workspace')
      ? { referencePanel: 'workspace', direction: 'within' }
      : undefined;
  }
  if (id === 'console' && api.getPanel('files')) {
    return { referencePanel: 'files', direction: 'right' };
  }
  if (['help', 'examples', 'packages', 'notebook'].includes(id) && api.getPanel('console')) {
    return { referencePanel: 'console', direction: 'within' };
  }
  return undefined;
}
//=============================================================================
export function openHelp(topic?: string): void {
  ensureCorePanel('help');
  if (topic) {
    setTimeout(
      () =>
        window.dispatchEvent(new CustomEvent('nelson-webview.help-topic', { detail: { topic } })),
      0
    );
  }
}
//=============================================================================
export function ensureCorePanel(id: string): void {
  if (!api) return;
  const existing = api.getPanel(id);
  if (existing) {
    focusPanel(existing);
    return;
  }
  const spec = CORE_PANELS[id];
  if (!spec) return;
  const panel = api.addPanel({
    id,
    component: spec.component,
    title: spec.title,
    minimumWidth: id === 'files' || id === 'workspace' ? 260 : undefined,
    initialWidth: id === 'files' ? 320 : undefined,
    position: corePosition(id),
  });
  focusPanel(panel);
  window.requestAnimationFrame(() => {
    if (api?.getPanel(id) === panel) focusPanel(panel);
  });
}
//=============================================================================
export type PanelToggleTransition = 'open' | 'focus' | 'close';
export function panelToggleTransition(visible: boolean, active: boolean): PanelToggleTransition {
  if (!visible) return 'open';
  return active ? 'close' : 'focus';
}
//=============================================================================
export function toggleCorePanel(id: string): void {
  if (!api) return;
  const panel = api.getPanel(id);
  const transition = panelToggleTransition(!!panel, api.activePanel?.id === id);
  switch (transition) {
    case 'open':
      ensureCorePanel(id);
      break;
    case 'close':
      panel?.api.close();
      break;
    case 'focus':
      if (panel) focusPanel(panel);
      break;
  }
}
//=============================================================================
export function panelState(id: string): { visible: boolean; active: boolean } {
  return { visible: !!api?.getPanel(id), active: api?.activePanel?.id === id };
}
//=============================================================================
export function listOpenPanels(): { id: string; title: string }[] {
  return (api?.panels ?? []).map((panel) => ({ id: panel.id, title: panel.title ?? panel.id }));
}
//=============================================================================
export function focusPanelById(id: string): void {
  const panel = api?.getPanel(id);
  if (panel) focusPanel(panel);
}
//=============================================================================
export function tilePanels(): void {
  if (!api || api.panels.length < 2) return;
  const panels = [...api.panels];
  for (let index = 1; index < panels.length; index++) {
    panels[index].api.moveTo({ group: panels[index - 1].group, position: 'right' });
  }
}
//=============================================================================
