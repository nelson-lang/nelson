//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { DockviewReact, type DockviewReadyEvent, type IDockviewPanelProps } from 'dockview';
import { lazy, Suspense, useCallback, useEffect, useState } from 'react';
import { ConsolePanel } from './panels/ConsolePanel';
import { WorkspacePanel } from './panels/WorkspacePanel';
import { HistoryPanel } from './panels/HistoryPanel';
import { NewsfeedPanel } from './panels/NewsfeedPanel';
import { FilesPanel } from './panels/FilesPanel';
import { VariableEditorPanel } from './panels/VariableEditorPanel';
import { FigurePanel } from './panels/FigurePanel';
import { DesktopTab } from './panels/DesktopTab';
import { GroupCollapseAction } from './panels/GroupCollapseAction';
import { forgetGroup } from './desktop/collapse';
import { ShellTerminalPanel } from './panels/ShellTerminalPanel';
import { HelpPanel } from './panels/HelpPanel';
import { ProfilerPanel } from './panels/ProfilerPanel';
import { DiagnosticsPanel } from './panels/DiagnosticsPanel';
import { VariablesEditorHost } from './panels/VariablesEditorHost';
import { PanelHeaderActions } from './panels/PanelHeaderActions';
import { MenuBar } from './components/MenuBar';
import { Sidebar } from './components/Sidebar';
import { StatusBar } from './components/StatusBar';
import { DesktopToolbar } from './components/DesktopToolbar';
import { DesktopFilePickerHost } from './components/DesktopFilePickerHost';
import { DialogHost } from './components/DialogHost';
import { ProgressDialogHost } from './components/ProgressDialogHost';
import { PromptHost } from './components/PromptHost';
import { PreferencesHost } from './components/PreferencesHost';
import { AboutHost } from './components/AboutHost';
import { SetPathHost } from './components/SetPathHost';
import { ImportDataHost } from './components/ImportDataHost';
import { CommandPalette, OPEN_PALETTE_EVENT } from './components/CommandPalette';
import { PlotGalleryHost } from './components/PlotGalleryHost';
import { LocalFileDropHost } from './components/LocalFileDropHost';
import { AudioUnlockHost } from './components/AudioUnlockHost';
import { ensureCorePanel, setDockApi } from './desktop/desktop';
import { isEngineBusy, onEngineReady, runCommand } from './desktop/engineSession';
import { installFigureLifecycle } from './desktop/figureLifecycle';
import {
  buildDefaultLayout,
  buildMobileLayout,
  isMobileViewport,
  MOBILE_MAX_WIDTH,
  persistLast,
  restoreLast,
} from './desktop/layout';
import {
  dockviewThemeClass,
  getThemePref,
  resolveScheme,
  setActiveScheme,
  setThemePref,
  THEME_CHANGE_EVENT,
  type ThemePref,
} from './desktop/theme';
import { isBackendConnected, onPush, rpc } from './rpc/client';
import type { ClientRoleResult } from './rpc/types';
import { getDockApi } from './desktop/desktop';
import { hasPrimaryModifier } from './desktop/platform';
import { useShellVisibility } from './desktop/shellState';
import { refreshCurrentDirectory } from './desktop/currentDirectory';
import {
  getDesktopCapabilities,
  refreshDesktopCapabilities,
  setLaunchMode,
  setPackageUpdates,
} from './desktop/capabilities';
import { showStatusMessage } from './desktop/statusMessage';
import { installNelsonDesktopMessageBridge } from './desktop/externalMessages';
import { notifyStartupUrlDesktopReady } from './desktop/startupUrlActions';
//=============================================================================
const EditorPanel = lazy(() =>
  import('./panels/EditorPanel').then((module) => ({ default: module.EditorPanel }))
);
//=============================================================================
const ExamplesPanel = lazy(() =>
  import('./panels/ExamplesPanel').then((module) => ({ default: module.ExamplesPanel }))
);
//=============================================================================
const NotebookPanel = lazy(() =>
  import('./notebook/NotebookPanel').then((module) => ({ default: module.NotebookPanel }))
);
// Notebook is intentionally lazy so the initial desktop stays small.  The
// browser has a second cost that the native WebView does not have, though: the
// first tab must download and parse both the Notebook and Monaco chunks.
// Warm those chunks only after the first screen is idle.  This keeps the
// critical Wasm startup path unchanged while making the first Notebook open
// behave like the already-local WebView bundle.  Respect Save-Data and slow
// connections, where competing with the runtime download would be harmful.
function scheduleNotebookPrefetch(): () => void {
  if (typeof window === 'undefined' || typeof navigator === 'undefined') return () => {};
  const connection = (
    navigator as Navigator & {
      connection?: { saveData?: boolean; effectiveType?: string };
    }
  ).connection;
  if (
    connection?.saveData ||
    connection?.effectiveType === 'slow-2g' ||
    connection?.effectiveType === '2g'
  ) {
    return () => {};
  }
  let cancelled = false;
  const warm = () => {
    if (!cancelled) void import('./notebook/NotebookPanel').catch(() => undefined);
  };
  const idle = (window as Window & { requestIdleCallback?: (cb: () => void) => number })
    .requestIdleCallback;
  if (idle) {
    const handle = idle(warm);
    return () => {
      cancelled = true;
      const cancel = (window as Window & { cancelIdleCallback?: (id: number) => void })
        .cancelIdleCallback;
      cancel?.(handle);
    };
  }
  const handle = window.setTimeout(warm, 2500);
  return () => {
    cancelled = true;
    window.clearTimeout(handle);
  };
}
//=============================================================================
const PackageManagerPanel = lazy(() =>
  import('./panels/PackageManagerPanel').then((module) => ({ default: module.PackageManagerPanel }))
);
//=============================================================================
const components = {
  console: (props: IDockviewPanelProps) => <ConsolePanel panelApi={props.api} />,
  workspace: WorkspacePanel,
  history: HistoryPanel,
  newsfeed: NewsfeedPanel,
  files: FilesPanel,
  variable: (props: IDockviewPanelProps<{ name: string }>) => (
    <VariableEditorPanel name={props.params.name} panelApi={props.api} />
  ),
  figure: (props: IDockviewPanelProps<{ handle: number; title: string }>) => (
    <FigurePanel handle={props.params.handle} title={props.params.title} panelApi={props.api} />
  ),
  editor: (props: IDockviewPanelProps<{ path: string }>) => (
    <Suspense fallback={<div className="browser__empty">Loading editor…</div>}>
      <EditorPanel path={props.params.path} />
    </Suspense>
  ),
  shell: ShellTerminalPanel,
  help: HelpPanel,
  examples: () => (
    <Suspense fallback={<div className="browser__empty">Loading examples…</div>}>
      <ExamplesPanel />
    </Suspense>
  ),
  notebook: (props: IDockviewPanelProps) => (
    <Suspense fallback={<div className="browser__empty">Loading Notebook…</div>}>
      <NotebookPanel panelApi={props.api} path={props.params.path as string | undefined} />
    </Suspense>
  ),
  packages: () => (
    <Suspense fallback={<div className="browser__empty">Loading package manager…</div>}>
      <PackageManagerPanel />
    </Suspense>
  ),
  diagnostics: DiagnosticsPanel,
  profiler: ProfilerPanel,
  variables: VariablesEditorHost,
};
//=============================================================================
function focusZone(zone: HTMLElement): void {
  if (zone.dataset.focusZone === 'dock') {
    getDockApi()?.focus();
    return;
  }
  const target = zone.matches('button, input, select, [tabindex]')
    ? zone
    : zone.querySelector<HTMLElement>(
        'button:not(:disabled), input:not(:disabled), select:not(:disabled), [tabindex]'
      );
  target?.focus();
}
//=============================================================================
function cycleFocusZones(reverse: boolean): void {
  const zones = [...document.querySelectorAll<HTMLElement>('[data-focus-zone]')].filter(
    (zone) => zone.getClientRects().length > 0
  );
  if (!zones.length) return;
  const current = (document.activeElement as HTMLElement | null)?.closest<HTMLElement>(
    '[data-focus-zone]'
  );
  const index = current ? zones.indexOf(current) : -1;
  const delta = reverse ? -1 : 1;
  focusZone(zones[(index + delta + zones.length) % zones.length]);
}
//=============================================================================
export function App(): JSX.Element {
  const [theme, setTheme] = useState<ThemePref>(getThemePref());
  const [, bumpScheme] = useState(0);
  const scheme = resolveScheme(theme);
  const [disconnected, setDisconnected] = useState(false);
  const shellVisibility = useShellVisibility();

  useEffect(() => installNelsonDesktopMessageBridge(), []);

  useEffect(() => scheduleNotebookPrefetch(), []);

  // A browser popup carries the figure handle in the URL ('?figure=N'); a native
  // figure window learns it from client.role. 'pending' holds the first render only
  // when there is no URL hint.
  const [standaloneFigure, setStandaloneFigure] = useState<number | 'desktop' | 'pending'>(() => {
    const p = new URLSearchParams(window.location.search).get('figure');
    return p !== null && p !== '' ? Number(p) : 'pending';
  });
  useEffect(() => {
    const p = new URLSearchParams(window.location.search).get('figure');
    if (p !== null && p !== '') return;
    let alive = true;
    // Optimistically unblock the first render as the desktop if the backend
    // is slow, but keep the real client.role answer authoritative: on a busy
    // machine a native figure window must not get stuck showing the desktop
    // because its role reply arrived after the timeout.
    const optimisticTimer = window.setTimeout(() => {
      if (alive) setStandaloneFigure((prev) => (prev === 'pending' ? 'desktop' : prev));
    }, 2500);
    rpc<ClientRoleResult>('client.role')
      .then((r) => {
        if (!alive) return;
        window.clearTimeout(optimisticTimer);
        // Authoritative launch mode from the startup RPC: the polled app.capabilities
        // can miss it across the desktop reload, and a new figure must open in its own
        // native window on the WebView desktop.
        setLaunchMode(r.launchMode);
        const resolved = r.role === 'figure' && r.handle != null ? r.handle : 'desktop';
        setStandaloneFigure((prev) => (prev === 'pending' || prev === 'desktop' ? resolved : prev));
      })
      .catch(() => {
        if (!alive) return;
        window.clearTimeout(optimisticTimer);
        setStandaloneFigure((prev) => (prev === 'pending' ? 'desktop' : prev));
      });
    return () => {
      alive = false;
      window.clearTimeout(optimisticTimer);
    };
  }, []);

  // A native figure window opens after the figure (and a possible Name) was
  // created, so the figure.title push already went by: fetch the current title
  // once the role is known instead of staying on the "Figure N" fallback.
  const [standaloneTitle, setStandaloneTitle] = useState<string | null>(null);
  useEffect(() => {
    if (typeof standaloneFigure !== 'number') return;
    let alive = true;
    rpc<{ figures?: { handle: number; title?: string }[] }>('figure.list')
      .then((r) => {
        if (!alive) return;
        const entry = (r.figures ?? []).find((f) => f.handle === standaloneFigure);
        if (entry?.title) setStandaloneTitle(entry.title);
      })
      .catch(() => undefined);
    return () => {
      alive = false;
    };
  }, [standaloneFigure]);

  useEffect(() => {
    let active = true;
    const refreshWhenReady = async () => {
      for (let attempt = 0; active && attempt < 60; attempt++) {
        if (await refreshDesktopCapabilities()) {
          const mode = getDesktopCapabilities().launchMode;
          if (!isBackendConnected() || (mode && mode !== 'none')) return;
        }
        await new Promise((resolve) => setTimeout(resolve, 250));
      }
    };
    void refreshWhenReady();
    const offReady = onEngineReady(() => void refreshDesktopCapabilities());
    const offUpdates = onPush('packages.updates', (p) =>
      setPackageUpdates((p as { count?: unknown } | null)?.count)
    );
    return () => {
      active = false;
      offReady();
      offUpdates();
    };
  }, []);

  useEffect(() => {
    const onError = () => showStatusMessage('Could not save desktop preferences.');
    window.addEventListener('nelson-webview.prefs-error', onError);
    return () => window.removeEventListener('nelson-webview.prefs-error', onError);
  }, []);

  useEffect(() => {
    setActiveScheme(scheme);
  }, [scheme]);

  useEffect(() => {
    if (theme !== 'system') return;
    const mm = window.matchMedia('(prefers-color-scheme: dark)');
    const onChange = () => bumpScheme((n) => n + 1);
    mm.addEventListener('change', onChange);
    return () => mm.removeEventListener('change', onChange);
  }, [theme]);

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (hasPrimaryModifier(e) && e.shiftKey && e.key.toLowerCase() === 'p') {
        e.preventDefault();
        window.dispatchEvent(new CustomEvent(OPEN_PALETTE_EVENT));
        return;
      }
      if (hasPrimaryModifier(e) && e.key === 'F6') {
        e.preventDefault();
        cycleFocusZones(e.shiftKey);
        return;
      }
      if (
        (e.key === 'c' || e.key === 'C') &&
        e.ctrlKey &&
        !e.altKey &&
        !e.metaKey &&
        isEngineBusy() &&
        (window.getSelection()?.toString() ?? '').length === 0
      ) {
        e.preventDefault();
        e.stopImmediatePropagation();
        void rpc('term.interrupt', { busy: true }).catch(() => undefined);
        return;
      }
      if (!hasPrimaryModifier(e) || e.altKey) return;
      const panel: Record<string, string> = {
        '1': 'history',
        '2': 'files',
        '3': 'workspace',
        '4': 'variables',
        '5': 'profiler',
        '6': 'newsfeed',
      };
      if (panel[e.key]) {
        e.preventDefault();
        ensureCorePanel(panel[e.key]);
      } else if (e.key === 'l' && !e.shiftKey) {
        // Clear console only when the console has focus (Ctrl+L elsewhere is
        // the browser's address bar).
        const inConsole = document.activeElement?.closest('.console-panel');
        if (inConsole) {
          e.preventDefault();
          void runCommand('clc');
        }
      }
    };
    window.addEventListener('keydown', onKey);
    return () => {
      window.removeEventListener('keydown', onKey);
    };
  }, []);

  // Heartbeat: when heartbeats stop (window closed) the server watchdog injects
  // `exit`. A few missed beats mean the engine is gone and flip the desktop to a
  // "disconnected" state.
  useEffect(() => {
    let active = true;
    let fails = 0;
    const beat = async (): Promise<boolean> => {
      try {
        await Promise.race([
          rpc('app.heartbeat'),
          new Promise((_, reject) => setTimeout(() => reject(new Error('timeout')), 1500)),
        ]);
        return true;
      } catch {
        return false;
      }
    };
    (async () => {
      while (active) {
        if (await beat()) {
          fails = 0;
          setDisconnected(false);
        } else if (++fails >= 3) {
          setDisconnected(true);
        }
        await new Promise((r) => setTimeout(r, 1000));
      }
    })();
    return () => {
      active = false;
    };
  }, []);

  const onTheme = useCallback((t: ThemePref) => {
    setThemePref(t);
    setTheme(t);
  }, []);

  useEffect(() => {
    const changeTheme = (event: Event) => onTheme((event as CustomEvent<ThemePref>).detail);
    window.addEventListener(THEME_CHANGE_EVENT, changeTheme);
    return () => window.removeEventListener(THEME_CHANGE_EVENT, changeTheme);
  }, [onTheme]);

  const onReady = useCallback((event: DockviewReadyEvent) => {
    const api = event.api;
    setDockApi(api);

    // Phones get a single full-width tabbed group; wider screens keep the
    // docked desktop layout. The two are swapped live at the breakpoint.
    let mobileActive = isMobileViewport();
    if (mobileActive) {
      buildMobileLayout(api);
    } else if (!restoreLast(api)) {
      buildDefaultLayout(api);
    }

    // Persist only the desktop layout, so switching to the mobile layout does
    // not overwrite the arrangement we restore when returning to a wide screen.
    const persist = () => {
      if (!mobileActive) persistLast(api);
    };
    api.onDidLayoutChange(persist);

    const mobileQuery = window.matchMedia(`(max-width: ${MOBILE_MAX_WIDTH}px)`);
    const onBreakpoint = () => {
      if (mobileQuery.matches === mobileActive) return;
      mobileActive = mobileQuery.matches;
      if (mobileActive) {
        buildMobileLayout(api);
      } else if (!restoreLast(api)) {
        buildDefaultLayout(api);
      }
    };
    mobileQuery.addEventListener('change', onBreakpoint);

    // Drop the collapsed state of groups that no longer exist (ids get reused).
    api.onDidRemoveGroup((group) => forgetGroup(group.id));

    const uninstallFigures = installFigureLifecycle(api);
    // An evaluation just finished; the real engine has no change signals for
    // these, so refresh the current directory and nudge the browsers.
    const offReady = onEngineReady(() => {
      void refreshCurrentDirectory().catch(() => undefined);
      window.__nelsonPush?.('workspace.changed', '{}');
      window.__nelsonPush?.('history.changed', '{}');
    });
    void rpc('client.ready')
      .catch(() => undefined)
      .finally(() => notifyStartupUrlDesktopReady());

    return () => {
      mobileQuery.removeEventListener('change', onBreakpoint);
      uninstallFigures();
      offReady();
    };
  }, []);

  useEffect(() => {
    if (theme !== 'system') return;
    const mq = window.matchMedia('(prefers-color-scheme: dark)');
    const handler = () => setTheme('system');
    mq.addEventListener('change', handler);
    return () => mq.removeEventListener('change', handler);
  }, [theme]);

  // Standalone figure window: opened with window.open('?figure=N') and renders just
  // that figure. Diagnostic mode (?renderall=1) fetches the draw list of every open
  // figure without mounting panels, so a verification sweep does not render dozens of
  // canvases. The server redirects to /index.html and drops the query string, so the
  // hash or a localStorage flag is accepted as well.
  const renderAll =
    new URLSearchParams(window.location.search).get('renderall') ||
    (window.location.hash.includes('renderall') ? '1' : '') ||
    (() => {
      try {
        return localStorage.getItem('nelson-webview.renderall') ?? '';
      } catch {
        return '';
      }
    })();
  if (renderAll) {
    return <RenderAllProbe />;
  }

  if (standaloneFigure === 'pending') {
    return <div className={`nelson-shell nelson-shell--${scheme}`} />;
  }
  if (typeof standaloneFigure === 'number') {
    return (
      <div className={`nelson-shell nelson-shell--${scheme} nelson-shell--standalone`}>
        <FigurePanel
          handle={standaloneFigure}
          title={standaloneTitle ?? `Figure ${standaloneFigure + 1}`}
          standalone
        />
        <PromptHost />
        <AudioUnlockHost />
      </div>
    );
  }

  return (
    <div className={`nelson-shell nelson-shell--${scheme}`}>
      <MenuBar />
      {shellVisibility.toolbar ? <DesktopToolbar /> : null}
      <div className="nelson-body">
        {shellVisibility.rail ? <Sidebar /> : null}
        <div
          className="nelson-dock"
          role="main"
          aria-label="Desktop panels"
          data-focus-zone="dock"
          tabIndex={-1}
        >
          <DockviewReact
            components={components}
            defaultTabComponent={DesktopTab}
            prefixHeaderActionsComponent={GroupCollapseAction}
            rightHeaderActionsComponent={PanelHeaderActions}
            onReady={onReady}
            className={dockviewThemeClass(scheme)}
          />
        </div>
      </div>
      {shellVisibility.status ? <StatusBar /> : null}
      <DialogHost />
      <ProgressDialogHost />
      <PromptHost />
      <PreferencesHost />
      <AboutHost />
      <SetPathHost />
      <ImportDataHost />
      <CommandPalette />
      <PlotGalleryHost />
      <DesktopFilePickerHost />
      <LocalFileDropHost />
      <AudioUnlockHost />
      {disconnected && (
        <div className="disconnected">
          <div className="disconnected__card">
            <div className="disconnected__title">Nelson engine disconnected</div>
            <p>
              The <code>nelson-webview</code> process has stopped, so the desktop is now disabled.
              Start nelson-webview again, then reload this page.
            </p>
            <button onClick={() => window.location.reload()}>Reload</button>
          </div>
        </div>
      )}
    </div>
  );
}
//=============================================================================
// Diagnostic probe: fetches the draw list of every open figure, one at a time, and
// reports one line per figure, without mounting a canvas for each.
function RenderAllProbe(): JSX.Element {
  const [lines, setLines] = useState<string[]>([]);
  const [done, setDone] = useState(false);
  useEffect(() => {
    let cancelled = false;
    (async () => {
      const out: string[] = [];
      try {
        const list = await rpc<{ figures: { handle: number; title: string }[] }>('figure.list');
        for (const f of list.figures) {
          if (cancelled) return;
          try {
            const dl = await rpc<{ width: number; height: number; primitives: { kind: string }[] }>(
              'figure.getDisplayList',
              { handle: f.handle }
            );
            const counts: Record<string, number> = {};
            for (const p of dl.primitives) counts[p.kind] = (counts[p.kind] ?? 0) + 1;
            out.push(
              `OK ${f.handle} ${dl.width}x${dl.height} ` +
                Object.entries(counts)
                  .map(([k, n]) => `${k}=${n}`)
                  .join(' ')
            );
          } catch (e) {
            out.push(`ERR ${f.handle} ${String((e as Error).message)}`);
          }
          setLines([...out]);
        }
      } catch (e) {
        out.push(`FATAL ${String((e as Error).message)}`);
      }
      if (!cancelled) {
        setLines([...out]);
        setDone(true);
      }
    })();
    return () => {
      cancelled = true;
    };
  }, []);
  return (
    <pre id="renderall" data-done={done ? '1' : '0'} style={{ padding: 12, fontSize: 12 }}>
      {lines.join(String.fromCharCode(10))}
    </pre>
  );
}
//=============================================================================
