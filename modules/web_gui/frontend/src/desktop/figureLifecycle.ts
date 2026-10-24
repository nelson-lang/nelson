//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Keeps the desktop's figure panels and native figure windows in sync with the
// engine's figures: lifecycle pushes while commands run, plus a reconcile after
// every evaluation (and during the startup window) as a recovery path.
import type { DockviewApi } from 'dockview';
import {
  closeFigurePanel,
  dockFigureFromWindow,
  FIGURE_CLOSED_EVENT,
  getDockActivationSerial,
  isFigureInWindow,
  markFigureInWindow,
  openNotebook,
  openEditor,
  openFigure,
  openFigureInSeparateWindow,
  openHelp,
  openShell,
  unmarkFigureInWindow,
  updateFigurePanelTitle,
} from './desktop';
import { onPush, rpc } from '../rpc/client';
import { installWebToolPushes } from './webTools';
import type { FigureWindowStyle } from '../rpc/types';
import { clearDetached, isDetached, startDetachedWatcher } from './figureDetach';
import { handleClipboardImage } from './clipboardImage';
import { getDesktopCapabilities } from './capabilities';
import { isEngineBusy, onEngineReady, subscribeEngine } from './engineSession';
import { isWasmRuntime } from './platform';
import { isFigureLive } from '../notebook/liveFigures';
//=============================================================================
// Only the webview launch mode can own native OS windows.
function shouldOpenSeparateFigureWindow(style?: FigureWindowStyle): boolean {
  if (getDesktopCapabilities().launchMode !== 'webview') return false;
  return style !== 'docked';
}
//=============================================================================
export function installFigureLifecycle(api: DockviewApi): () => void {
  const seen = new Set<number>();
  let busyStartActivationSerial = getDockActivationSerial();
  type FigureFocusSnapshot = {
    activationSerial: number;
    busy: boolean;
    busyStartActivationSerial: number;
  };
  const captureFigureFocusSnapshot = (): FigureFocusSnapshot => ({
    activationSerial: getDockActivationSerial(),
    busy: isEngineBusy(),
    busyStartActivationSerial,
  });
  const newFigureShouldFocus = (snapshot: FigureFocusSnapshot): boolean => {
    if (!snapshot.busy) return true;
    return (
      snapshot.activationSerial === getDockActivationSerial() &&
      snapshot.activationSerial === snapshot.busyStartActivationSerial
    );
  };
  const offBusy = subscribeEngine((busy) => {
    if (busy) busyStartActivationSerial = getDockActivationSerial();
  });

  // figure('WindowStyle','docked') applies the style just after creation:
  // coalesce figure.created with the figure.windowStyle that follows so the
  // figure opens once, in its final container.
  const pendingCreate = new Map<number, { title: string; style?: FigureWindowStyle }>();
  const off = onPush('figure.created', (p) => {
    const { handle, title } = p as { handle: number; title: string };
    const windowStyle = (p as { windowStyle?: FigureWindowStyle }).windowStyle;
    const focusSnapshot = captureFigureFocusSnapshot();
    seen.add(handle);
    pendingCreate.set(handle, { title, style: windowStyle });
    setTimeout(() => {
      const pend = pendingCreate.get(handle);
      if (!pend) return;
      pendingCreate.delete(handle);
      if (shouldOpenSeparateFigureWindow(pend.style)) {
        openFigureInSeparateWindow(handle, pend.title);
      } else {
        openFigure(handle, pend.title, {
          focus: newFigureShouldFocus(focusSnapshot),
        });
      }
    }, 0);
  });
  const offStyle = onPush('figure.windowStyle', (p) => {
    const { handle } = p as { handle: number };
    const windowStyle = (p as { windowStyle?: FigureWindowStyle }).windowStyle;
    const pend = pendingCreate.get(handle);
    if (pend) {
      pend.style = windowStyle;
      return;
    }
    const wantWindow = shouldOpenSeparateFigureWindow(windowStyle);
    const isWindow = isFigureInWindow(handle);
    if (wantWindow && !isWindow) {
      openFigureInSeparateWindow(handle, `Figure ${handle + 1}`);
    } else if (!wantWindow && isWindow) {
      dockFigureFromWindow(handle, `Figure ${handle + 1}`);
    }
  });
  const offClosed = onPush('figure.closed', (p) => {
    const handle = Number((p as { handle?: number }).handle);
    if (!Number.isFinite(handle)) return;
    seen.delete(handle);
    clearDetached(handle);
    unmarkFigureInWindow(handle);
    closeFigurePanel(handle);
  });
  const offTitle = onPush('figure.title', (p) => {
    const { handle, title } = p as { handle: number; title: string };
    const pending = pendingCreate.get(handle);
    if (pending) pending.title = title;
    updateFigurePanelTitle(handle, title);
  });
  // Never dock a windowed figure; when its window goes away bring it back as
  // a panel so it is not lost.
  const offWindow = onPush('figure.window', (p) => {
    const { handle, open } = p as {
      handle: number;
      open: boolean;
      reason?: 'dock' | 'close';
    };
    const reason = (p as { reason?: 'dock' | 'close' }).reason;
    if (open) {
      markFigureInWindow(handle);
      closeFigurePanel(handle);
    } else {
      unmarkFigureInWindow(handle);
      if (reason === 'close') {
        void rpc('figure.close', { handle }).catch(() => undefined);
      } else if (seen.has(handle)) {
        openFigure(handle, `Figure ${handle + 1}`);
      }
    }
  });

  const offTerminal = onPush('terminal.open', () => openShell());
  // `doc` opens the shared documentation panel, not an operating-system browser.
  const offHelp = onPush('help.show', (payload) => {
    const topic = String((payload as { topic?: string })?.topic ?? '').trim();
    openHelp(topic || undefined);
  });
  const offTools = installWebToolPushes();

  // Re-dock a detached figure whose popup stopped heartbeating (popup closed
  // while this page was reloading).
  const offDetached = startDetachedWatcher(openFigure);

  const offEditor = onPush('editor.show', (payload) => {
    const editor = payload as { path?: string; line?: number };
    openEditor(String(editor.path ?? ''), editor.line);
  });
  const offNotebookOpen = onPush('notebook.open', (payload) => {
    const path = String((payload as { path?: string })?.path ?? '');
    if (!path) return;
    // One tab per document; the panel reads the file itself and shows its saved
    // output without auto-running.
    openNotebook(path);
  });
  const offClipboard = onPush('clipboard.image', (p) => {
    const dataUrl = String((p as { dataUrl?: string }).dataUrl ?? '');
    if (!dataUrl) return;
    void handleClipboardImage(dataUrl);
  });
  const offLiveFigureFrame = onPush('figure.frame', (p) => {
    const handle = Number((p as { handle?: number }).handle);
    // Inline Notebook figures are rendered by their owning panel. Their
    // frames must never be interpreted as requests to open a desktop tab.
    if (
      !Number.isFinite(handle) ||
      isFigureLive(handle) ||
      seen.has(handle) ||
      isFigureInWindow(handle)
    )
      return;
    seen.add(handle);
    openFigure(handle, `Figure ${handle + 1}`, {
      focus: newFigureShouldFocus(captureFigureFocusSnapshot()),
    });
    window.__nelsonPush?.('figure.updated', JSON.stringify({ handle }));
  });

  // Recovery path for a browser that connected after a lifecycle push.
  const forgetClosedFigure = (event: Event) => {
    const handle = Number((event as CustomEvent<{ handle?: number }>).detail?.handle);
    if (Number.isFinite(handle)) seen.delete(handle);
  };
  window.addEventListener(FIGURE_CLOSED_EVENT, forgetClosedFigure);
  const reconcileFigures = (): void => {
    rpc<{ figures: { handle: number; title: string; windowStyle?: FigureWindowStyle }[] }>(
      'figure.list'
    )
      .then((r) => {
        const currentHandles = new Set(r.figures.map((figure) => figure.handle));
        for (const handle of seen) {
          if (!currentHandles.has(handle)) seen.delete(handle);
        }
        for (const panel of api.panels) {
          if (!panel.id.startsWith('fig:')) continue;
          const handle = Number(panel.id.slice(4));
          if (Number.isFinite(handle) && !currentHandles.has(handle)) closeFigurePanel(handle);
        }
        for (const f of r.figures) {
          const pending = pendingCreate.get(f.handle);
          if (pending) pending.title = f.title;
          if (!seen.has(f.handle)) {
            seen.add(f.handle);
            if (!isDetached(f.handle) && !isFigureInWindow(f.handle)) {
              if (shouldOpenSeparateFigureWindow(f.windowStyle)) {
                openFigureInSeparateWindow(f.handle, f.title);
              } else {
                openFigure(f.handle, f.title, {
                  focus: newFigureShouldFocus(captureFigureFocusSnapshot()),
                });
              }
            }
          }
          updateFigurePanelTitle(f.handle, f.title);
          window.__nelsonPush?.('figure.updated', JSON.stringify({ handle: f.handle }));
        }
      })
      .catch(() => undefined);
  };
  // Poll only during the startup window; a permanent 2 Hz RPC would be
  // marshalled onto the interpreter thread during long computations.
  const RECONCILE_STARTUP_WINDOW_MS = 10000;
  // A browser reload can reconnect to the shared Wasm worker after a Live
  // Script run. Its old handles no longer have frontend ownership metadata, so
  // reconciling the worker's existing list would incorrectly resurrect those
  // inline figures as standalone desktop tabs. New figures are still opened by
  // the lifecycle pushes above; only this startup recovery is skipped in Wasm.
  const reconcileExistingOnStartup = !isWasmRuntime();
  const reconcileTimer = reconcileExistingOnStartup
    ? window.setInterval(reconcileFigures, 500)
    : null;
  const reconcileStopTimer = window.setTimeout(() => {
    if (reconcileTimer !== null) window.clearInterval(reconcileTimer);
  }, RECONCILE_STARTUP_WINDOW_MS);
  if (reconcileExistingOnStartup) reconcileFigures();
  const offReady = onEngineReady(() => {
    if (reconcileExistingOnStartup) reconcileFigures();
  });

  return () => {
    off();
    offClosed();
    offWindow();
    offStyle();
    offTitle();
    offBusy();
    offTerminal();
    offHelp();
    offTools();
    offEditor();
    offNotebookOpen();
    offClipboard();
    offLiveFigureFrame();
    offDetached();
    offReady();
    if (reconcileTimer !== null) window.clearInterval(reconcileTimer);
    window.clearTimeout(reconcileStopTimer);
    window.removeEventListener(FIGURE_CLOSED_EVENT, forgetClosedFigure);
  };
}
//=============================================================================
