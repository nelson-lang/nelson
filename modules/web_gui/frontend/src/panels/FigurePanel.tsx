//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { openFigurePrintPreview } from './figurePrint';
import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import { webPrompt } from '../components/PromptHost';
import type { FigureDisplayListResult, UIControl } from '../rpc/types';
import { FigureCanvas } from './FigureCanvas';
import { useFigureInspector } from './useFigureInspector';
import { useInspectorModel } from './useInspectorModel';
import { useFigureNavigation } from './useFigureNavigation';
import { startFigureZoomRegion } from './figureZoomRegion';
import { useFigureQuickActions } from './useFigureQuickActions';
import { FigureAxesIndicator } from './FigureAxesIndicator';
import { useFigureWheelZoom } from './useFigureWheelZoom';
import { useFigurePinch } from './useFigurePinch';
import { FigureToolbar } from './FigureToolbar';
import { isFigureFormTarget } from './figureShortcuts';
import { FigureInspector } from './FigureInspector';
import { FigureInspection } from './FigureInspection';
import { FigureLegend } from './FigureLegend';
import { useFigurePreferences } from './figurePreferences';
import { useFigureExport } from './useFigureExport';
import './figureWorkspace.css';
import { FigureControls } from './FigureControls';
import {
  installUiHtmlMessageBridge,
  releaseUiHtmlHosts,
  retainUiHtmlHosts,
  syncUiHtmlHosts,
} from './uihtmlHost';
import type { UiHtmlBridge } from './uihtmlHost';
import { FigureMenuBar, ContextMenuPopup } from './FigureMenuBar';
import { figureWindowNeedsResize, positionFigureControl } from './figureControlLayout';
import type { MenuNode } from '../rpc/types';
import { createResponseOrder, fetchDisplayList, withFigureSlot } from '../desktop/figureQueue';
import { startDetachedHeartbeat } from '../desktop/figureDetach';
import { undockFigure } from '../desktop/desktop';
import {
  hasCompleteFigureWebGlGeometry,
  isMatrixOnlyWebGlFrame,
  queueFigureWebGlAnimationFrameForHandle,
  retainFigureDisplayListWebGlGeometry,
} from './figureWebGlGeometry';
import type { FigureWebGlGeometry } from './figureWebGlGeometry';
import type { FigureWebGlBackend } from './figureWebGlRenderer';
import { useFigureMouse } from './useFigureMouse';
import { createLatestAnimationFrameQueue } from './figureAnimationFrameQueue';
import { reuseStableDisplayListParts } from './figureDisplayListReuse';
import { loadInitialFigureSnapshot } from './figureInitialSnapshot';
//=============================================================================
type PanelApi = {
  maximize: () => void;
  isMaximized: () => boolean;
  exitMaximized: () => void;
};
//=============================================================================
function hasNonEmptyStableParts(displayList: FigureDisplayListResult | null): boolean {
  if (!displayList) return false;
  return (
    (displayList.controls?.length ?? 0) > 0 ||
    (displayList.menus?.length ?? 0) > 0 ||
    (displayList.contextMenu?.length ?? 0) > 0
  );
}
//=============================================================================
// Picks the axes under the pointer so a drag rotates only that one. `rect`
// values are normalized top-origin figure fractions, so the pointer is mapped to
// the same space through the overlay's bounding box. Over a margin it falls back
// to the first axes. Returns null only for an engine that sends no per-axes
// information.
export { pickRotatableAxes } from './figureAxes';
//=============================================================================
// The backend serializes the figure as a draw list (RenderWeb) with Nelson's own
// projection, replayed on a canvas for 2D and 3D. Rotation is
// server-authoritative: a drag sends the new View to the engine, which returns
// the fresh draw list in one round-trip.
export function FigurePanel({
  handle,
  title,
  panelApi,
  standalone,
  inline,
  reloadToken,
  onPopOut,
}: {
  handle: number;
  title: string;
  panelApi?: PanelApi;
  standalone?: boolean;
  // Embedded in a document (Notebook): do not steal keyboard focus on mount,
  // which would pull the caret out of the surrounding editor.
  inline?: boolean;
  // Inline only: changes when a re-run redraws the figure behind this reused
  // handle. The panel then refetches the display list, keeping the current frame
  // until the new one loads.
  reloadToken?: string | number;
  // Inline only: "open in figure window", hands the figure to the desktop.
  onPopOut?: () => void;
}): JSX.Element {
  const [dl, setDl] = useState<FigureDisplayListResult | null>(null);
  const displayListRef = useRef<FigureDisplayListResult | null>(null);
  const acceptDisplayListRef = useRef<((next: FigureDisplayListResult) => void) | null>(null);
  const ordered = useRef(createResponseOrder()).current;
  const animationFrameQueueRef = useRef(
    createLatestAnimationFrameQueue<FigureDisplayListResult>((next) =>
      acceptDisplayListRef.current?.(next)
    )
  );
  const retainedWebGlGeometryRef = useRef(new Map<number, FigureWebGlGeometry>());
  const initialDisplayListLoadedRef = useRef(false);
  const liveAnimationFrameSeenRef = useRef(false);
  const handleRef = useRef(handle);
  const webGlBackendRef = useRef<FigureWebGlBackend | 'software' | 'pending'>('pending');
  // Title honors NumberTitle / Name; the backend pushes updates via figure.title.
  const [liveTitle, setLiveTitle] = useState(title);
  useEffect(() => setLiveTitle(title), [title]);
  useEffect(() => {
    return onPush('figure.title', (p) => {
      const d = p as { handle: number; title: string };
      if (d.handle === handle) setLiveTitle(d.title);
    });
  }, [handle]);
  const [maximized, setMaximized] = useState(false);
  const { preferences, change: changePreference } = useFigurePreferences();
  const { properties: inspectorOpen, details: detailsVisible } = preferences;
  const [selectedObject, setSelectedObject] = useState(0);
  const figureExport = useFigureExport(handle);
  const [printError, setPrintError] = useState('');
  const [saveError, setSaveError] = useState('');
  const [figCtx, setFigCtx] = useState<{ items: MenuNode[]; x: number; y: number } | null>(null);
  // Standalone window: the figure Position (drawing area) captured from the first
  // display list, and a flag set once the canvas matches it. Until then the
  // resize feedback below is suppressed, so the transient too-small opening size
  // cannot overwrite the Position.
  const targetDrawSize = useRef<{ handle: number; w: number; h: number } | null>(null);
  const windowSized = useRef(false);
  const windowSizeAttempts = useRef(0);
  const interactiveRefreshTimer = useRef<ReturnType<typeof setTimeout> | null>(null);

  const acceptDisplayList = (next: FigureDisplayListResult) => {
    const hydrated = retainFigureDisplayListWebGlGeometry(next, retainedWebGlGeometryRef.current);
    const displayList = reuseStableDisplayListParts(displayListRef.current, hydrated);
    displayListRef.current = displayList;
    setDl(displayList);
  };
  acceptDisplayListRef.current = acceptDisplayList;
  const needsSoftwareFallback = () => webGlBackendRef.current === 'software';
  const displayListParams = () => ({
    handle,
    softwareFallback: needsSoftwareFallback(),
  });
  const handleWebGlBackendChange = useCallback((backend: FigureWebGlBackend | 'software') => {
    webGlBackendRef.current = backend;
    if (
      backend === 'software' &&
      displayListRef.current?.webglMeshes?.length &&
      !displayListRef.current.softwareFallbackImage
    ) {
      fetchDisplayList({ handle: handleRef.current, softwareFallback: true })
        .then(ordered((next: FigureDisplayListResult) => acceptDisplayListRef.current?.(next)))
        .catch(() => undefined);
    }
  }, []);
  useEffect(() => {
    handleRef.current = handle;
    webGlBackendRef.current = 'pending';
    retainedWebGlGeometryRef.current.clear();
    initialDisplayListLoadedRef.current = false;
    liveAnimationFrameSeenRef.current = false;
    targetDrawSize.current = null;
    windowSized.current = false;
    windowSizeAttempts.current = 0;
  }, [handle]);
  useEffect(() => {
    return () => {
      if (interactiveRefreshTimer.current !== null) {
        clearTimeout(interactiveRefreshTimer.current);
        interactiveRefreshTimer.current = null;
      }
    };
  }, [handle]);

  const controlHeightOffset = dl ? dl.height - (dl.controlLayoutHeight ?? dl.height) : 0;
  const displayedControls = useMemo(
    () => dl?.controls?.map((control) => positionFigureControl(control, controlHeightOffset)) ?? [],
    [dl?.controls, controlHeightOffset]
  );
  const displayListReady = dl !== null;

  // Focus the figure on its first render so WindowKeyPressFcn figures (games) get
  // keys without a click, e.g. right after Run in the Examples panel. Skipped
  // while the user types in a field (the Command Window that launched the plot).
  const panelRef = useRef<HTMLDivElement | null>(null);
  const autoFocusedRef = useRef(false);
  useEffect(() => {
    if (autoFocusedRef.current || !displayListReady || inline) return;
    const el = panelRef.current;
    if (!el || el.offsetParent === null) return;
    const active = document.activeElement;
    if (active && active !== document.body && isFigureFormTarget(active)) return;
    el.focus();
    autoFocusedRef.current = true;
  }, [displayListReady]);

  const toggleMaximize = () => {
    if (!panelApi) return;
    if (panelApi.isMaximized()) {
      panelApi.exitMaximized();
      setMaximized(false);
    } else {
      panelApi.maximize();
      setMaximized(true);
    }
  };

  useEffect(() => {
    if (!standalone) return;
    return startDetachedHeartbeat(handle);
  }, [standalone, handle]);

  // WebUI mirrors document.title to the native window title bar.
  useEffect(() => {
    if (!standalone) return;
    const previous = document.title;
    document.title = liveTitle;
    return () => {
      document.title = previous;
    };
  }, [standalone, liveTitle]);

  // Standalone window: write user move/resize back onto the figure Position
  // (pixels, bottom-origin, drawable area). JS has no window-move event, so poll
  // and also react to resize.
  useEffect(() => {
    if (!standalone) return;
    const geom = () => {
      // The Position is in logical (CSS) pixels: (w,h) is the canvas box and
      // (x,y) its bottom-left in logical screen coordinates. WebView2 reports
      // screenX/screenY in physical pixels but innerWidth/innerHeight and
      // screen.height in CSS pixels, so the physical ones are divided by the
      // device pixel ratio.
      const dpr = window.devicePixelRatio || 1;
      const box = canvasBox.current;
      const width = Math.round(box ? box.clientWidth : window.innerWidth);
      const height = Math.round(box ? box.clientHeight : window.innerHeight);
      const x = Math.round(window.screenX / dpr);
      const screenH = Math.round(window.screen?.height ?? window.innerHeight);
      // The drawing area sits at the bottom of the client, so its bottom edge is
      // the client bottom (screenY + full client height, top-origin).
      const bottom = Math.max(0, screenH - Math.round(window.screenY / dpr) - window.innerHeight);
      return { x, y: bottom, width, height };
    };
    let last = JSON.stringify(geom());
    const tick = () => {
      // Suppress until the window has been sized to the requested Position, so
      // the transient opening geometry is never written back as the Position.
      if (!windowSized.current) {
        last = JSON.stringify(geom());
        return;
      }
      const g = geom();
      if (g.width <= 0 || g.height <= 0) return;
      const key = JSON.stringify(g);
      if (key === last) return;
      last = key;
      void rpc('figure.setPosition', { handle, ...g }).catch(() => undefined);
    };
    const id = window.setInterval(tick, 350);
    window.addEventListener('resize', tick);
    return () => {
      window.clearInterval(id);
      window.removeEventListener('resize', tick);
    };
  }, [standalone, handle]);

  const printFigure = () => {
    setPrintError('');
    void openFigurePrintPreview(async () => {
      const result = await rpc<{ bytes?: number[]; queued?: boolean }>('figure.action', {
        handle,
        request: { action: 'export', format: 'svg', scale: 1 },
      });
      if (!result.bytes)
        throw new Error(
          result.queued
            ? 'Printing is available when execution has finished.'
            : 'No figure received for printing.'
        );
      return new Blob([new Uint8Array(result.bytes)], { type: 'image/svg+xml' });
    }, liveTitle).catch((error) =>
      setPrintError(error instanceof Error ? error.message : String(error))
    );
  };
  // Server-side save (png / svg / pdf by extension), by graphics handle rather
  // than figure number.
  const saveAs = async () => {
    setSaveError('');
    const path = await webPrompt(
      typeof __NELSON_WASM__ !== 'undefined' && __NELSON_WASM__
        ? 'Save figure to workspace (.png / .svg / .pdf):'
        : 'Save figure to file (.png / .svg / .pdf):',
      `figure-${handle}.png`
    );
    if (!path) return;
    try {
      await rpc('figure.saveAs', { handle, path });
    } catch (error) {
      setSaveError(error instanceof Error ? error.message : String(error));
    }
  };

  const refresh = () => {
    fetchDisplayList(displayListParams())
      .then(
        ordered((next) => {
          acceptDisplayList(next);
          // All figure refresh paths (cell runs, stale-cell runs, controls and
          // native figure updates) share this callback. Keep the inline stage
          // aligned even when the caller did not provide a Notebook token.
          const force = forceResizeAfterRefreshRef.current;
          forceResizeAfterRefreshRef.current = false;
          reloadSizeSyncRef.current?.(force);
          if (fitAfterRefreshRequestedRef.current) {
            fitAfterRefreshRequestedRef.current = false;
            void fitAfterRefreshRef.current?.();
          }
        })
      )
      .catch(() => {
        // Keep the flag set so a later retry still performs the post-refresh
        // resize if the first fetch raced the interpreter.
      });
  };
  const refreshRef = useRef(refresh);
  refreshRef.current = refresh;

  // A re-run that reuses this handle redraws the figure without changing
  // `handle`, so `reloadToken` triggers the refetch. The current frame stays until
  // the new one arrives. The initial mount is loaded by the [handle] effect.
  const reloadTokenMounted = useRef(false);
  const reloadSizeSyncRef = useRef<((force?: boolean) => void) | null>(null);
  // A Notebook rerun replaces the engine display list while keeping the
  // inline FigurePanel mounted. The immediate resize below is only a preview;
  // the authoritative resize must happen after refresh() has accepted the new
  // display list. This flag avoids forcing ordinary figure.updated refreshes.
  const forceResizeAfterRefreshRef = useRef(false);
  const fitAfterRefreshRequestedRef = useRef(false);
  const fitAfterRefreshRef = useRef<(() => Promise<void>) | null>(null);
  useEffect(() => {
    if (!reloadTokenMounted.current) {
      reloadTokenMounted.current = true;
      return;
    }
    if (inline) forceResizeAfterRefreshRef.current = true;
    refreshRef.current();
    // A Notebook rerun keeps the inline panel mounted. Its ResizeObserver
    // therefore has no geometry change to observe, even though the engine has
    // recreated the figure and reset its draw size. Re-apply the current host
    // rectangle explicitly so Run has the same sizing result as Restore view.
    if (inline) {
      const sync = () => reloadSizeSyncRef.current?.();
      sync();
      const frame = window.requestAnimationFrame(() => {
        window.requestAnimationFrame(sync);
      });
      return () => window.cancelAnimationFrame(frame);
    }
  }, [inline, reloadToken]);

  // Run all/stale executes several cells back-to-back. Fit only once after the
  // complete Notebook run, otherwise a fit action can race the next cell's
  // interpreter call and corrupt the action response.
  useEffect(() => {
    if (!inline) return;
    return onPush('notebook.run.completed', () => {
      fitAfterRefreshRequestedRef.current = true;
      refreshRef.current();
    });
  }, [inline, handle]);

  const applyViewDisplayList = (next: FigureDisplayListResult) => {
    if (isMatrixOnlyWebGlFrame(displayListRef.current, next)) {
      queueFigureWebGlAnimationFrameForHandle(handle, next.webglMeshes ?? []);
    } else acceptDisplayList(next);
  };
  // The axes indicator of the pan/rotate quick tools names the hovered axes
  // after its title, which only the inspector state knows: keep that state
  // loaded while a quick tool is active, not only while the inspector is open.
  const [quickToolActive, setQuickToolActive] = useState(false);
  const inspector = useFigureInspector(
    handle,
    acceptDisplayList,
    inspectorOpen || detailsVisible || quickToolActive
  );
  const inspectTarget = selectedObject || inspector.point?.objectHandle || 0;
  const inspectorModel = useInspectorModel(handle, inspectTarget, inspectorOpen);
  const uihtmlAnchorRef = useRef<HTMLDivElement | null>(null);
  const afterRotation = (request: Record<string, unknown>): Promise<void> =>
    navigation.finishRotation().then((active) => {
      if (active) return inspector.action(request);
    });
  const flushWheel = useFigureWheelZoom(
    handle,
    uihtmlAnchorRef,
    dl,
    inspector.setAxesHandle,
    afterRotation,
    () => refreshRef.current()
  );
  const figureAction = (request: Record<string, unknown>) => {
    flushWheel();
    // Tell the engine whether this client still needs the software raster, so a
    // WebGL frame is not rasterised twice on every property edit or navigation.
    const compactView = request.compactView === true;
    if (interactiveRefreshTimer.current !== null) {
      clearTimeout(interactiveRefreshTimer.current);
      interactiveRefreshTimer.current = null;
    }
    const promise = afterRotation({ softwareFallback: needsSoftwareFallback(), ...request });
    if (compactView) {
      // Compact pan/zoom frames update only the retained WebGL matrices. Once
      // the pointer settles, fetch one complete frame so axes, ticks and labels
      // catch up without serialising them for every pointer event.
      interactiveRefreshTimer.current = setTimeout(() => {
        interactiveRefreshTimer.current = null;
        refreshRef.current();
      }, 180);
    }
    return promise;
  };
  fitAfterRefreshRef.current = async () => {
    // `CurrentAxes` is not guaranteed to be scalar (notably for a freshly
    // recreated Notebook figure). Passing axesHandle=0 makes the Nelson
    // macro operate on that handle-array and raises conversionNotSupported.
    // Use the explicit axes handles reported by the display list instead.
    const axes = displayListRef.current?.rotatableAxes ?? [];
    for (const axis of axes) {
      await figureAction({ action: 'fit', axesHandle: axis.handle });
    }
  };
  useFigurePinch(handle, uihtmlAnchorRef, dl, inspector.setAxesHandle, figureAction);
  const navigation = useFigureNavigation({
    handle,
    dl,
    displayListParams,
    apply: applyViewDisplayList,
    axes: inspector.axes,
    selectAxes: inspector.setAxesHandle,
    action: figureAction,
    refreshProperties: inspector.refresh,
  });
  const { tool, setTool, startDrag, startPan, beginModeless, cancelDrag } = navigation;
  useEffect(() => {
    setQuickToolActive(tool === 'pan' || tool === 'rotate');
  }, [tool]);
  useEffect(() => {
    if (inspector.point?.found && inspector.point.objectHandle)
      setSelectedObject(inspector.point.objectHandle);
  }, [inspector.point]);
  // inspect(obj) / inspect('-close') from the command line arrives through the
  // one-shot inspector channel in the figure state.
  useEffect(() => {
    const channel = inspector.state?.inspector;
    if (!channel) return;
    if (channel.target >= 0) {
      if (!inspectorOpen) changePreference('properties', true);
      setSelectedObject(channel.target);
    } else if (channel.close && inspectorOpen) {
      changePreference('properties', false);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [inspector.state?.inspector]);
  const quick = useFigureQuickActions({
    handle,
    dl,
    axes: inspector.axes,
    setTool,
    selectAxes: inspector.setAxesHandle,
    action: figureAction,
    tool,
    point: inspector.point,
  });
  useEffect(() => {
    inspector.clearPoint();
  }, [tool, dl?.width, dl?.height, inspector.clearPoint]);

  useEffect(() => {
    let active = true;
    // A native figure window can mount while the creating command still owns
    // the interpreter. Retry the initial idempotent read so that one early
    // "still starting"/"not available" reply cannot leave the window on its
    // placeholder forever.
    const loadInitialDisplayList = async () => {
      // Give a running drawnow animation time to publish its first frame. Its
      // creating command still owns the interpreter, so an immediate snapshot
      // RPC can occupy a WebUI request indefinitely and delay control actions.
      // Static figures do not publish figure.frame and take the normal RPC path.
      await new Promise((resolve) => window.setTimeout(resolve, 250));
      const hasStableParts = hasNonEmptyStableParts(displayListRef.current);
      if (!active || (liveAnimationFrameSeenRef.current && hasStableParts)) {
        return;
      }
      const accept = ordered(acceptDisplayList);
      await loadInitialFigureSnapshot(
        () => fetchDisplayList(displayListParams()),
        (next) => {
          accept(next);
          initialDisplayListLoadedRef.current = true;
        },
        () => active
      );
    };
    void loadInitialDisplayList();
    const offUpdated = onPush('figure.updated', (p) => {
      if ((p as { handle?: number }).handle === handle) refresh();
    });
    // drawnow frames arrive already rendered: apply them directly, since a
    // re-fetch would block until the script yields.
    const offFrame = onPush('figure.frame', (p) => {
      const f = p as { handle?: number; displayList?: FigureDisplayListResult };
      if (f.handle !== handle || !f.displayList) return;
      const frame = retainFigureDisplayListWebGlGeometry(
        f.displayList,
        retainedWebGlGeometryRef.current
      );
      if (hasNonEmptyStableParts(frame)) {
        liveAnimationFrameSeenRef.current = true;
      }
      // A long-running script can keep the interpreter busy before the initial
      // RPC snapshot executes. Ignore compact startup frames until the periodic
      // full animation frame establishes every retained WebGL buffer.
      if (!initialDisplayListLoadedRef.current) {
        if (frame.webglMeshes?.length && !hasCompleteFigureWebGlGeometry(frame.webglMeshes)) {
          return;
        }
        initialDisplayListLoadedRef.current = true;
      }
      const matrixOnlyFrame = isMatrixOnlyWebGlFrame(displayListRef.current, frame);
      if (frame.matrixOnly && !matrixOnlyFrame) {
        refresh();
        return;
      }
      if (matrixOnlyFrame) {
        queueFigureWebGlAnimationFrameForHandle(handle, frame.webglMeshes ?? []);
      } else {
        animationFrameQueueRef.current.push(frame);
      }
    });
    return () => {
      active = false;
      animationFrameQueueRef.current.cancel();
      offUpdated();
      offFrame();
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [handle]);

  // Resize sync: tell the engine to resize the figure (updates its Position) and
  // repaint from the returned draw list. Coalesced; no-op resizes are skipped.
  const canvasBox = useRef<HTMLDivElement>(null);
  // Live panel size, updated on every ResizeObserver tick. During a Dockview sash
  // drag the engine round-trip lags far behind the pointer, so the stage below is
  // CSS-scaled to this size as an instant preview; the debounced engine resize
  // then repaints crisp.
  const [box, setBox] = useState<{ w: number; h: number } | null>(null);
  const sizeDebounce = useRef<ReturnType<typeof setTimeout> | null>(null);
  const sizeJob = useRef<{ pending: boolean; queued: [number, number] | null; last: string }>({
    pending: false,
    queued: null,
    last: '',
  });
  const resizeFrame = useRef<number | null>(null);

  // A live result can replace the display list without changing the inline
  // wrapper geometry. ResizeObserver is intentionally asynchronous and may
  // therefore leave one paint with the old draw-list size. Capture the host
  // rectangle before that paint so the stage is scaled immediately; the normal
  // resize observer still performs the crisp engine resize afterwards.
  useLayoutEffect(() => {
    if (!inline || !displayListReady) return;
    const el = canvasBox.current;
    if (!el) return;
    const w = Math.round(el.clientWidth);
    const h = Math.round(el.clientHeight);
    if (w <= 10 || h <= 10) return;
    setBox((previous) => (previous?.w === w && previous.h === h ? previous : { w, h }));
  }, [inline, displayListReady, dl?.width, dl?.height, reloadToken]);

  const setSize = (w: number, h: number, force = false) => {
    const key = `${w}x${h}`;
    if (!force && key === sizeJob.current.last) return;
    if (sizeJob.current.pending) {
      sizeJob.current.queued = [w, h];
      return;
    }
    sizeJob.current.pending = true;
    sizeJob.current.last = key;
    withFigureSlot(() =>
      rpc<FigureDisplayListResult>('figure.setSize', {
        ...displayListParams(),
        width: w,
        height: h,
      })
    )
      .then(ordered(acceptDisplayList))
      .catch(() => {
        // A Notebook can finish a draw while the interpreter is still
        // servicing the panel's previous resize request. Do not remember a
        // failed size as if it had been applied; the next explicit sync must
        // be allowed to retry it.
        if (sizeJob.current.last === key) sizeJob.current.last = '';
      })
      .finally(() => {
        sizeJob.current.pending = false;
        const q = sizeJob.current.queued;
        if (q) {
          sizeJob.current.queued = null;
          setSize(q[0], q[1], true);
        }
      });
  };
  reloadSizeSyncRef.current = (force = true) => {
    const el = canvasBox.current;
    if (!el) return;
    const w = Math.round(el.clientWidth);
    const h = Math.round(el.clientHeight);
    if (w <= 10 || h <= 10) return;
    setBox({ w, h });
    setSize(w, h, force);
  };
  // uihtml pages live in iframes owned by uihtmlHost, outside React's control
  // list and this panel: moving an iframe in the DOM reloads its document, the
  // control list reorders whenever the figure's children do, and the dock removes
  // an unselected tab's content from the document. The stage below is only the
  // rectangle those iframes are aligned with.
  const figureMouse = useFigureMouse(handle, uihtmlAnchorRef, dl);
  const uihtmlBridge = useRef<UiHtmlBridge>({
    pull: (h) =>
      rpc('uihtml.pull', { handle: h }) as Promise<{
        dataJson: string;
        revision: number;
        events: { name: string; data: string }[];
      }>,
    push: (h, envelope) =>
      withFigureSlot(() => rpc('uicontrol.action', { handle: h, value: 0, text: envelope }))
        .then(() => refresh())
        .catch(() => undefined),
    fileUrl: (h) => `/uihtml/${h}`,
  });

  useEffect(() => installUiHtmlMessageBridge(), []);

  // The pages outlive this component, so they are handed back only when the
  // figure itself goes away.
  useEffect(() => {
    retainUiHtmlHosts(handle);
    return () => releaseUiHtmlHosts(handle);
  }, [handle]);

  useEffect(() => {
    syncUiHtmlHosts(handle, uihtmlAnchorRef.current, displayedControls, uihtmlBridge.current);
  }, [handle, displayedControls]);

  const controlAction = useCallback((control: UIControl, value: number, text: string) => {
    withFigureSlot(() => rpc('uicontrol.action', { handle: control.handle, value, text }))
      .then(() => refreshRef.current())
      .catch(() => undefined);
  }, []);

  // A uimenu selection uses the same action RPC; the bridge dispatches by handle
  // to the GOMenu and enqueues its MenuSelectedFcn.
  const menuSelect = useCallback((menuHandle: number) => {
    withFigureSlot(() => rpc('uicontrol.action', { handle: menuHandle, value: 0, text: '' }))
      .then(() => refreshRef.current())
      .catch(() => undefined);
  }, []);

  // Keep the native standalone window aligned with the figure Position. This is
  // also needed after SizeChangedFcn constrains a user resize and returns a draw
  // size different from the current canvas.
  useEffect(() => {
    if (!standalone || !displayListReady) return;
    const current = displayListRef.current;
    const el = canvasBox.current;
    if (!current || !el) return;
    const canvasWidth = Math.round(el.clientWidth);
    const canvasHeight = Math.round(el.clientHeight);
    targetDrawSize.current = { handle, w: current.width, h: current.height };
    if (figureWindowNeedsResize(current.width, current.height, canvasWidth, canvasHeight)) {
      windowSized.current = false;
      windowSizeAttempts.current = 0;
    } else {
      windowSized.current = true;
    }
  }, [standalone, displayListReady, handle, dl?.width, dl?.height]);

  // Size the host window so the canvas equals the figure Position, and keep the
  // figure size in sync with later user resizes. One ResizeObserver drives both:
  // requestAnimationFrame is throttled when the window is unfocused and would
  // stall the sizing.
  //
  // The drawing area is the Position plus chrome. Only the frontend knows the
  // device pixel ratio and its toolbar/menu height, so it measures both and asks
  // the host for (Position + chrome) x dpr, iterating (the observer re-fires
  // after each host resize) until the canvas box matches the Position.
  useEffect(() => {
    if (!displayListReady) return;
    const el = canvasBox.current;
    if (!el || typeof ResizeObserver === 'undefined') return;
    const MAX_SIZE_ATTEMPTS = 25;
    const driveStandaloneWindowSize = (w: number, h: number): boolean => {
      // Returns true while it is still driving the host size (caller must not
      // also run the figure.setSize feedback, which would fight it).
      const target = targetDrawSize.current;
      if (!target || target.handle !== handle) return false;
      const dw = w - target.w;
      const dh = h - target.h;
      if (Math.abs(dw) <= 1 && Math.abs(dh) <= 1) {
        windowSized.current = true;
        return false;
      }
      if (windowSizeAttempts.current >= MAX_SIZE_ATTEMPTS) {
        // Stop retrying but let move/resize tracking resume.
        windowSized.current = true;
        return false;
      }
      windowSizeAttempts.current += 1;
      if (window.opener) {
        // Browser popup fallback: resizeBy adjusts the outer window and the
        // canvas box moves with it, so it needs neither dpr nor chrome.
        window.resizeBy(-dw, -dh);
      } else {
        // Native window: client area = drawing area plus panel chrome, in
        // physical pixels.
        const dpr = window.devicePixelRatio || 1;
        const chromeW = Math.max(0, window.innerWidth - w);
        const chromeH = Math.max(0, window.innerHeight - h);
        const width = Math.round((target.w + chromeW) * dpr);
        const height = Math.round((target.h + chromeH) * dpr);
        void rpc('figure.setWindowSize', { handle, width, height }).catch(() => undefined);
      }
      return true;
    };
    const syncSize = () => {
      const w = Math.round(el.clientWidth);
      const h = Math.round(el.clientHeight);
      if (w <= 10 || h <= 10) return;
      setBox({ w, h });
      if (standalone && !windowSized.current) {
        // Drive the host window to the figure Position; suppress figure.setSize
        // meanwhile so the opening (too-small) canvas cannot shrink the figure.
        if (driveStandaloneWindowSize(w, h)) return;
      }
      if (dl && figureWindowNeedsResize(dl.width, dl.height, w, h)) {
        if (sizeDebounce.current) clearTimeout(sizeDebounce.current);
        if (resizeFrame.current !== null) window.cancelAnimationFrame(resizeFrame.current);
        resizeFrame.current = window.requestAnimationFrame(() => {
          resizeFrame.current = window.requestAnimationFrame(() => {
            resizeFrame.current = null;
            setSize(w, h, true);
          });
        });
        return;
      }
      if (sizeDebounce.current) clearTimeout(sizeDebounce.current);
      sizeDebounce.current = setTimeout(() => setSize(w, h), standalone ? 40 : 120);
    };
    const ro = new ResizeObserver(syncSize);
    ro.observe(el);
    syncSize();
    // Liveness backstop: if the host never honors the resize (so the observer
    // never re-fires to converge), release the gate anyway so later user resizes
    // still reach figure.setSize.
    const backstop =
      standalone && !windowSized.current
        ? window.setTimeout(() => {
            windowSized.current = true;
          }, 3000)
        : undefined;
    return () => {
      ro.disconnect();
      if (backstop) window.clearTimeout(backstop);
      if (sizeDebounce.current) clearTimeout(sizeDebounce.current);
      if (resizeFrame.current !== null) {
        window.cancelAnimationFrame(resizeFrame.current);
        resizeFrame.current = null;
      }
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [handle, displayListReady, standalone, dl?.width, dl?.height]);

  return (
    <div
      ref={panelRef}
      className="figure-panel"
      aria-busy={inspector.pending}
      tabIndex={0}
      onMouseDown={(event) => {
        const target = event.target as HTMLElement;
        if (!target.closest('button, input, select, textarea, [contenteditable="true"]')) {
          event.currentTarget.focus();
        }
      }}
      onKeyDown={(event) => quick.key(event, true)}
      onKeyUp={(event) => quick.key(event, false)}
      onBlur={quick.blur}
    >
      <FigureToolbar
        title={liveTitle}
        is3d={!!dl?.is3d}
        tool={tool}
        setTool={setTool}
        view={(inspectorOpen || detailsVisible ? inspector.axes?.View : undefined) ?? dl?.view}
        detailsVisible={detailsVisible}
        toggleDetails={() => changePreference('details', !detailsVisible)}
        inspectorOpen={inspectorOpen}
        toggleInspector={() => changePreference('properties', !inspectorOpen)}
        canUndo={inspector.state?.canUndo}
        canRedo={inspector.state?.canRedo}
        maximized={maximized}
        toggleMaximize={!standalone && panelApi ? toggleMaximize : undefined}
        onAction={(request) => {
          void figureAction(request);
        }}
        exportFigure={figureExport.exportFigure}
        exporting={figureExport.exporting}
        saveAsLabel={
          typeof __NELSON_WASM__ !== 'undefined' && __NELSON_WASM__
            ? 'Save to workspace…'
            : 'Save as…'
        }
        saveAs={() => {
          void saveAs();
        }}
        print={printFigure}
        refresh={refresh}
        detachLabel={
          inline && onPopOut
            ? 'Open in figure window'
            : !standalone
            ? 'Undock (separate window)'
            : 'Dock into desktop'
        }
        detach={() => {
          if (inline && onPopOut) onPopOut();
          else if (!standalone) undockFigure(handle, liveTitle);
          else if (window.opener) window.close();
          else void rpc('figure.dock', { handle }).catch(() => undefined);
        }}
      />
      {dl?.menus && dl.menus.length > 0 && <FigureMenuBar menus={dl.menus} onSelect={menuSelect} />}
      <div className="figure-workspace-content">
        <div
          className="figure-panel__canvas"
          ref={canvasBox}
          onContextMenu={(e) => {
            if (dl?.contextMenu && dl.contextMenu.length > 0) {
              e.preventDefault();
              setFigCtx({ items: dl.contextMenu, x: e.clientX, y: e.clientY });
            }
          }}
        >
          {dl ? (
            // The stage holds the canvas and the uicontrol overlay at the
            // engine-rendered size; scaling it (instead of its children) keeps
            // the overlay aligned during the resize preview.
            <div
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: dl.width,
                height: dl.height,
                transformOrigin: '0 0',
                transform:
                  box && (box.w !== dl.width || box.h !== dl.height)
                    ? `scale(${box.w / dl.width}, ${box.h / dl.height})`
                    : undefined,
              }}
              ref={uihtmlAnchorRef}
              {...figureMouse}
              onMouseMoveCapture={quick.move}
              onMouseLeave={quick.leave}
              onDoubleClick={quick.doubleClick}
              onMouseDownCapture={(event) => {
                flushWheel();
                quick.down(event);
                if (tool !== 'inspect' || (event.target as HTMLElement).tagName !== 'CANVAS')
                  return;
                // Modeless: click picks, drag pans (2D) or rotates (3D).
                beginModeless(event, (xFrac, yFrac) => {
                  void inspector.action({
                    action: 'pick',
                    x: xFrac * dl.width,
                    y: dl.height - yFrac * dl.height,
                  });
                });
              }}
            >
              <FigureCanvas
                handle={handle}
                primitives={dl.primitives}
                webglMeshes={dl.webglMeshes}
                softwareFallbackImage={dl.softwareFallbackImage}
                width={dl.width}
                height={dl.height}
                onWebGlBackendChange={handleWebGlBackendChange}
              />
              {quick.axis && (
                <FigureAxesIndicator
                  axis={quick.axis}
                  label={
                    inspector.state?.axes.find((axis) => axis.handle === quick.axis?.handle)
                      ?.title ||
                    `Axes ${
                      (dl.rotatableAxes?.findIndex((axis) => axis.handle === quick.axis?.handle) ??
                        0) + 1
                    }`
                  }
                />
              )}
              {tool === 'inspect' && (
                <FigureInspection
                  point={inspector.point}
                  objects={inspector.axes?.objects ?? []}
                  width={dl.width}
                  height={dl.height}
                />
              )}
              {displayedControls.length > 0 && (
                <FigureControls
                  controls={displayedControls}
                  onAction={controlAction}
                  onMenuSelect={menuSelect}
                />
              )}
              {dl.is3d && tool === 'rotate' && (
                <div
                  className="figure-panel__rotate"
                  title="Drag to rotate"
                  data-view={dl.view?.join(',')}
                  onMouseDown={startDrag}
                />
              )}
              {tool === 'pan' && (
                <div className="figure-panel__pan" title="Drag to pan" onMouseDown={startPan} />
              )}
              {tool === 'zoom' && (
                <div
                  className="figure-panel__zoom"
                  title={
                    dl.is3d
                      ? 'Click to zoom in, Shift-click to zoom out; scroll to zoom'
                      : 'Drag a box to zoom in; click to zoom in, Shift-click to zoom out'
                  }
                  onMouseDown={(event) =>
                    startFigureZoomRegion(event, {
                      dl,
                      axesHandle: inspector.axes?.handle ?? 0,
                      selectAxes: inspector.setAxesHandle,
                      action: figureAction,
                      cancelDrag,
                    })
                  }
                />
              )}
            </div>
          ) : (
            <div className="browser__empty">rendering…</div>
          )}
        </div>
        {inspectorOpen && (
          <FigureInspector
            width={preferences.width}
            onWidthChange={(width) => changePreference('width', width)}
            state={inspector.state}
            axes={inspector.axes}
            model={inspectorModel?.model ?? null}
            tree={inspectorModel?.tree ?? null}
            selectedObject={inspectTarget}
            selectObject={setSelectedObject}
            action={(request) => {
              void figureAction(request);
            }}
          />
        )}
      </div>
      {detailsVisible && (
        <>
          {inspector.axes && inspector.axes.objects.length > 0 && (
            <FigureLegend
              objects={inspector.axes.objects}
              action={(request) => {
                void figureAction(request);
              }}
            />
          )}
          <div className="figure-workspace-status">
            <span aria-live="polite">
              {inspector.pending
                ? 'Pending engine update…'
                : figureExport.exporting
                ? 'Exporting…'
                : tool === 'rotate'
                ? 'Rotate · drag to change the view'
                : tool === 'pan'
                ? 'Pan · drag to move the view'
                : tool === 'zoom'
                ? 'Zoom · drag a box or scroll'
                : 'Inspect · click a point, drag to pan or rotate'}
            </span>
            <span>
              {inspector.point?.found && inspector.point.position
                ? `Point ${inspector.point.index} · ${inspector.point.position
                    .map((v, i) => `${'XYZ'[i]} ${Number(v.toPrecision(6))}`)
                    .join(' · ')}`
                : inspector.axes
                ? `Az ${inspector.axes.View[0].toFixed(1)}° · El ${inspector.axes.View[1].toFixed(
                    1
                  )}°`
                : ''}
            </span>
          </div>
        </>
      )}
      {(inspector.error || figureExport.exportError || printError || saveError) && (
        <div className="figure-workspace-error" role="alert">
          {inspector.error || figureExport.exportError || printError || saveError}
        </div>
      )}
      {figCtx ? (
        <ContextMenuPopup
          items={figCtx.items}
          x={figCtx.x}
          y={figCtx.y}
          onSelect={menuSelect}
          onClose={() => setFigCtx(null)}
        />
      ) : null}
    </div>
  );
}
//=============================================================================
