//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
import { writeStatus } from './status-service.js';
import { clearDocumentDiagnostics } from './diagnostics-service.js';
import {
  state,
  signalDiagramChanged,
  setCurrentFilePath,
  setFitToDiagram,
  setUpdateStabilityPanel,
} from './state.js';
import { initModelSession } from './model-session.js';
import {
  toJSON,
  serializeDiagram,
  serializeDiagramForSave,
  commitSerializedSaveMetadata,
  parseDiagram,
  sanitizeFilename,
} from './file-operations.js';
import { setStatusEl, handleSaveAsSubsystem, saveSubsystemFromBlock } from './modal-handlers.js';
import {
  setHomeBtn as setHomeBtnMenu,
  setZoomInBtn as setZoomInBtnMenu,
  setZoomOutBtn as setZoomOutBtnMenu,
  setDiagramNameInput,
  setRuntimeInput,
  setSimDt,
  setRealTimeModeInput,
  setAutoRouteInput,
  setVariablesInput,
  setVariablesPreview,
  initMenuHandlers,
} from './menu-handlers.js';
import {
  loadDiagram,
  setSubsystemUpBtn,
  updateSubsystemNavUi,
  syncLockedUi,
  setRendererRef as setRendererRefDiagram,
  setLockedInput as setLockedInput,
  setRealTimeModeInput as setRealTimeModeInputDiagram,
} from './diagram-handlers.js';
import {
  setRendererRef,
  setViewBox,
  getViewBox,
  initViewBox,
  setSvg,
  setUpdateStatusBar,
} from './workspace-handlers.js';
import { initSignalBadges } from './signal-badges.js';
import { createInspector } from '../blocks/inspector.js';
import { applyDataEditNames } from '../blocks/inspector-block.js';
import { renderScope } from '../sim/render-scope.js';
import { buildDiagramAndScopeMap } from '../sim/resolve-params.js';
import {
  setBlockLibraryGroups,
  setBlockLibrary,
  setGridSize,
  setRendererRef as setRendererRefLibrary,
  setStatusEl as setStatusElLibrary,
} from './block-library-handlers.js';
import {
  initSidebarUI,
  initZoomButtons,
  setStatusElRef,
  setHomeBtnRef,
  setZoomInBtnRef,
  setZoomOutBtnRef,
  initModals,
} from './ui-handlers.js';
import {
  initEventListeners,
  setRendererRef as setRendererRefEvent,
  setStatusEl as setStatusElEvent,
  setRuntimeInput as setRuntimeInputEvent,
  setRealTimeModeInput as setRealTimeModeInputEvent,
  setSimSolverInput as setSimSolverInputEvent,
  setSimToleranceInputs as setSimToleranceInputsEvent,
  setSimStopFcnInput as setSimStopFcnInputEvent,
  setInspectorBody,
  setRotateSelectionBtn,
  setMarginLoopSelect,
  setMarginOutputText,
  setLockedInput as setLockedInputEvent,
  setRenderInspector as setRenderInspectorEvent,
} from './event-handlers.js';
import { initGridManager } from './grid-manager.js';
import {
  initContextMenu,
  setContextMenus,
  setStateRef as setContextMenuStateRef,
  setRendererRef as setContextMenuRendererRef,
  setFitToDiagram as setContextMenuFitToDiagram,
  setRenderInspector as setContextMenuRenderInspector,
  setFocusPropertiesPanel as setContextMenuFocusPropertiesPanel,
  setSvg as setContextMenuSvg,
  setOnGenerateBlockCode as setContextMenuOnGenerateBlockCode,
} from './context-menu-manager.js';
import { addRecentFile } from './recent-files.js';
import { mountSidebarPanels } from './components/sidebar-panels.js';
import { mountStatusBar } from './components/status-bar.js';
import { mountLayout } from './components/mount-layout.js';
import { setStatusElement, setProgressElement } from './status-service.js';
import { mountModals } from './components/modals.js';
import { mountContextMenus } from './components/context-menus.js';
import * as FieldNames from '../FieldNames.js';
import { initI18n, localizeDOM, t, getLocale } from '../utils/i18n.js';
import { getCurrentLocale } from './host-utils.js';
import { installNFlowNativeMenu, refreshNFlowNativeMenu } from './native-menu.js';
import { bindDocumentViewPreferences } from './document-view-preferences.js';
import { bindNewDiagramDefaults } from './new-diagram-defaults.js';
import { loadUiPrefs } from './ui-prefs.js';
import { installDashboardBindingEvents } from './dashboard-bindings.js';
import { emitAppEvent, onAppEvent } from './app-events.js';
import { applyStartupTheme } from './startup-theme.js';
import { initVariablesPanel } from './variables-panel.js';
import { startLibraries } from './library-startup.js';
import {
  applyExternalModelChange,
  installHighlightHandler,
  loadStartupModel,
} from './startup-model-load.js';
import { generateBlockCode } from './block-codegen.js';
import { fitToDiagram } from './fit-to-diagram.js';
//=============================================================================
installDashboardBindingEvents();
try {
  (window as any).__nf_serializeDiagram = () => toJSON(serializeDiagram(state));
  (window as any).__nf_runSimulationForTest = async (options?: { realTime?: boolean }) => {
    const { diagram } = buildDiagramAndScopeMap(state, runtimeInput);
    // mux/demux are real vector-signal blocks in the engine: send as-is.
    const sendDiagram = diagram;
    if (options && typeof options.realTime === 'boolean') {
      (sendDiagram as Record<string, unknown>).realTime = options.realTime;
    }
    return nflowRpc<string>('nflow.sim.run', {
      diagramJson: JSON.stringify(sendDiagram),
    });
  };
} catch (_e) {
  /* hostless test environment */
}
//=============================================================================
let svg: SVGSVGElement | null = null;
let blockLayer: HTMLElement | null = null;
let wireLayer: HTMLElement | null = null;
let overlayLayer: HTMLElement | null = null;
let renderer: any = null;
const rendererRef: { current: any | null } = { current: null };
let blockLibraryGroups: HTMLElement | null = null;
let statusEl: HTMLElement | null = null;
let diagramNameInput: HTMLInputElement | null = null;
let runtimeInput: HTMLInputElement | null = null;
let simDt: HTMLInputElement | null = null;
let realTimeModeInput: HTMLSelectElement | null = null;
let autoRouteInput: HTMLInputElement | null = null;
let variablesInput: HTMLTextAreaElement | null = null;
let variablesPreview: HTMLElement | null = null;
let authorInput: HTMLInputElement | null = null;
let licenseInput: HTMLInputElement | null = null;
let commentsInput: HTMLInputElement | null = null;
let subsystemUpBtn: HTMLElement | null = null;
let marginLoopSelect: HTMLSelectElement | null = null;
let marginOutputText: HTMLElement | null = null;
let fileOpenInput: HTMLInputElement | null = null;
let fileSaveAsBtn: HTMLElement | null = null;
let saveSubsystemBtn: HTMLElement | null = null;
let rotateSelectionBtn: HTMLButtonElement | null = null;
let homeBtn: HTMLElement | null = null;
let zoomInBtn: HTMLElement | null = null;
let zoomOutBtn: HTMLElement | null = null;
let renderInspector: any = () => {};
let inspectorBody: HTMLElement | null = null;
const GRID_SIZE = 20;
function formatModelTimestamp(value: string): string {
  if (!value || /^\d{4}-\d{2}-\d{2}$/.test(value)) return value;
  const date = new Date(value);
  return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
}
//=============================================================================
function syncModelMetadataUi(): void {
  const values: Record<string, string> = {
    modelVersionInput: state.modelVersion || '1.0.0',
    createdByInput: state.createdBy || '',
    createdInput: formatModelTimestamp(state.created),
    lastSavedByInput: state.lastSavedBy || '',
    lastSavedAtInput: formatModelTimestamp(state.lastSavedAt),
  };
  Object.entries(values).forEach(([id, value]) => {
    const input = document.getElementById(id) as HTMLInputElement | null;
    if (input) input.value = value;
  });
}
//=============================================================================
const onOpenSubsystemHandler = (block: any) => {
  try {
    void import('./subsystem-handlers.js')
      .then((m) => m.openSubsystemFromBlock(block))
      .catch((err) => {
        console.error('[OpenSubsystem] openSubsystemFromBlock error', err);
      });
  } catch (err) {
    console.error('[OpenSubsystem] openSubsystemFromBlock error', err);
  }
};
//=============================================================================
const onSaveSubsystemHandler = async (block: any) => {
  try {
    saveSubsystemFromBlock(block);
  } catch (err) {
    console.error('[SaveSubsystem] error', err);
  }
};
//=============================================================================
const onGenerateBlockCodeHandler = (block: any) => generateBlockCode(block, statusEl);
let statusBarInfo: HTMLElement | null = null;
let statusBarTime: HTMLElement | null = null;
let statusBarZoom: HTMLElement | null = null;
let statusBarBlocks: HTMLElement | null = null;
let statusBarConnections: HTMLElement | null = null;
const updateStatusBar = (info?: string | null, time?: number | null, zoom?: number | null) => {
  if (statusBarInfo && info) statusBarInfo.textContent = info;
  if (statusBarTime && time !== undefined && time !== null)
    statusBarTime.textContent = `${time.toFixed(2)}s`;
  if (statusBarZoom && zoom !== undefined && zoom !== null)
    statusBarZoom.textContent = `${Math.round(zoom * 100)}%`;
  if (statusBarBlocks) statusBarBlocks.textContent = `${state.blocks.size} blocks`;
  if (statusBarConnections)
    statusBarConnections.textContent = `${state.connections.length} connections`;
};
//=============================================================================
const focusPropertiesPanel = () => {
  emitAppEvent('nf-inspector-show', { tab: 'properties' });
};
//=============================================================================
const updateStabilityPanel: () => void = () => {};
export async function init(): Promise<void> {
  try {
    // Without a host (static page) there is no user to ask for: do not wait
    // for a transport that will never come.
    const hostUser = canUseNFlowRpc('nflow.host.user')
      ? await nflowRpc<{ user?: string }>('nflow.host.user')
      : null;
    state.currentUser = String(hostUser?.user || '').trim();
    if (!state.author) state.author = state.currentUser;
    if (!state.createdBy) state.createdBy = state.currentUser;
  } catch (_e) {
    state.currentUser = '';
  }
  await applyStartupTheme();

  // Ensure the DOM layout exists before mounting UI components.
  try {
    mountLayout();
    installNFlowNativeMenu();
  } catch (e) {
    /* ignore layout mount errors */
  }

  // Load host-backed preferences before mounting components that consume them.
  try {
    await loadUiPrefs();
  } catch (_e) {
    /* localStorage fallback remains */
  }

  mountSidebarPanels();
  bindNewDiagramDefaults();
  mountStatusBar();
  mountModals();
  mountContextMenus();
  try {
    const navigation = await import('./canvas-navigation-tools.js');
    navigation.restoreCanvasNavigationTools();
  } catch (_e) {
    /* view controls remain visible */
  }
  bindDocumentViewPreferences();
  try {
    const simulationToolbar = await import('./simulation-toolbar-visibility.js');
    simulationToolbar.restoreSimulationToolbar();
  } catch (_e) {
    /* simulation toolbar remains visible */
  }
  try {
    const { mountConsolePanel } = await import('../shell/console-host.js');
    mountConsolePanel();
  } catch (_e) {
    /* console panel is optional - never block startup */
  }

  // Ensure all form controls have `name` and `id` attributes to satisfy
  // browser autofill and static analysis tools that flag missing identifiers.
  try {
    document
      .querySelectorAll<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>(
        'input:not([name]), select:not([name]), textarea:not([name])'
      )
      .forEach((el, idx) => {
        try {
          const dataEdit = el.getAttribute('data-edit');
          const existingId = el.getAttribute('id');
          const base = dataEdit || existingId || 'field';
          if (!el.getAttribute('name')) el.setAttribute('name', String(base));
          if (!existingId)
            el.setAttribute('id', `global-${String(base).replace(/[^a-zA-Z0-9_-]/g, '')}-${idx}`);
        } catch (_e) {}
      });
  } catch (_e) {}

  svg = document.getElementById('svgCanvas') as SVGSVGElement | null;
  blockLayer = document.getElementById('blockLayer');
  wireLayer = document.getElementById('wireLayer');
  overlayLayer = document.getElementById('overlayLayer');
  blockLibraryGroups = document.getElementById('blockLibraryGroups');
  statusBarInfo = document.getElementById('statusBarInfo');
  statusBarTime = document.getElementById('statusBarTime');
  statusBarZoom = document.getElementById('statusBarZoom');
  statusBarBlocks = document.getElementById('statusBarBlocks');
  statusBarConnections = document.getElementById('statusBarConnections');
  setStatusElement(statusBarInfo);
  setProgressElement(document.getElementById('statusBarProgress'));
  subsystemUpBtn = document.getElementById('subsystemUpBtn');
  marginLoopSelect = document.getElementById('marginLoopSelect') as HTMLSelectElement | null;
  marginOutputText = document.getElementById('marginOutputText');
  fileOpenInput = document.getElementById('fileOpenInput') as HTMLInputElement | null;
  fileSaveAsBtn = document.getElementById('fileSaveAs');
  saveSubsystemBtn = document.getElementById('saveSubsystem');
  rotateSelectionBtn = document.getElementById('rotateSelection') as HTMLButtonElement | null;
  homeBtn = document.getElementById('homeBtn');
  zoomInBtn = document.getElementById('zoomInBtn');
  zoomOutBtn = document.getElementById('zoomOutBtn');
  // '#status' exists only in test fixtures; fall back to statusBarInfo so that
  // legacy modules writing statusEl.textContent still show messages at runtime.
  statusEl = document.getElementById('status') ?? statusBarInfo;
  if (document.body.dataset.uiPreferencesErrorBound !== '1') {
    document.body.dataset.uiPreferencesErrorBound = '1';
    onAppEvent('nf-ui-prefs-error', () => {
      writeStatus(statusEl, t('Could not save UI preferences.'), 'error');
    });
  }
  diagramNameInput = document.getElementById('diagramName') as HTMLInputElement | null;
  runtimeInput = document.getElementById('runtimeInput') as HTMLInputElement | null;
  simDt = document.getElementById('simDt') as HTMLInputElement | null;
  realTimeModeInput = document.getElementById('realTimeMode') as HTMLSelectElement | null;
  const simSolverInput = document.getElementById('simSolver') as HTMLSelectElement | null;
  const simRelTolInput = document.getElementById('simRelTol') as HTMLInputElement | null;
  const simAbsTolInput = document.getElementById('simAbsTol') as HTMLInputElement | null;
  const simMaxStepInput = document.getElementById('simMaxStep') as HTMLInputElement | null;
  const solverToleranceGroup = document.getElementById('solverToleranceGroup');
  const simStopFcnInput = document.getElementById('simStopFcn') as HTMLTextAreaElement | null;
  autoRouteInput = document.getElementById('autoRouteInput') as HTMLInputElement | null;
  variablesInput = document.getElementById('variablesInput') as HTMLTextAreaElement | null;
  variablesPreview = document.getElementById('variablesPreview');
  const lockedInput = document.getElementById('lockedInput') as HTMLInputElement | null;
  authorInput = document.getElementById('authorInput') as HTMLInputElement | null;
  licenseInput = document.getElementById('licenseInput') as HTMLInputElement | null;
  commentsInput = document.getElementById('commentsInput') as HTMLInputElement | null;
  inspectorBody = document.getElementById('inspectorBody');
  syncModelMetadataUi();
  onAppEvent('nf-model-metadata-change', syncModelMetadataUi);

  setRendererRef(rendererRef);
  setRendererRefDiagram(rendererRef);
  setRendererRefLibrary(rendererRef);
  setSubsystemUpBtn(subsystemUpBtn);
  setDiagramNameInput(diagramNameInput);
  setRuntimeInput(runtimeInput);
  setSimDt(simDt);
  setRealTimeModeInput(realTimeModeInput);
  setRealTimeModeInputDiagram(realTimeModeInput);
  setAutoRouteInput(autoRouteInput);
  setVariablesInput(variablesInput);
  setVariablesPreview(variablesPreview);
  setLockedInput(lockedInput);
  setStatusEl(statusEl);
  setStatusElLibrary(statusEl);
  initMenuHandlers();
  setBlockLibraryGroups(blockLibraryGroups);
  setBlockLibrary([]);
  setGridSize(GRID_SIZE);
  setSvg(svg);
  setUpdateStatusBar(updateStatusBar);
  setStatusElRef(statusEl);
  setHomeBtnRef(homeBtn);
  setZoomInBtnRef(zoomInBtn);
  setZoomOutBtnRef(zoomOutBtn);

  setHomeBtnMenu(homeBtn);
  setZoomInBtnMenu(zoomInBtn);
  setZoomOutBtnMenu(zoomOutBtn);

  setRendererRefEvent(rendererRef);
  setStatusElEvent(statusEl);
  setRuntimeInputEvent(runtimeInput);
  setRealTimeModeInputEvent(realTimeModeInput);
  setSimSolverInputEvent(simSolverInput);
  setSimToleranceInputsEvent(simRelTolInput, simAbsTolInput, simMaxStepInput, solverToleranceGroup);
  setSimStopFcnInputEvent(simStopFcnInput);
  setLockedInputEvent(lockedInput);
  setInspectorBody(inspectorBody);
  setRotateSelectionBtn(rotateSelectionBtn);
  setMarginLoopSelect(marginLoopSelect);
  setMarginOutputText(marginOutputText);

  setFitToDiagram(fitToDiagram);
  setUpdateStabilityPanel(updateStabilityPanel);

  try {
    document.body.classList.toggle('diagram-locked', Boolean(state.locked));
  } catch (_e) {}
  try {
    syncLockedUi();
  } catch (_e) {}

  setContextMenuStateRef(state as any);
  setContextMenuRendererRef(rendererRef as any);
  setContextMenuFitToDiagram(fitToDiagram);
  setContextMenuSvg(svg);

  let historyInitPromise: Promise<any> = Promise.resolve();
  if (svg) {
    try {
      const renderMod = await import('../render/render.js');
      if (renderMod && typeof renderMod.createRenderer === 'function') {
        renderer = renderMod.createRenderer({
          svg,
          blockLayer,
          wireLayer,
          overlayLayer,
          state,
          onSelectBlock: (blockId: any) => {
            renderInspector(blockId);
            focusPropertiesPanel();
            updateStabilityPanel();
          },
          onSelectConnection: (connectionId: any) => {
            renderInspector(connectionId);
            focusPropertiesPanel();
            updateStabilityPanel();
          },
          onOpenSubsystem: onOpenSubsystemHandler,
          onSaveSubsystem: onSaveSubsystemHandler,
          onConnectionError: (message: string) => {
            writeStatus(statusEl, message, 'error');
          },
        });
        rendererRef.current = renderer;
      }
    } catch (e) {}
    try {
      const qi = await import('./quick-insert.js');
      qi.setRendererRef(rendererRef);
      qi.initQuickInsert(svg);
    } catch (_e) {
      /* optional feature - never block startup */
    }
    try {
      const wl = await import('./wire-labels.js');
      wl.initWireLabels(svg);
      wl.refreshWireLabels();
    } catch (_e) {
      /* optional feature - never block startup */
    }
    try {
      const qc = await import('./quick-connect.js');
      qc.setRendererRef(rendererRef);
      qc.initQuickConnect(svg);
    } catch (_e) {
      /* optional feature - never block startup */
    }
    try {
      const dd = await import('./dup-drag.js');
      dd.setRendererRef(rendererRef);
      dd.initCtrlDragDuplicate(svg);
    } catch (_e) {
      /* optional feature - never block startup */
    }
    try {
      const mm = await import('./minimap.js');
      mm.mountMinimap();
    } catch (_e) {
      /* optional feature - never block startup */
    }
    try {
      initSignalBadges(svg);
    } catch (_e) {
      /* optional feature - never block startup */
    }
    try {
      onAppEvent('nf-error-blocks', (detail) => {
        try {
          const ids = detail?.ids;
          if (ids && ids.length) {
            rendererRef.current?.setLoopHighlight?.(ids, []);
          } else {
            rendererRef.current?.setLoopHighlight?.(null, null);
          }
        } catch (_e) {
          /* ignore */
        }
      });
    } catch (_e) {
      /* ignore */
    }
    try {
      onAppEvent('nf-select-block', (detail) => {
        try {
          const blockId = String(detail?.blockId || '');
          if (!blockId || !state.blocks.has(blockId)) return;
          const requestedIds = detail?.blockIds;
          const blockIds = Array.isArray(requestedIds)
            ? requestedIds.map(String).filter((id) => state.blocks.has(id))
            : [];
          if (blockIds.length > 1) {
            rendererRef.current?.setMultiSelection?.(new Set(blockIds), new Set());
          } else {
            rendererRef.current?.selectBlock?.(blockId);
          }
          const block = state.blocks.get(blockId) as {
            x?: number;
            y?: number;
            width?: number;
            height?: number;
          };
          const vb = getViewBox();
          if (block && vb && Number.isFinite(vb.w) && Number.isFinite(vb.h)) {
            const cx = (Number(block.x) || 0) + (Number(block.width) || 0) / 2;
            const cy = (Number(block.y) || 0) + (Number(block.height) || 0) / 2;
            setViewBox({ x: cx - vb.w / 2, y: cy - vb.h / 2, w: vb.w, h: vb.h });
          }
        } catch (_e) {
          /* ignore */
        }
      });
    } catch (_e) {
      /* ignore */
    }
    historyInitPromise = import('./history.js')
      .then((m: any) => {
        try {
          if (m && typeof m.initHistory === 'function') m.initHistory({ renderer: rendererRef });
        } catch (e) {}
      })
      .catch(() => {});
  }

  const __inspector_opts: any = {
    inspectorBody,
    rotateSelectionBtn,
    renderer: rendererRef,
    renderScope,
    signalDiagramChanged,
    onOpenSubsystem: onOpenSubsystemHandler,
    onSaveSubsystem: onSaveSubsystemHandler,
    onGenerateBlockCode: onGenerateBlockCodeHandler,
    getRuntimeSeconds: () => {
      const value = Number(runtimeInput?.value);
      return Number.isFinite(value) ? value : 0;
    },
  };

  renderInspector = createInspector(__inspector_opts).renderInspector;

  // Apply name/id attributes to any form fields added during startup to
  // suppress DevTools warnings about form fields without `id`/`name`.
  try {
    applyDataEditNames(document.body as unknown as HTMLElement, 'app');
  } catch (_e) {}

  setRenderInspectorEvent(renderInspector);
  setContextMenuRenderInspector(renderInspector);
  setContextMenuFocusPropertiesPanel(focusPropertiesPanel);
  setContextMenuOnGenerateBlockCode(onGenerateBlockCodeHandler);

  if (inspectorBody) {
    inspectorBody.addEventListener('input', (event: Event) => {
      const target = event.target as Element | null;
      if (!target || !(target instanceof HTMLElement)) return;
      if ((target as HTMLElement).dataset.edit !== FieldNames.kExpr) return;
    });
  }

  updateSubsystemNavUi();
  updateStatusBar('Ready', 0, 1);

  window.addEventListener('updateStatusBar', () => {
    updateStatusBar();
  });

  try {
    window.addEventListener('diagramLoaded', () => {
      try {
        syncLockedUi();
      } catch (_e) {}
    });
  } catch (_e) {}

  if (diagramNameInput) {
    diagramNameInput.value = state.diagramName;
    diagramNameInput.addEventListener('input', () => {
      state.diagramName = (diagramNameInput as HTMLInputElement).value.trim() || 'nflow';
      emitAppEvent('nf-diagram-name-change', { name: state.diagramName });
    });
  }
  if (authorInput) {
    authorInput.value = state.author || '';
    authorInput.addEventListener('input', () => {
      state.author = (authorInput as HTMLInputElement).value || '';
    });
  }
  if (licenseInput) {
    licenseInput.value = state.license || '';
    licenseInput.addEventListener('input', () => {
      state.license = (licenseInput as HTMLInputElement).value || '';
    });
  }
  if (commentsInput) {
    commentsInput.value = state.comments || '';
    commentsInput.addEventListener('input', () => {
      state.comments = (commentsInput as HTMLInputElement).value || '';
    });
  }
  if (marginLoopSelect) {
    marginLoopSelect.addEventListener('change', () => {
      state.selectedLoopKey = (marginLoopSelect as HTMLSelectElement).value || null;
      updateStabilityPanel();
    });
  }
  updateStabilityPanel();

  window.addEventListener('diagramChanged', updateStabilityPanel);

  initVariablesPanel(variablesInput, variablesPreview, statusEl);

  const libPromise = startLibraries();

  // Do not block app readiness on library loading - let libraries load
  // in the background so the main UI can appear faster.
  await Promise.allSettled([historyInitPromise]);
  emitAppEvent('nf-ready');
  void libPromise.catch(() => {}).finally(() => emitAppEvent('nf-libraries-ready'));

  initViewBox();

  if (fileOpenInput) {
    fileOpenInput.addEventListener('change', () => {
      const file = fileOpenInput!.files?.[0];
      if (!file) return;
      const reader = new FileReader();
      reader.onload = async () => {
        try {
          const text = String(reader.result || '');
          const data = parseDiagram(text, file.name);
          await Promise.resolve((window as any).__nf_lib_ready_promise).catch(() => undefined);
          clearDocumentDiagnostics();
          await loadDiagram(data);
          const curPath = file.name;
          setCurrentFilePath(curPath);
          try {
            addRecentFile(curPath);
          } catch (_e) {}
          writeStatus(statusEl, t('Loaded: {fileName}', { fileName: file.name }));
        } catch (error) {
          writeStatus(
            statusEl,
            t('Load error: {error}', {
              error: (error as any)?.message || String(error),
            })
          );
        }
      };
      reader.readAsText(file);
      if (fileOpenInput) fileOpenInput.value = '';
    });
  }

  if (fileSaveAsBtn) {
    fileSaveAsBtn.addEventListener('click', () => {
      const dataObj = serializeDiagramForSave(state);
      const json = toJSON(dataObj);
      const blob = new Blob([json], { type: 'application/json' });
      const filename = `${sanitizeFilename(state.diagramName)}.nflow`;
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = url;
      link.download = filename;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      URL.revokeObjectURL(url);
      commitSerializedSaveMetadata(state, dataObj);
      writeStatus(statusEl, t('Saved diagram'));
    });
  }

  if (saveSubsystemBtn) {
    saveSubsystemBtn.addEventListener('click', () => {
      try {
        handleSaveAsSubsystem();
      } catch (err) {
        writeStatus(statusEl, t('Error opening subsystem save dialog'));
      }
    });
  }

  const menuTrigger = document.querySelector('.menu-trigger');
  const menu = document.querySelector('.menu');

  if (menuTrigger && menu) {
    menuTrigger.addEventListener('click', (e) => {
      e.stopPropagation();
      (menu as HTMLElement).classList.toggle('active');
    });

    document.addEventListener('click', () => {
      (menu as HTMLElement).classList.remove('active');
    });

    menu.addEventListener('click', (e) => {
      e.stopPropagation();
    });
  }

  const blockContextMenu = document.getElementById('blockContextMenu');
  const canvasContextMenu = document.getElementById('canvasContextMenu');
  setContextMenus(blockContextMenu as HTMLElement | null, canvasContextMenu as HTMLElement | null);

  await initGridManager();
  initEventListeners();
}
//=============================================================================
async function initUI() {
  initSidebarUI();
  initZoomButtons();
  initModals();
  initContextMenu();
  await loadStartupModel(statusEl);
  initModelSession({
    serialize: () => toJSON(serializeDiagramForSave(state)),
    onModelChanged: () => applyExternalModelChange(statusEl),
  });
  installHighlightHandler();
  // window.resizeTo() is blocked by browsers/WebViews on non-popup windows.
  // Dispatching a synthetic resize event is sufficient to force layout
  // recalculation for any listener that depends on window dimensions.
  requestAnimationFrame(() => {
    window.dispatchEvent(new Event('resize'));
  });
}
//=============================================================================
/** Surface a startup failure in the loading overlay instead of hanging. */
export function reportStartupError(err: unknown): void {
  // eslint-disable-next-line no-console
  console.error('[nflow] startup failed:', err);
  try {
    const statusEl = document.getElementById('nf-loading-status');
    if (statusEl) {
      const message = (err as Error | null)?.message ?? String(err);
      statusEl.textContent = `Startup failed: ${message}`;
      statusEl.classList.add('nf-loading-error');
    }
    const spinner = document.querySelector('.nf-loading-spinner') as HTMLElement | null;
    if (spinner) spinner.style.display = 'none';
    document.documentElement.classList.add('nf-startup-failed');
  } catch (_e) {
    /* the console.error above is the last resort */
  }
}
//=============================================================================
/** Full application startup. Exported so tests can drive and observe it. */
export async function startApp(): Promise<void> {
  // Start i18n loading in the background - do NOT await it before init()
  // so that locale-file network requests never block app startup.
  void (async () => {
    try {
      let locale = 'en_US';

      // Prefer locale pre-seeded in sessionStorage by the C++ bootstrap page,
      // fall back to localStorage for older installs or non-bootstrap hosts.
      try {
        const sess = sessionStorage.getItem('nf-i18n-locale');
        if (sess) {
          locale = sess;
        } else {
          const cached = localStorage.getItem('nf-i18n-locale');
          if (cached) locale = cached;
        }
      } catch (_e) {}

      // Fallback: the host helper reads session/local storage and any
      // bootstrapped values, without depending on a host binding.
      if (locale === 'en_US') {
        try {
          const detected = await getCurrentLocale();
          if (detected) locale = detected;
        } catch (_e) {}
      }

      try {
        await initI18n(locale || 'en_US');
        localizeDOM(document);
        refreshNFlowNativeMenu();
      } catch (_e) {
        // ignore i18n failures
      }

      // Host bindings can appear after startup in embedded hosts: re-check the
      // locale and reload translations if it changed.
      try {
        const maxRetries = 5;
        for (let attempt = 0; attempt < maxRetries; attempt++) {
          try {
            const actual = await getCurrentLocale();
            if (typeof actual === 'string' && actual && actual !== getLocale()) {
              try {
                await initI18n(actual);
                localizeDOM(document);
                refreshNFlowNativeMenu();
              } catch (_e) {}
              break;
            }
          } catch (_e) {}
          await new Promise((r) => setTimeout(r, 200));
        }
      } catch (_e) {}
    } catch (_e) {}
  })();

  try {
    await init();
    await initUI();
  } catch (err) {
    reportStartupError(err);
  }
}
//=============================================================================
// Start as soon as the DOM is ready. No requestAnimationFrame gating: rAF never
// fires in non-composited tabs (hidden panes, background windows). Unit tests
// import this module without booting and drive init()/startApp() themselves.
const IS_TEST_ENV =
  typeof import.meta !== 'undefined' &&
  (import.meta as unknown as { env?: { MODE?: string } }).env?.MODE === 'test';
if (!IS_TEST_ENV) {
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => void startApp());
  } else {
    void startApp();
  }
}
//=============================================================================
export {};
//=============================================================================
