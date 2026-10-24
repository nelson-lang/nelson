//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { state, clearDirty, getCurrentFilePath, setCurrentFilePath } from './state.js';
import { writeStatus } from './status-service.js';
import { clearDocumentDiagnostics } from './diagnostics-service.js';
import { buildTitleName } from './title-manager.js';
import {
  toJSON,
  serializeDiagram,
  serializeDiagramForSave,
  commitSerializedSaveMetadata,
  commitSerializedSaveMetadataFromText,
  parseDiagram,
  sanitizeFilename,
} from './file-operations.js';
import { showConfirmSaveModal, handleSaveAsSubsystem } from './modal-handlers.js';
import { loadDiagram, updateSubsystemNavUi, syncLockedUi } from './diagram-handlers.js';
import { clearWorkspace } from './workspace-handlers.js';
import { exportCanvasSVG } from './export-handlers.js';
import { t } from '../utils/i18n.js';
import {
  getRecentFiles,
  addRecentFile,
  clearRecentFiles,
  ensureRecentFilesLoaded,
} from './recent-files.js';
import { ABOUT_SHOW_EVENT, OPEN_MODEL_PATH_EVENT } from './components/modal-events.js';
import { renderOpenRecentMenu } from './components/open-recent-menu.js';
import { modeToRealTime } from './real-time-mode.js';
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
import { getNFlowHostMode } from './host-mode.js';
import { isModelBound, saveModel, flushPendingCommit } from './model-session.js';
import { openExampleBrowser } from './example-browser.js';
import { saveViaDialog, openViaDialog, openFilePicker } from './file-dialogs.js';
import { resetUiPrefs } from './ui-prefs.js';
import { readNewDiagramDefaults } from './new-diagram-defaults.js';
import { openBlockHelp } from './block-help.js';
import { emitAppEvent } from './app-events.js';
import {
  hideMenubarDropdowns,
  openNFlowWithBrowserPicker,
  saveNFlowWithBrowserDownload,
  saveTextWithBrowserPicker,
  selectedBlockFromState,
} from './browser-file-operations.js';
//=============================================================================
let statusEl: HTMLElement | null = null;
let _fileOpenInput: HTMLElement | null = null;
void _fileOpenInput;
let deleteSelectionBtn: HTMLElement | null = null;
let homeBtn: HTMLElement | null = null;
let zoomInBtn: HTMLElement | null = null;
let zoomOutBtn: HTMLElement | null = null;
let diagramNameInput: HTMLInputElement | null = null;
let runtimeInput: HTMLInputElement | null = null;
let simDt: HTMLInputElement | null = null;
let realTimeModeInput: HTMLSelectElement | null = null;
let autoRouteInput: HTMLInputElement | null = null;
let variablesInput: HTMLInputElement | HTMLTextAreaElement | null = null;
let variablesPreview: HTMLElement | null = null;
export function setStatusEl(el: HTMLElement | null): void {
  statusEl = el;
}
//=============================================================================
export function setFileOpenInput(el: HTMLElement | null): void {
  _fileOpenInput = el;
}
//=============================================================================
export function setDeleteSelectionBtn(el: HTMLElement | null): void {
  deleteSelectionBtn = el;
}
//=============================================================================
export function setHomeBtn(el: HTMLElement | null): void {
  homeBtn = el;
}
//=============================================================================
export function setZoomInBtn(el: HTMLElement | null): void {
  zoomInBtn = el;
}
//=============================================================================
export function setZoomOutBtn(el: HTMLElement | null): void {
  zoomOutBtn = el;
}
//=============================================================================
export function setDiagramNameInput(el: HTMLInputElement | null): void {
  diagramNameInput = el;
}
//=============================================================================
export function setRuntimeInput(el: HTMLInputElement | null): void {
  runtimeInput = el;
}
//=============================================================================
export function setSimDt(el: HTMLInputElement | null): void {
  simDt = el;
}
//=============================================================================
export function setRealTimeModeInput(el: HTMLSelectElement | null): void {
  realTimeModeInput = el;
}
//=============================================================================
export function setAutoRouteInput(el: HTMLInputElement | null): void {
  autoRouteInput = el;
}
//=============================================================================
export function setVariablesInput(el: HTMLInputElement | HTMLTextAreaElement | null): void {
  variablesInput = el;
}
//=============================================================================
export function setVariablesPreview(el: HTMLElement | null): void {
  variablesPreview = el;
}
//=============================================================================
export async function performSave(
  content: string,
  defaultName: string
): Promise<Record<string, unknown>> {
  if (
    (canUseNFlowRpc('nflow.host.mode') && (await getNFlowHostMode()) === 'web') ||
    !canUseNFlowRpc('nflow.file.writeText')
  ) {
    return saveTextWithBrowserPicker({
      content,
      defaultName,
      extension: '.nflow',
      mimeType: 'application/json',
      description: 'NFlow model',
      onSaved: (fileName) => {
        commitSerializedSaveMetadataFromText(state, content);
        clearDirty();
        writeStatus(statusEl, t('Saved: {path}', { path: fileName }));
      },
    });
  }
  // For a window bound to a live registry document, persist through
  // the shared document (the host writes the doc to its real file path and
  // clears its dirty flag) so save_system/bdIsDirty stay coherent. Never a temp.
  if (isModelBound()) {
    try {
      await flushPendingCommit();
      const boundPath = getCurrentFilePath() || '';
      const res = await saveModel(content, boundPath || undefined);
      if (res['success'] === true) {
        const savedPath = String(res['filePath'] || boundPath);
        commitSerializedSaveMetadataFromText(state, content);
        if (savedPath) {
          setCurrentFilePath(savedPath);
          try {
            addRecentFile(savedPath);
          } catch (_e) {}
        }
        clearDirty();
        writeStatus(statusEl, t('Saved: {path}', { path: savedPath }));
        return res;
      }
      if (res['needPath'] === true) {
        // No file target yet (never-saved model): Save As, then sync the doc's
        // path and clear its dirty flag through the shared document.
        const asRes = await performSaveAs(content, defaultName);
        if (asRes['success'] === true) {
          const savedPath = getCurrentFilePath() || '';
          if (savedPath) await saveModel(content, savedPath);
        }
        return asRes;
      }
    } catch (err: unknown) {
      writeStatus(
        statusEl,
        t('Save error: {error}', { error: (err as Error)?.message || String(err) })
      );
    }
    return performSaveAs(content, defaultName);
  }
  const path = getCurrentFilePath();
  if (path) {
    try {
      const raw = await nflowRpc<string>('nflow.file.writeText', { content, path });
      const result = JSON.parse(raw) as Record<string, unknown>;
      if (result['success'] === true) {
        commitSerializedSaveMetadataFromText(state, content);
        clearDirty();
        writeStatus(statusEl, t('Saved: {path}', { path }));
        return result;
      }
      // The bound file is not writable (e.g. a shipped example under a read-only
      // install) -- fall back to Save As so the user can save a copy elsewhere.
      writeStatus(statusEl, t('Cannot save to {path}, choose another location.', { path }));
    } catch (err: unknown) {
      writeStatus(
        statusEl,
        t('Save error: {error}', {
          error: (err as Error)?.message || String(err),
        })
      );
    }
    return performSaveAs(content, defaultName);
  }
  return performSaveAs(content, defaultName);
}
//=============================================================================
export async function performSaveAs(
  content: string,
  defaultName: string
): Promise<Record<string, unknown>> {
  if (
    (canUseNFlowRpc('nflow.host.mode') && (await getNFlowHostMode()) === 'web') ||
    !canUseNFlowRpc('nflow.file.writeText')
  ) {
    return saveTextWithBrowserPicker({
      content,
      defaultName,
      extension: '.nflow',
      mimeType: 'application/json',
      description: 'NFlow model',
      onSaved: (fileName) => {
        commitSerializedSaveMetadataFromText(state, content);
        clearDirty();
        writeStatus(statusEl, t('Saved: {path}', { path: fileName }));
      },
    });
  }
  try {
    const result = (await saveViaDialog({
      typeOfFile: 'nflow',
      content,
      defaultName,
    })) as unknown as Record<string, unknown>;
    if (result['success'] === true) {
      commitSerializedSaveMetadataFromText(state, content);
      const rawPath = String(result['filePath'] || '');
      const fileName = String(result['fileName'] || '');
      let savedPath = rawPath || fileName;
      try {
        if (rawPath && fileName) {
          if (rawPath.endsWith(fileName)) savedPath = rawPath;
          else
            savedPath =
              rawPath.endsWith('/') || rawPath.endsWith('\\')
                ? rawPath + fileName
                : rawPath + '/' + fileName;
        }
      } catch (_e) {}
      setCurrentFilePath(savedPath);
      try {
        addRecentFile(savedPath);
      } catch (_e) {}
      clearDirty();
      writeStatus(statusEl, t('Saved: {path}', { path: savedPath }));
    } else if (result['canceled'] !== true) {
      // A refused write must not stay silent: the status bar tells the user why.
      writeStatus(
        statusEl,
        t('Save error: {error}', { error: String(result['error'] || 'Cannot write file') })
      );
    }
    return result as Record<string, unknown>;
  } catch (err: unknown) {
    writeStatus(
      statusEl,
      t('Save error: {error}', {
        error: (err as Error)?.message || String(err),
      })
    );
    return { success: false } as Record<string, unknown>;
  }
}
//=============================================================================
// Browser runtime: Save writes to the engine /workspace; this hands the model
// to the user's machine through the browser's save picker, or a download when
// the picker is unavailable or its write fails. Failures are reported.
export async function exportModelToComputer(): Promise<void> {
  const content = toJSON(serializeDiagramForSave(state));
  const defaultName = `${sanitizeFilename(state.diagramName)}.nflow`;
  try {
    const result = await saveTextWithBrowserPicker({
      content,
      defaultName,
      extension: '.nflow',
      mimeType: 'application/json',
      description: 'NFlow model',
    });
    if (result.success) {
      writeStatus(
        statusEl,
        t('Exported {fileName} to this computer', {
          fileName: String(result.fileName ?? defaultName),
        })
      );
      return;
    }
    if (result.canceled) return;
    // The picker accepted but the write failed: a plain download still works.
    saveNFlowWithBrowserDownload(content, defaultName, (fileName) =>
      writeStatus(statusEl, t('Exported {fileName} to this computer', { fileName }))
    );
  } catch (error) {
    writeStatus(
      statusEl,
      t('Save error: {error}', { error: (error as Error)?.message || String(error) })
    );
  }
}
//=============================================================================
// Browser runtime: copy a local .nflow into the engine /workspace (persistent,
// visible in the desktop Files panel), then open it from there so Save writes
// back in place.
export async function importModelFromComputer(): Promise<void> {
  if (typeof document === 'undefined') return;
  const input = document.createElement('input');
  input.type = 'file';
  input.accept = '.nflow,application/json';
  input.hidden = true;
  document.body.appendChild(input);
  const file = await new Promise<File | null>((resolve) => {
    input.addEventListener('change', () => resolve(input.files?.[0] ?? null), { once: true });
    input.addEventListener('cancel', () => resolve(null), { once: true });
    input.click();
  });
  input.remove();
  if (!file) return;
  try {
    const content = await file.text();
    parseDiagram(content, file.name); // validate before touching the workspace
    const name = sanitizeFilename(file.name.replace(/\.nflow$/i, '')) + '.nflow';
    const path = `/workspace/${name}`;
    if (canUseNFlowRpc('nflow.file.writeText')) {
      const raw = await nflowRpc<string>('nflow.file.writeText', { path, content });
      const res = JSON.parse(String(raw || '{}')) as { success?: boolean; error?: string };
      if (!res.success) throw new Error(res.error || 'write failed');
      await openModelFromPath(path);
      writeStatus(statusEl, t('Imported {fileName} into {path}', { fileName: file.name, path }));
      return;
    }
    // No engine filesystem: load in memory only.
    clearDocumentDiagnostics();
    await loadDiagram(parseDiagram(content, file.name));
    setCurrentFilePath(file.name);
    writeStatus(statusEl, t('Loaded: {fileName}', { fileName: file.name }));
  } catch (error) {
    writeStatus(
      statusEl,
      t('Load error: {error}', { error: (error as Error)?.message || String(error) })
    );
  }
}
//=============================================================================
export async function performOpen(): Promise<void> {
  if (canUseNFlowRpc('nflow.host.mode') && (await getNFlowHostMode()) === 'web') {
    openNFlowWithBrowserPicker();
    return;
  }
  if (!canUseNFlowRpc('nflow.file.readText')) {
    openNFlowWithBrowserPicker();
    return;
  }
  try {
    const result = (await openViaDialog({ extension: '.nflow' })) as unknown as Record<
      string,
      unknown
    >;
    if (result['success'] === true) {
      const contentStr = String(result['content'] || '');
      const rawPath = String(result['filePath'] || '');
      const fileName = String(result['fileName'] || '');
      let curPath = rawPath || fileName;
      try {
        if (rawPath && fileName) {
          if (rawPath.endsWith(fileName)) curPath = rawPath;
          else
            curPath =
              rawPath.endsWith('/') || rawPath.endsWith('\\')
                ? rawPath + fileName
                : rawPath + '/' + fileName;
        }
      } catch (_e) {}
      const data = parseDiagram(contentStr, curPath);
      clearDocumentDiagnostics();
      await loadDiagram(data);
      setCurrentFilePath(curPath);
      try {
        addRecentFile(curPath);
      } catch (_e) {}
      clearDirty();
      writeStatus(
        statusEl,
        t('Loaded: {fileName}', {
          fileName: String(result['fileName'] || ''),
        })
      );
      try {
        document.title = buildTitleName(state.diagramName);
      } catch (_e) {}
    }
  } catch (error: unknown) {
    writeStatus(
      statusEl,
      t('Load error: {error}', {
        error: (error as Error)?.message || String(error),
      })
    );
  }
}
//=============================================================================
// Open a model by absolute path (no dialog): read it through nflow.file.readText,
// load it onto the canvas and bind it as the current file, so a later Save writes
// in place. Shared by Open Recent and the Example Browser. It runs in the app
// instance that owns the menu/Save handlers, which keeps currentFilePath consistent.
export async function openModelFromPath(path: string): Promise<void> {
  if (!path) return;
  if (!canUseNFlowRpc('nflow.file.readText')) {
    writeStatus(statusEl, t('Cannot open recent file in this host'));
    return;
  }
  try {
    const raw = await nflowRpc<string>('nflow.file.readText', { path });
    const resp = JSON.parse(String(raw || '{}')) as Record<string, unknown>;
    if (resp && resp['success'] === true && typeof resp['content'] === 'string') {
      const data = parseDiagram(String(resp['content'] || ''), path);
      clearDocumentDiagnostics();
      await loadDiagram(data);
      setCurrentFilePath(path);
      clearDirty();
      try {
        addRecentFile(path);
      } catch (_e) {
        /* recent-files is best-effort */
      }
      try {
        document.title = buildTitleName(state.diagramName);
      } catch (_e) {
        /* title is cosmetic */
      }
      writeStatus(statusEl, t('Loaded: {fileName}', { fileName: path }));
    } else {
      writeStatus(
        statusEl,
        t('Load error: {error}', { error: String(resp['error'] || 'Cannot open file') })
      );
    }
  } catch (e: unknown) {
    writeStatus(statusEl, t('Load error: {error}', { error: (e as Error)?.message || String(e) }));
  }
}
//=============================================================================
// Binds the document listener that lets any part of the UI (e.g. the Example
// Browser, which may live in a separate bundle instance) ask the main app to open
// a model by path. A global flag makes it idempotent across duplicate module
// instances, so the open runs once.
export function initMenuHandlers(): void {
  if (typeof document === 'undefined') return;
  // The local-computer entries only make sense where Open/Save do not reach
  // the user's disk, i.e. the browser runtime.
  void getNFlowHostMode()
    .then((mode) => {
      if (mode !== 'wasm') return;
      document.getElementById('menu-local-files')?.removeAttribute('hidden');
      // Generated sources can be produced in the browser, but building them
      // needs a native toolchain: the option has no meaning here.
      const buildAfter = document.getElementById('codegenBuildAfterGen') as HTMLInputElement | null;
      if (buildAfter) {
        buildAfter.checked = false;
        const group = buildAfter.closest('.control-group') as HTMLElement | null;
        if (group) group.style.display = 'none';
      }
      // OpenModelica is a native compiler producing host-specific FMUs: neither
      // runs in the browser, so its settings have no meaning here.
      const modelica = document.getElementById('modelicaSettingsSection');
      if (modelica) modelica.style.display = 'none';
    })
    .catch(() => undefined);
  if (window.__nfOpenModelPathBound) return;
  window.__nfOpenModelPathBound = true;
  document.addEventListener(OPEN_MODEL_PATH_EVENT, (event: Event) => {
    const detail = (event as CustomEvent<{ path?: string }>).detail;
    if (detail && typeof detail.path === 'string') {
      void openModelFromPath(detail.path);
    }
  });
}
//=============================================================================
// Open an SSP archive: import it to a runnable .nflow through the native host
// (nflow.ssp.import -> NFlow.sspImport on the interpreter thread), then load the
// produced model. Requires the nflow editor host.
export async function performOpenSsp(): Promise<void> {
  try {
    if (!canUseNFlowRpc('nflow.ssp.import')) {
      writeStatus(statusEl, t('Opening an SSP requires the nflow editor host.'));
      return;
    }
    const sspPath = await openFilePicker({ mode: 'open', title: 'Open SSP', filters: ['ssp'] });
    if (!sspPath) return;
    writeStatus(statusEl, t('Importing SSP...'));
    const raw = await nflowRpc<string>('nflow.ssp.import', { path: sspPath });
    const res = JSON.parse(raw) as { success?: boolean; path?: string; error?: string };
    if (!res.success || !res.path) {
      writeStatus(
        statusEl,
        t('SSP import failed: {error}', { error: res.error || 'unknown error' })
      );
      return;
    }
    const rawRead = await nflowRpc<string>('nflow.file.readText', { path: res.path });
    const resp = JSON.parse(rawRead) as Record<string, unknown>;
    if (resp['success'] !== true) {
      writeStatus(statusEl, t('SSP imported but the model could not be read.'));
      return;
    }
    const data = parseDiagram(String(resp['content'] || ''), res.path);
    clearDocumentDiagnostics();
    await loadDiagram(data);
    setCurrentFilePath(res.path);
    try {
      addRecentFile(res.path);
    } catch (_e) {}
    clearDirty();
    writeStatus(statusEl, t('Loaded SSP: {fileName}', { fileName: sspPath }));
    try {
      document.title = buildTitleName(state.diagramName);
    } catch (_e) {}
  } catch (error: unknown) {
    writeStatus(
      statusEl,
      t('SSP open error: {error}', { error: (error as Error)?.message || String(error) })
    );
  }
}
//=============================================================================
// Export the diagram as an SSP through the native host (nflow.ssp.export ->
// NFlow.sspExportDiagram on the interpreter thread), the inverse of Open SSP: each
// FMU block becomes a Component, FMU-to-FMU wires become Connections, and To
// Workspace sinks fed by an FMU become system outputs. Requires the editor host and
// a non-empty diagram.
export async function performExportSsp(): Promise<void> {
  try {
    if (!canUseNFlowRpc('nflow.ssp.export')) {
      writeStatus(statusEl, t('Exporting an SSP requires the nflow editor host.'));
      return;
    }
    const data = serializeDiagram(state) as Record<string, unknown>;
    const blocks = Array.isArray(data.blocks)
      ? (data.blocks as Array<Record<string, unknown>>)
      : [];
    if (blocks.length === 0) {
      writeStatus(statusEl, t('Export as SSP needs a non-empty diagram.'));
      return;
    }
    // A composition of FMU blocks is repackaged directly; any other diagram is
    // compiled to a single FMU component. The native host decides (nflow_ssp_export).
    const target = await openFilePicker({
      mode: 'save',
      title: 'Export as SSP',
      suggestedName: `${sanitizeFilename(state.diagramName)}.ssp`,
      filters: ['ssp'],
    });
    if (!target) return;
    writeStatus(statusEl, t('Exporting SSP...'));
    const raw = await nflowRpc<string>('nflow.ssp.export', {
      content: toJSON(data),
      path: target,
    });
    const res = JSON.parse(raw) as { success?: boolean; path?: string; error?: string };
    if (!res.success || !res.path) {
      writeStatus(
        statusEl,
        t('SSP export failed: {error}', { error: res.error || 'unknown error' })
      );
      return;
    }
    writeStatus(statusEl, t('Exported SSP: {fileName}', { fileName: res.path }));
  } catch (error: unknown) {
    writeStatus(
      statusEl,
      t('SSP export error: {error}', { error: (error as Error)?.message || String(error) })
    );
  }
}
//=============================================================================
// Export the diagram as an FMI 3.0 Co-Simulation FMU through the native host
// (nflow.fmu.export -> NFlow.exportfmu on the interpreter thread). External ports
// (labelSource/labelSink) become the FMU inputs/outputs. Requires the editor host.
// The FMU carries a compiled binary only when a C toolchain is present, else it is
// source-only.
export async function performExportFmu(): Promise<void> {
  try {
    if (!canUseNFlowRpc('nflow.fmu.export')) {
      writeStatus(statusEl, t('Exporting an FMU requires the nflow editor host.'));
      return;
    }
    const data = serializeDiagram(state) as Record<string, unknown>;
    const blocks = Array.isArray(data.blocks)
      ? (data.blocks as Array<Record<string, unknown>>)
      : [];
    if (blocks.length === 0) {
      writeStatus(statusEl, t('Export as FMU needs a non-empty diagram.'));
      return;
    }
    const target = await openFilePicker({
      mode: 'save',
      title: 'Export as FMU',
      suggestedName: `${sanitizeFilename(state.diagramName)}.fmu`,
      filters: ['fmu'],
    });
    if (!target) return;
    writeStatus(statusEl, t('Exporting FMU...'));
    const raw = await nflowRpc<string>('nflow.fmu.export', {
      content: toJSON(data),
      path: target,
    });
    const res = JSON.parse(raw) as { success?: boolean; path?: string; error?: string };
    if (!res.success || !res.path) {
      writeStatus(
        statusEl,
        t('FMU export failed: {error}', { error: res.error || 'unknown error' })
      );
      return;
    }
    writeStatus(statusEl, t('Exported FMU: {fileName}', { fileName: res.path }));
  } catch (error: unknown) {
    writeStatus(
      statusEl,
      t('FMU export error: {error}', { error: (error as Error)?.message || String(error) })
    );
  }
}
//=============================================================================
export function newDiagram(): void {
  const defaults = readNewDiagramDefaults();
  clearDocumentDiagnostics();
  state.routeEpoch = (Number(state.routeEpoch) || 0) + 1;
  state.diagramName = 'New Diagram';
  state.modelVersion = '1.0.0';
  state.author = state.currentUser;
  state.createdBy = state.currentUser;
  state.created = new Date().toISOString();
  state.lastSavedBy = '';
  state.lastSavedAt = '';
  state.license = '';
  state.comments = '';
  const authorEl = document.getElementById('authorInput') as HTMLInputElement | null;
  const licenseEl = document.getElementById('licenseInput') as HTMLInputElement | null;
  const commentsEl = document.getElementById('commentsInput') as HTMLTextAreaElement | null;
  if (authorEl) authorEl.value = state.author;
  if (licenseEl) licenseEl.value = '';
  if (commentsEl) commentsEl.value = '';
  emitAppEvent('nf-model-metadata-change');
  if (diagramNameInput) diagramNameInput.value = state.diagramName;
  try {
    document.title = buildTitleName(state.diagramName);
  } catch (_e) {}
  if (runtimeInput) runtimeInput.value = String(defaults.runtime);
  if (simDt) {
    simDt.value = String(defaults.sampleTime);
    state.sampleTime = defaults.sampleTime;
  }
  state.realTime = modeToRealTime(defaults.execution);
  if (realTimeModeInput) realTimeModeInput.value = defaults.execution;
  state.solver = defaults.solver;
  state.relTol = defaults.relTol;
  state.absTol = defaults.absTol;
  state.maxStep = defaults.maxStep;
  const solverInput = document.getElementById('simSolver') as HTMLSelectElement | null;
  const relTolInput = document.getElementById('simRelTol') as HTMLInputElement | null;
  const absTolInput = document.getElementById('simAbsTol') as HTMLInputElement | null;
  const maxStepInput = document.getElementById('simMaxStep') as HTMLInputElement | null;
  if (solverInput) solverInput.value = defaults.solver;
  if (relTolInput) relTolInput.value = String(defaults.relTol);
  if (absTolInput) absTolInput.value = String(defaults.absTol);
  if (maxStepInput) maxStepInput.value = String(defaults.maxStep);
  state.stopFcn = '';
  const stopFcnInput = document.getElementById('simStopFcn') as HTMLTextAreaElement | null;
  if (stopFcnInput) stopFcnInput.value = '';
  state.autoRoute = defaults.autoRoute;
  if (autoRouteInput) autoRouteInput.checked = state.autoRoute as unknown as boolean;
  state.variablesText = '';
  if (variablesInput) variablesInput.value = state.variablesText;
  state.variables = {};
  state.variablesDisplay = [];
  if (variablesPreview) variablesPreview.textContent = t('No variables defined.');
  state.subsystemStack = [];
  if (typeof updateSubsystemNavUi === 'function') updateSubsystemNavUi();
  clearWorkspace();
  state.spawnIndex = 0;
  state.loadingDiagram = false;
  state.routingDirty = false;
  state.dirtyBlocks.clear();
  state.dirtyConnections.clear();
  state.selectedId = null;
  state.selectedConnection = null;
  state.selectedIds.clear();
  state.selectedConnections.clear();
  setCurrentFilePath(null);
  state.locked = false;
  try {
    syncLockedUi();
  } catch (_e) {}
  writeStatus(statusEl, t('New diagram created'));
}
//=============================================================================
export function updateSelectedBlockHelpAction(): void {
  const item = document.getElementById('selectedBlockHelpAction');
  if (!item) return;
  const enabled = selectedBlockFromState(state.blocks, state.selectedId) !== null;
  item.classList.toggle('disabled', !enabled);
  item.setAttribute('aria-disabled', String(!enabled));
}
//=============================================================================
export function handleMenuAction(action: string): void {
  switch (action) {
    case 'new':
      showConfirmSaveModal((shouldSave: boolean | null) => {
        if (shouldSave === true) {
          const dataObj = serializeDiagramForSave(state);
          const blob = new Blob([toJSON(dataObj)], { type: 'application/json' });
          const downloadName =
            getCurrentFilePath() || `${sanitizeFilename(state.diagramName)}.nflow`;
          const url = URL.createObjectURL(blob);
          const link = document.createElement('a');
          link.href = url;
          link.download = downloadName;
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
          URL.revokeObjectURL(url);
          commitSerializedSaveMetadata(state, dataObj);
          clearDirty();
          newDiagram();
        } else if (shouldSave === false) {
          newDiagram();
        }
      });
      break;
    case 'open':
      performOpen();
      break;
    case 'openExamples':
      openExampleBrowser();
      break;
    case 'openSsp':
      performOpenSsp();
      break;
    case 'importFmu':
      import('./fmu-import.js')
        .then((m) =>
          m.importFmuAsBlock((msg: string) => {
            if (statusEl && msg) statusEl.textContent = msg;
          })
        )
        .catch(() => {});
      break;
    case 'save':
      {
        const dataObj = serializeDiagramForSave(state);
        const defaultName = `${sanitizeFilename(state.diagramName)}.nflow`;
        performSave(toJSON(dataObj), defaultName);
      }
      break;
    case 'saveAs':
      performSaveAs(
        toJSON(serializeDiagramForSave(state)),
        `${sanitizeFilename(state.diagramName)}.nflow`
      );
      break;
    case 'saveAsSubsystem':
      handleSaveAsSubsystem();
      break;
    case 'exportSsp':
      performExportSsp();
      break;
    case 'exportToComputer':
      void exportModelToComputer();
      break;
    case 'importFromComputer':
      void importModelFromComputer();
      break;
    case 'exportFmu':
      performExportFmu();
      break;
    case 'exit':
      if (state.dirty) {
        showConfirmSaveModal((shouldSave: boolean | null) => {
          if (shouldSave === true) {
            const dataObj = serializeDiagramForSave(state);
            const blob = new Blob([toJSON(dataObj)], { type: 'application/json' });
            const downloadName =
              getCurrentFilePath() || `${sanitizeFilename(state.diagramName)}.nflow`;
            const url = URL.createObjectURL(blob);
            const link = document.createElement('a');
            link.href = url;
            link.download = downloadName;
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            URL.revokeObjectURL(url);
            commitSerializedSaveMetadata(state, dataObj);
            clearDirty();
          }
        });
      }
      break;
    case 'undo':
      try {
        import('./history.js').then((hist) => hist.undo()).catch(() => {});
      } catch (_e) {}
      break;
    case 'redo':
      try {
        import('./history.js').then((hist) => hist.redo()).catch(() => {});
      } catch (_e) {}
      break;
    case 'copy':
      try {
        import('./history.js').then((m) => m.copySelection && m.copySelection()).catch(() => {});
      } catch (_e) {}
      break;
    case 'cut':
      try {
        import('./history.js').then((m) => m.cutSelection && m.cutSelection()).catch(() => {});
      } catch (_e) {}
      break;
    case 'paste':
      try {
        import('./history.js').then((m) => m.pasteSelection && m.pasteSelection()).catch(() => {});
      } catch (_e) {}
      break;
    case 'delete':
      if (deleteSelectionBtn) deleteSelectionBtn.click();
      break;
    case 'autoArrange':
      try {
        import('./context-menu-manager.js')
          .then((m) => m.runAutoArrange && m.runAutoArrange())
          .catch(() => {});
      } catch (_e) {}
      break;
    case 'resetView':
      if (homeBtn) homeBtn.click();
      break;
    case 'zoomIn':
      if (zoomInBtn) zoomInBtn.click();
      break;
    case 'zoomOut':
      if (zoomOutBtn) zoomOutBtn.click();
      break;
    case 'toggleConsole':
      try {
        import('../shell/console-host.js')
          .then((m) => m.toggleConsolePanel && m.toggleConsolePanel())
          .catch(() => {});
      } catch (_e) {}
      break;
    case 'toggleMinimap':
      try {
        import('./minimap.js').then((m) => m.toggleMinimap && m.toggleMinimap()).catch(() => {});
      } catch (_e) {}
      break;
    case 'toggleViewControls':
      try {
        import('./canvas-navigation-tools.js')
          .then((m) => m.toggleCanvasNavigationTools && m.toggleCanvasNavigationTools())
          .catch(() => {});
      } catch (_e) {}
      break;
    case 'toggleSimulationToolbar':
      try {
        import('./simulation-toolbar-visibility.js')
          .then((m) => m.toggleSimulationToolbar && m.toggleSimulationToolbar())
          .catch(() => {});
      } catch (_e) {}
      break;
    case 'resetUiPreferences':
      void resetUiPrefs()
        .then(() =>
          writeStatus(statusEl, t('UI preferences reset. Defaults apply to new windows.'))
        )
        .catch((error) =>
          writeStatus(statusEl, t('Could not reset UI preferences: {message}', { message: error }))
        );
      break;
    case 'simulationSettings':
      emitAppEvent('nf-inspector-show', { tab: 'simulation' });
      break;
    case 'selectedBlockHelp': {
      const block = selectedBlockFromState(state.blocks, state.selectedId) as {
        type?: unknown;
      } | null;
      if (block) void openBlockHelp(block);
      break;
    }
    case 'about':
      document.dispatchEvent(new CustomEvent(ABOUT_SHOW_EVENT, { cancelable: true }));
      break;
    case 'export-svg':
      void exportCanvasSVG();
      break;

    default:
      // Support dynamic open-recent actions encoded as `openRecent:<encodedPath>`
      if (action && typeof action === 'string' && action.indexOf('openRecent:') === 0) {
        try {
          const path = decodeURIComponent(action.slice('openRecent:'.length));
          void openModelFromPath(path);
        } catch (_e) {
          /* malformed encoded path */
        }
        break;
      }

      if (action === 'clearRecent') {
        try {
          clearRecentFiles();
          try {
            populateOpenRecentMenu();
          } catch (_e) {}
        } catch (_e) {}
        break;
      }
  }
}
//=============================================================================
export function populateOpenRecentMenu(): void {
  try {
    const container = document.getElementById('menu-open-recent-list');
    if (!container) return;
    const recents = getRecentFiles();
    // No recents yet: try reading the preferences file once and repopulate the menu
    // after the async load. The guard avoids repeated attempts in one UI session.
    try {
      const _guardKey = '__nf_recent_files_load_attempted__';
      const attempted = (window as unknown as Record<string, unknown>)[_guardKey] as
        | boolean
        | undefined;
      if ((!recents || recents.length === 0) && !attempted) {
        try {
          (window as unknown as Record<string, unknown>)[_guardKey] = true;
          ensureRecentFilesLoaded().then(() => {
            try {
              populateOpenRecentMenu();
            } catch (_e) {}
          });
        } catch (_e) {}
      }
    } catch (_e) {}
    renderOpenRecentMenu(container, recents || [], (action) => {
      try {
        hideMenubarDropdowns();
        handleMenuAction(action);
      } catch (_err) {}
    });
  } catch (_e) {}
}
//=============================================================================
export default null;
//=============================================================================
