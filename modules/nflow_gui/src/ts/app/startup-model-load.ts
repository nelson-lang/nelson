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
import { canUseNFlowRpc, nflowRpc, onNFlowPush } from './native-rpc.js';
import { writeStatus } from './status-service.js';
import { clearDocumentDiagnostics } from './diagnostics-service.js';
import { state, setCurrentFilePath, clearDirty } from './state.js';
import { isModelBound, fetchModelDiagram } from './model-session.js';
import { parseDiagram } from './file-operations.js';
import { loadDiagram } from './diagram-handlers.js';
import { addRecentFile } from './recent-files.js';
import { t } from '../utils/i18n.js';
//=============================================================================
async function tryToLoadBoundModel(statusEl: HTMLElement | null): Promise<boolean> {
  if (!isModelBound()) return false;
  const model = await fetchModelDiagram();
  if (!model || !model.content) return false;
  if ((window as any).__nf_lib_ready_promise) {
    try {
      await (window as any).__nf_lib_ready_promise;
    } catch (_e) {}
  }
  const curPath = model.filePath || '';
  const data = parseDiagram(model.content, curPath);
  clearDocumentDiagnostics();
  await loadDiagram(data);
  setCurrentFilePath(curPath);
  // A freshly loaded document is clean; edits after this mark it dirty again.
  clearDirty();
  if (curPath) {
    try {
      addRecentFile(curPath);
    } catch (_e) {}
  }
  writeStatus(statusEl, t('Loaded: {fileName}', { fileName: model.name || curPath }));
  return true;
}
//=============================================================================
// Conflict policy A: a scripted mutation reloads the editor only when it has no
// divergent unsaved edits; otherwise the local edits are kept (not clobbered).
export async function applyExternalModelChange(statusEl: HTMLElement | null): Promise<void> {
  if (state.dirty) {
    writeStatus(
      statusEl,
      t('Model changed by a script; save or discard your edits to see the update.')
    );
    return;
  }
  const model = await fetchModelDiagram();
  if (!model || !model.content) return;
  const curPath = model.filePath || '';
  const data = parseDiagram(model.content, curPath);
  clearDocumentDiagnostics();
  await loadDiagram(data);
  setCurrentFilePath(curPath);
  clearDirty();
  writeStatus(statusEl, t('Model updated from script.'));
}
//=============================================================================
// Briefly flash a block when the scripted API calls nflow_highlight.
export function installHighlightHandler(): void {
  try {
    const styleId = 'nf-highlight-style';
    if (!document.getElementById(styleId)) {
      const style = document.createElement('style');
      style.id = styleId;
      // drop-shadow works on both HTML and SVG block nodes.
      style.textContent =
        '.nf-highlight-flash{filter:drop-shadow(0 0 6px #ffcc00) drop-shadow(0 0 10px #ffcc00);}';
      document.head.appendChild(style);
    }
  } catch (_e) {
    /* ignore */
  }
  onNFlowPush('nflow.highlight', (payload: unknown) => {
    const path = String((payload as { block?: string })?.block || '');
    if (!path) return;
    const leaf = path.includes('/') ? path.slice(path.lastIndexOf('/') + 1) : path;
    if (!leaf) return;
    let el: Element | null = null;
    try {
      const sel = (window as any).CSS?.escape ? (window as any).CSS.escape(leaf) : leaf;
      el =
        document.querySelector(`[data-block-id="${sel}"]`) ||
        document.querySelector(`[data-id="${sel}"]`);
    } catch (_e) {
      el = null;
    }
    if (!el) return;
    el.classList.add('nf-highlight-flash');
    setTimeout(() => {
      try {
        el?.classList.remove('nf-highlight-flash');
      } catch (_e) {
        /* ignore */
      }
    }, 1200);
  });
}
//=============================================================================
// Opens the bound model, else the file the host was launched with.
export async function loadStartupModel(statusEl: HTMLElement | null): Promise<void> {
  try {
    if (await tryToLoadBoundModel(statusEl)) return;
    if (!canUseNFlowRpc('nflow.file.loadInitial')) return;
    // Apply a 5-second timeout so a slow or unresponsive host never hangs initUI.
    const loadResult = await Promise.race([
      nflowRpc<string>('nflow.file.loadInitial'),
      new Promise<never>((_, reject) => setTimeout(() => reject(new Error('load-timeout')), 5000)),
    ]);
    const resp = JSON.parse(loadResult);
    if (resp && resp.success && resp.content) {
      // Block templates must be registered before the diagram can be rendered.
      // Wait for the full library-ready promise (discover + load + refreshBlockTemplates).
      if ((window as any).__nf_lib_ready_promise) {
        try {
          await (window as any).__nf_lib_ready_promise;
        } catch (_e) {}
      }
      const rawPath = String(resp.filePath || '');
      const fileName = String(resp.fileName || '');
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
      const data = parseDiagram(resp.content, curPath);
      clearDocumentDiagnostics();
      await loadDiagram(data);
      setCurrentFilePath(curPath);
      try {
        addRecentFile(curPath);
      } catch (_e) {}
      writeStatus(statusEl, t('Loaded: {fileName}', { fileName: String(resp.fileName || '') }));
    }
  } catch (_e) {
    // ignore - non-fatal
  }
}
//=============================================================================
