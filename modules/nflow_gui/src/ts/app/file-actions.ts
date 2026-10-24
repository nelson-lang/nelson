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
import { state } from './state.js';
import { toJSON, serializeDiagram, parseDiagram, sanitizeFilename } from './file-operations.js';
import { loadDiagram } from './diagram-handlers.js';
import { clearDocumentDiagnostics } from './diagnostics-service.js';
import { t } from '../utils/i18n.js';
import { nflowRpc } from './native-rpc.js';
import { chooseDirectoryViaDialog } from './file-dialogs.js';
import { renderBlockLibrary } from './block-library-handlers.js';
import { _setStatus, buildCodegenDiagramPayload, buildSubsystemSpec } from './event-context.js';
import type { EventRefs } from './event-context.js';
//=============================================================================
export function initFileActions(refs: EventRefs) {
  const { statusEl, simDt } = refs;
  const codegenBtn = document.getElementById('codegenBtn');
  const codegenLang = document.getElementById('codegenLang') as HTMLSelectElement | null;
  const codegenIncludeMain = document.getElementById(
    'codegenIncludeMain'
  ) as HTMLInputElement | null;
  const codegenBuildAfterGen = document.getElementById(
    'codegenBuildAfterGen'
  ) as HTMLInputElement | null;
  const debugLog = document.getElementById('debugLog');
  const _errorBox = document.getElementById('errorBox');
  void _errorBox;
  const saveBtn = document.getElementById('saveBtn');
  const loadBtn = document.getElementById('loadBtn');
  const loadInput = document.getElementById('loadInput') as HTMLInputElement | null;
  const loadSubsystemBtn = document.getElementById('loadSubsystemBtn');
  const loadSubsystemInput = document.getElementById(
    'loadSubsystemInput'
  ) as HTMLInputElement | null;

  if (codegenBtn) {
    codegenBtn.addEventListener('click', async () => {
      const lang = (codegenLang?.value as string) || 'c';
      const includeMain = codegenIncludeMain ? codegenIncludeMain.checked : true;
      const build = codegenBuildAfterGen ? codegenBuildAfterGen.checked : false;
      const diagram = buildCodegenDiagramPayload();
      const diagramBlocks = Array.isArray(diagram.blocks) ? diagram.blocks : [];
      const diagramConnections = Array.isArray(diagram.connections) ? diagram.connections : [];
      const sampleTimeVal = simDt ? Number((simDt as HTMLInputElement).value) : 0.01;
      if (debugLog)
        debugLog.textContent = [
          '[codegen] start',
          `lang=${lang}`,
          `includeMain=${includeMain}`,
          `sampleTime=${sampleTimeVal}`,
          `blocks=${diagramBlocks.length} connections=${diagramConnections.length}`,
        ].join('\n');
      try {
        const diagramJson = JSON.stringify(diagram);
        const optionsJson = JSON.stringify({ sampleTime: sampleTimeVal, includeMain, lang, build });
        try {
          const choose = (await chooseDirectoryViaDialog()) as unknown as Record<string, unknown>;
          if (!choose || choose['success'] !== true) {
            if (debugLog) debugLog.textContent += '\n[codegen] canceled';
            return;
          }
          const destDir = String(choose['path'] || '');
          if (build) {
            if (debugLog) debugLog.textContent += '\n[codegen] building...';
            _setStatus(statusEl, t('Building...'), 'info');
          }
          const genRaw = await nflowRpc<string>('nflow.codegen.generateToDir', {
            diagramJson,
            optionsJson,
            destinationDirectory: destDir,
          });
          const genRes = JSON.parse(genRaw) as Record<string, unknown>;
          const genWarnings = Array.isArray(genRes['warnings'])
            ? (genRes['warnings'] as unknown[]).map((w) => String(w))
            : [];
          for (const w of genWarnings) {
            if (debugLog) debugLog.textContent += `\n[codegen] warning: ${w}`;
          }
          if (genRes['success'] === true) {
            const outDir = String(genRes['dir'] || destDir);
            if (debugLog) debugLog.textContent += `\n[codegen] saved to ${outDir}`;
            if (genRes['buildSuccess'] === true) {
              if (debugLog) debugLog.textContent += '\n[codegen] build succeeded';
              _setStatus(statusEl, t('Build succeeded'), 'success');
            } else if (genRes['buildSuccess'] === false) {
              const buildMsg = String(genRes['buildError'] || 'unknown build error');
              if (debugLog) debugLog.textContent += `\n[codegen] build failed: ${buildMsg}`;
              _setStatus(statusEl, t('Build failed: {message}', { message: buildMsg }), 'error');
            } else {
              _setStatus(statusEl, t('Saved: {path}', { path: outDir }), 'success');
            }
          } else {
            const msg = String(genRes['error'] || 'unknown');
            if (debugLog) debugLog.textContent += `\n[codegen] error: ${msg}`;
            _setStatus(statusEl, t('Codegen error: {message}', { message: msg }), 'error');
          }
        } catch (_error) {
          const message = (_error as any)?.message || _error;
          if (debugLog) debugLog.textContent += `\n[codegen] error: ${message}`;
          _setStatus(statusEl, t('Codegen error: {message}', { message }), 'error');
        }
      } catch (_error) {
        const message = (_error as any)?.message || _error;
        if (debugLog) debugLog.textContent += `\n[codegen] error: ${message}`;
        _setStatus(statusEl, t('Codegen error: {message}', { message }), 'error');
      }
    });
  }

  if (saveBtn) {
    saveBtn.addEventListener('click', () => {
      const dataObj = serializeDiagram(state);
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
      _setStatus(statusEl, t('Saved diagram'), 'success');
    });
  }

  if (loadBtn && loadInput) {
    loadBtn.addEventListener('click', () => loadInput.click());
    loadInput.addEventListener('change', () => {
      const file = loadInput.files?.[0];
      if (!file) return;
      const reader = new FileReader();
      reader.onload = async () => {
        try {
          const text = String(reader.result || '');
          const data = parseDiagram(text, file.name);
          clearDocumentDiagnostics();
          await loadDiagram(data);
          _setStatus(statusEl, t('Loaded diagram'), 'success');
        } catch (_error) {
          _setStatus(
            statusEl,
            t('Load error: {error}', { error: (_error as any)?.message || _error }),
            'error'
          );
        }
      };
      reader.readAsText(file);
      loadInput.value = '';
    });
  }

  if (loadSubsystemBtn && loadSubsystemInput) {
    loadSubsystemBtn.addEventListener('click', () => loadSubsystemInput.click());
    loadSubsystemInput.addEventListener('change', () => {
      const file = loadSubsystemInput.files?.[0];
      if (!file) return;
      const fileName = file.name;
      const ext = String(fileName).trim().toLowerCase().split('.').pop() || '';
      if (ext !== 'nblock') {
        _setStatus(
          statusEl,
          t('Invalid file type: {fileName}. Only .nblock files are supported for subsystems.', {
            fileName,
          }),
          'error'
        );
        loadSubsystemInput.value = '';
        return;
      }
      const fileKey = fileName.replace(/\.(nblock)$/i, '').toLowerCase();
      if (state.loadedSubsystems.has(fileKey)) {
        _setStatus(statusEl, t('Subsystem already loaded: {fileName}', { fileName }), 'warn');
        loadSubsystemInput.value = '';
        return;
      }
      const reader = new FileReader();
      reader.onload = () => {
        try {
          const text = String(reader.result || '');
          const data = parseDiagram(text, file.name);
          const baseName = fileName.replace(/\.(nblock)$/i, '') || 'Subsystem';
          const spec = buildSubsystemSpec(data, baseName);
          const key = sanitizeFilename(spec.name).toLowerCase() || fileKey;
          state.loadedSubsystems.set(key, spec);
          renderBlockLibrary();
          _setStatus(statusEl, t('Loaded subsystem: {name}', { name: spec.name }), 'success');
        } catch (_error) {
          _setStatus(
            statusEl,
            t('Subsystem load error: {error}', { error: (_error as any)?.message || _error }),
            'error'
          );
        }
      };
      reader.readAsText(file);
      loadSubsystemInput.value = '';
    });
  }
}
//=============================================================================
