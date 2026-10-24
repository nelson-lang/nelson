//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { state } from './state.js';
import { serializeDiagram, serializeNBlock, sanitizeFilename } from './file-operations.js';
import { collectExternalPorts } from '../utils/subsystem-ports.js';
import * as FieldNames from '../FieldNames.js';
import { t } from '../utils/i18n.js';
import { pushStatus } from './status-service.js';
import { CONFIRM_SAVE_EVENT, MISSING_BLOCKS_EVENT } from './components/modal-events.js';
import { canUseNativeFiles, saveViaDialog } from './file-dialogs.js';
//=============================================================================
/** Dual-write: keeps legacy statusEl write for tests AND notifies the service. */
function _setStatus(
  el: HTMLElement | null,
  msg: string,
  level: 'info' | 'warn' | 'error' | 'success' = 'info',
  durationMs = 3000
): void {
  if (el) el.textContent = msg;
  pushStatus({ level, message: msg, durationMs });
}
//=============================================================================
let statusEl: HTMLElement | null = null;
let saveInProgress = false;
declare global {
  interface Window {
    showSaveFilePicker?: (options?: {
      suggestedName?: string;
      types?: Array<{ description?: string; accept?: Record<string, string[]> }>;
      excludeAcceptAllOption?: boolean;
    }) => Promise<FileSystemFileHandle>;
  }

  interface FileSystemFileHandle {
    createWritable(): Promise<FileSystemWritableFileStream>;
  }

  interface FileSystemWritableFileStream {
    write(data: Blob | string): Promise<void>;
    close(): Promise<void>;
  }
}
//=============================================================================
export function setStatusEl(el: HTMLElement | null): void {
  statusEl = el;
}
//=============================================================================
export function handleSaveAsSubsystem(): void {
  if (saveInProgress) {
    _setStatus(statusEl, t('Save already in progress'), 'warn');
    return;
  }
  saveInProgress = true;
  const blocks = Array.from(state.blocks.values());
  const externalInputs = collectExternalPorts(blocks, FieldNames.kLabelSource);
  const externalOutputs = collectExternalPorts(blocks, FieldNames.kLabelSink);

  if (!externalInputs.length && !externalOutputs.length) {
    _setStatus(
      statusEl,
      t(
        "Cannot save as subsystem: No external ports found. Add labelSource (input) or labelSink (output) blocks and mark them as 'Is external port'."
      ),
      'error'
    );
    saveInProgress = false;
    return;
  }

  // The payload is computed synchronously so the fallback download (URL.createObjectURL)
  // happens without waiting for extra microtasks.
  try {
    const diagramData = serializeDiagram(state);
    const inputPorts = collectExternalPorts(
      Array.from(state.blocks.values()),
      FieldNames.kLabelSource
    ).map((p) => ({ name: String(((p as Record<string, unknown>).label as unknown) || '') }));
    const outputPorts = collectExternalPorts(
      Array.from(state.blocks.values()),
      FieldNames.kLabelSink
    ).map((p) => ({ name: String(((p as Record<string, unknown>).label as unknown) || '') }));
    const masked = Boolean((diagramData as Record<string, unknown>)?.masked);
    const nblockData = serializeNBlock(
      diagramData as Record<string, unknown>,
      inputPorts,
      outputPorts,
      masked,
      {
        description: state.comments || '',
        author: state.author || '',
      }
    );
    const jsonForDialog = JSON.stringify(nblockData, null, 2);
    const filename = `${sanitizeFilename(state.diagramName)}.nblock`;

    if (canUseNativeFiles()) {
      void (async () => {
        try {
          const result = await saveViaDialog({
            typeOfFile: 'nblock',
            content: jsonForDialog,
            defaultName: filename,
          });
          if (result.success === true) {
            _setStatus(
              statusEl,
              t('Saved subsystem: {name}', { name: state.diagramName }),
              'success'
            );
            return;
          }
          _setStatus(statusEl, t('Save canceled or failed'), 'warn');
          return;
        } catch {
          _setStatus(statusEl, t('Save failed: native dialog error'), 'error');
          return;
        } finally {
          saveInProgress = false;
        }
      })();
      return;
    }

    // Browser download fallback, synchronous.
    try {
      const blob = new Blob([jsonForDialog], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const link = typeof document !== 'undefined' ? document.createElement('a') : null;
      if (link) {
        link.href = url;
        link.download = filename;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      }
      URL.revokeObjectURL(url);
      _setStatus(statusEl, t('Saved subsystem: {name}', { name: state.diagramName }), 'success');
      return;
    } catch {
      _setStatus(statusEl, t('Save failed: fallback download error'), 'error');
      return;
    }
  } catch (err) {
    _setStatus(statusEl, t('Error saving subsystem'), 'error');
  } finally {
    saveInProgress = false;
  }
}
//=============================================================================
export function showConfirmSaveModal(callback: (result: boolean | null) => void): void {
  if (typeof document !== 'undefined') {
    const event = new CustomEvent(CONFIRM_SAVE_EVENT, {
      cancelable: true,
      detail: { callback },
    });
    document.dispatchEvent(event);
    if (event.defaultPrevented) return;
  }

  callback(false);
}
//=============================================================================
export function saveSubsystemFromBlock(block: unknown): void {
  const blk = block as Record<string, unknown> | null;
  if (!blk || (blk as Record<string, unknown>).type !== FieldNames.kSubsystem) return;
  if (saveInProgress) {
    _setStatus(statusEl, t('Save already in progress'), 'warn');
    return;
  }
  saveInProgress = true;

  void (async () => {
    try {
      let spec = (blk.params as Record<string, unknown> | undefined)?.subsystem as
        | Record<string, unknown>
        | undefined;

      // A missing or malformed spec is saved as an empty subsystem so the dialog still opens.
      if (!spec || !Array.isArray(spec.blocks) || !Array.isArray(spec.connections)) {
        const fallbackName = String(
          (blk.params as Record<string, unknown> | undefined)?.name || 'subsystem'
        );
        spec = { name: fallbackName, blocks: [], connections: [] };
      }

      const inputPorts = collectExternalPorts(
        spec.blocks as unknown as unknown[],
        FieldNames.kLabelSource
      ).map((p) => ({
        name: String(((p as Record<string, unknown>).name as unknown) || ''),
      }));
      const outputPorts = collectExternalPorts(
        spec.blocks as unknown as unknown[],
        FieldNames.kLabelSink
      ).map((p) => ({
        name: String(((p as Record<string, unknown>).name as unknown) || ''),
      }));

      const masked = Boolean((blk.params as Record<string, unknown> | undefined)?.masked);

      const nblockData = serializeNBlock(
        spec as Record<string, unknown>,
        inputPorts,
        outputPorts,
        masked,
        {
          description: String(
            (blk.params as Record<string, unknown> | undefined)?.description || ''
          ),
          author: String((blk.params as Record<string, unknown> | undefined)?.author || ''),
          maskParameters: (blk.params as Record<string, unknown> | undefined)?.maskParameters as
            | Record<string, unknown>
            | undefined,
        }
      );
      const jsonForDialog = JSON.stringify(nblockData, null, 2);

      const suggestedName = String(
        (blk.params as Record<string, unknown> | undefined)?.name ||
          (spec as Record<string, unknown>)?.name ||
          'subsystem'
      );
      const filename = `${sanitizeFilename(suggestedName)}.nblock`;

      // With native files available there is no browser fallback.
      try {
        if (canUseNativeFiles()) {
          try {
            const result = await saveViaDialog({
              typeOfFile: 'nblock',
              content: jsonForDialog,
              defaultName: filename,
            });
            if (result.success === true) {
              _setStatus(
                statusEl,
                t('Saved subsystem: {name}', { name: suggestedName }),
                'success'
              );
              return;
            }
            _setStatus(statusEl, t('Save canceled or failed'), 'warn');
            return;
          } catch {
            _setStatus(statusEl, t('Save failed: native dialog error'), 'error');
            return;
          }
        }
        try {
          const blob = new Blob([jsonForDialog], { type: 'application/json' });
          const url = URL.createObjectURL(blob);
          const link = typeof document !== 'undefined' ? document.createElement('a') : null;
          if (link) {
            link.href = url;
            link.download = filename;
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
          }
          URL.revokeObjectURL(url);
          _setStatus(statusEl, t('Saved subsystem: {name}', { name: suggestedName }), 'success');
          return;
        } catch {
          _setStatus(statusEl, t('Save failed: fallback download error'), 'error');
          return;
        }
      } catch {
        _setStatus(statusEl, t('Save failed: error checking native dialog'), 'error');
        return;
      }
    } catch (err) {
      _setStatus(statusEl, t('Cannot save: Subsystem missing or malformed'), 'error');
    } finally {
      saveInProgress = false;
    }
  })();
}
//=============================================================================
export function showMissingBlocksModal(
  missingBlocks: { type: string; libraryId?: string; libraryTitle?: string }[]
): void {
  if (typeof document !== 'undefined') {
    const event = new CustomEvent(MISSING_BLOCKS_EVENT, {
      cancelable: true,
      detail: { missingBlocks },
    });
    document.dispatchEvent(event);
    if (event.defaultPrevented) return;
  }

  const list = missingBlocks
    .map((b) => (b.libraryTitle ? `${b.type} (library: ${b.libraryTitle})` : b.type))
    .join('\n');
  // eslint-disable-next-line no-alert
  alert(`Cannot open diagram.\nMissing block types:\n${list}`);
}
//=============================================================================
export default null;
//=============================================================================
