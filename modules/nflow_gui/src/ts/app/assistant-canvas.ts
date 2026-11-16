//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { loadDiagram } from './diagram-handlers.js';
import { parseDiagram } from './file-operations.js';
import { nflowRpc } from './native-rpc.js';
import { setBoundModel, getBoundModelName } from './model-session.js';
import { showConfirmSaveModal } from './modal-handlers.js';
import { handleMenuAction } from './menu-handlers.js';
import { state } from './state.js';
import { writeStatus } from './status-service.js';
import { t } from '../utils/i18n.js';
//=============================================================================
// Build the model an assistant code block describes and draw it on the CURRENT
// NFlow canvas, instead of opening a separate window. The script runs on the
// live interpreter (scripted API), then the resulting diagram is pulled from the
// model registry and loaded in place. Re-running is safe (a prior build of the
// same model is closed first), and nflow_layout keeps the blocks arranged.
//=============================================================================
// nflow.model.getDiagram answers over the string-result channel: nflowRpc hands
// back the envelope as a JSON STRING (a blob reference is resolved to its text
// for a large model), never a ready object, so it must be parsed. The inner
// `content` is itself the model document serialized as a JSON string.
interface GetDiagramResult {
  success?: boolean;
  content?: string;
  revision?: number;
}
//=============================================================================
async function fetchDiagram(name: string): Promise<GetDiagramResult | undefined> {
  const raw = await nflowRpc<string>('nflow.model.getDiagram', { name });
  try {
    return JSON.parse(String(raw ?? '{}')) as GetDiagramResult;
  } catch {
    return undefined;
  }
}
//=============================================================================
// A model built by the scripted API starts with new_system('<name>'); pull that
// name so the host can fetch and lay out exactly that model.
function modelNameFromScript(code: string): string | undefined {
  const match = code.match(/new_system\s*\(\s*'([^']+)'/);
  return match ? match[1] : undefined;
}
//=============================================================================
// Drop open_system/save_system calls: the host draws the model in the open
// canvas, so a second window or a file write would be redundant.
function stripWindowCalls(code: string): string {
  return code.replace(/^\s*(?:open_system|save_system)\s*\([^;]*\);?\s*$/gm, '');
}
//=============================================================================
async function runLiveScript(code: string): Promise<void> {
  await nflowRpc('nflow.callback.invoke', { code, blockId: '' });
}
//=============================================================================
const errText = (err: unknown): string => (err instanceof Error ? err.message : String(err));
//=============================================================================
// The main window status bar, where load/save outcomes already appear, so the
// build reports there too (visible under the assistant pane) and no longer fails
// silently when a script the model wrote is rejected by the engine.
function statusBar(): HTMLElement | null {
  return typeof document !== 'undefined' ? document.getElementById('status') : null;
}
//=============================================================================
// A build failed. When the pane gave an `ask` callback (auto-correction), hand
// the engine error back to the agent so it fixes its own script and replies with
// a corrected code block; otherwise just surface the error in the status bar.
function reportBuildFailure(error: string, requestFix?: (prompt: string) => void): void {
  if (requestFix) {
    writeStatus(statusBar(), t('Build failed; asking the assistant to fix it...'), 'warn');
    requestFix(
      t(
        'The build failed with this error: {error}. Fix the model script and reply with a single corrected code block.',
        { error }
      )
    );
    return;
  }
  writeStatus(statusBar(), t('Could not build the model: {error}', { error }), 'error');
}
//=============================================================================
// Building replaces the whole canvas, so unsaved work on it would be lost. When
// the canvas is dirty, ask first (the same Save / Don't save / Cancel dialog the
// rest of the editor uses): Cancel keeps the canvas and skips the build, Save
// persists the current work first, Don't save discards it. A clean canvas builds
// straight away.
async function confirmReplaceCanvas(): Promise<boolean> {
  if (!state.dirty) {
    return true;
  }
  return new Promise<boolean>((resolve) => {
    showConfirmSaveModal((shouldSave: boolean | null) => {
      if (shouldSave === null) {
        resolve(false);
        return;
      }
      if (shouldSave === true) {
        // Fire the editor's own Save (it snapshots the content synchronously, so
        // the later canvas replace cannot corrupt what gets written).
        handleMenuAction('save');
      }
      resolve(true);
    });
  });
}
//=============================================================================
// Pull the just-built (or just-edited) model from the registry and draw it on the
// current canvas, re-binding the window to it (so edits commit to it and Save
// targets it). Returns false only when the model exists but the in-place sync
// failed. parseDiagram validates the format and yields the loader's shape, the
// same path a model file open takes.
async function refreshCanvas(name: string): Promise<boolean> {
  try {
    const result = await fetchDiagram(name);
    if (result && result.success === true && typeof result.content === 'string') {
      await loadDiagram(parseDiagram(result.content, name));
      setBoundModel(name, Number(result.revision) || 0);
      return true;
    }
  } catch (_e) {
    // The model is in the registry; only the in-place canvas sync failed.
  }
  return false;
}
//=============================================================================
export async function buildModelOnCanvas(
  code: string,
  requestFix?: (prompt: string) => void
): Promise<void> {
  const name = modelNameFromScript(code);
  const body = stripWindowCalls(code);

  if (name) {
    // A full model build (new_system ...): it replaces the whole canvas, so guard
    // unsaved work first. Re-running the same request fails because the model is
    // already loaded; in that one case close it and rebuild once.
    if (!(await confirmReplaceCanvas())) {
      return;
    }
    writeStatus(statusBar(), t('Building the model {name} on the canvas...', { name }), 'info');
    try {
      await runLiveScript(body);
    } catch (err) {
      const message = errText(err);
      if (/load/i.test(message)) {
        await runLiveScript(`bdclose('${name}');`).catch(() => undefined);
        try {
          await runLiveScript(body);
        } catch (retryErr) {
          reportBuildFailure(errText(retryErr), requestFix);
          return;
        }
      } else {
        reportBuildFailure(message, requestFix);
        return;
      }
    }
    if (await refreshCanvas(name)) {
      writeStatus(statusBar(), t('Model {name} is on the canvas.', { name }), 'success');
    } else {
      writeStatus(
        statusBar(),
        t('The model was built but could not be shown on the canvas.'),
        'warn'
      );
    }
    return;
  }

  // No new_system: an incremental edit of the model already on the canvas (the
  // assistant iterating: set_param / add_block / add_line / delete_* on the bound
  // model). Apply it in place and refresh, without wiping or re-confirming.
  const bound = getBoundModelName();
  if (bound) {
    writeStatus(statusBar(), t('Updating the model {name}...', { name: bound }), 'info');
    try {
      await runLiveScript(body);
    } catch (err) {
      reportBuildFailure(errText(err), requestFix);
      return;
    }
    if (await refreshCanvas(bound)) {
      writeStatus(statusBar(), t('Model {name} updated.', { name: bound }), 'success');
    } else {
      writeStatus(
        statusBar(),
        t('The model was updated but could not be shown on the canvas.'),
        'warn'
      );
    }
    return;
  }

  // No model to build on: run the snippet as-is, still reporting the outcome so a
  // rejected snippet does not look like nothing happened.
  writeStatus(statusBar(), t('Running assistant code...'), 'info');
  try {
    await runLiveScript(body);
    writeStatus(statusBar(), t('Assistant code finished.'), 'success');
  } catch (err) {
    writeStatus(statusBar(), t('Assistant code failed: {error}', { error: errText(err) }), 'error');
  }
}
//=============================================================================
