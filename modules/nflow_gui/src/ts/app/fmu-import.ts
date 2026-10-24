//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// FMU import assistant (editor action): reads the model description of a .fmu
// (nflow.fmi3.inspect), builds a one-block library that wraps it, writes it under
// the preferences directory and loads it into the palette. Uses existing RPCs only.
//=============================================================================
import { openFilePicker } from './file-dialogs.js';
import { nflowRpc, canUseNFlowRpc } from './native-rpc.js';
import { getPreferencesPath } from './host-utils.js';
import { parseFmuInspection, type FmuInspection } from '../libraries/fmi/fmu-inspect.js';
import { buildFmuBlockManifest, sanitizeLibraryName } from '../libraries/fmi/fmu-to-block.js';
import { IMPORT_FMU_EVENT } from './components/modal-events.js';
//=============================================================================
export interface FmuImportResult {
  ok: boolean;
  canceled?: boolean;
  message: string;
  libraryId?: string;
  blockLabel?: string;
}
//=============================================================================
// Detail carried by IMPORT_FMU_EVENT: the preview modal fills `resolve` with the
// chosen block name (or null on cancel).
export interface ImportFmuRequestDetail {
  fmuName: string;
  fmiVersion: string;
  coSimulation: boolean;
  modelExchange: boolean;
  inputNames: string[];
  outputNames: string[];
  parameterNames: string[];
  resolve: (name: string | null) => void;
}
//=============================================================================
// Resolves with the block name chosen in the preview modal, or null on cancel.
// When no modal host claims the event (headless), the default name is used.
function promptImportFmu(info: FmuInspection): Promise<string | null> {
  return new Promise((resolve) => {
    const detail: ImportFmuRequestDetail = {
      fmuName: (info.modelIdentifier || 'FMU').trim() || 'FMU',
      fmiVersion: info.fmiVersion ?? '?',
      coSimulation: info.coSimulation === true,
      modelExchange: info.modelExchange === true,
      inputNames: info.inputs.map((v) => v.name),
      outputNames: info.outputs.map((v) => v.name),
      parameterNames: info.parameters.map((p) => p.name),
      resolve,
    };
    const claimed = !document.dispatchEvent(
      new CustomEvent(IMPORT_FMU_EVENT, { detail, cancelable: true })
    );
    if (!claimed) resolve(detail.fmuName);
  });
}
/**
 * Run the FMU import flow. `notify` (optional) receives progress/status text.
 */
export async function importFmuAsBlock(notify?: (msg: string) => void): Promise<FmuImportResult> {
  const say = (m: string) => {
    try {
      notify?.(m);
    } catch {
      /* ignore */
    }
  };

  if (!canUseNFlowRpc('nflow.fmi3.inspect') || !canUseNFlowRpc('nflow.file.writeText')) {
    const message = 'FMU import needs the native host (run it in the nflow editor window).';
    say(message);
    return { ok: false, message };
  }

  const fmuPath = await openFilePicker({
    mode: 'open',
    title: 'Import FMU',
    filters: ['fmu'],
  });
  if (!fmuPath) {
    return { ok: false, canceled: true, message: '' };
  }

  say('Reading FMU…');
  let info;
  try {
    const raw = await nflowRpc<string>('nflow.fmi3.inspect', { path: fmuPath });
    info = parseFmuInspection(raw);
  } catch (err) {
    const message = `FMU inspection failed: ${(err as Error)?.message ?? String(err)}`;
    say(message);
    return { ok: false, message };
  }
  if (!info.ok) {
    const message = info.error ? `FMU inspection failed: ${info.error}` : 'FMU inspection failed.';
    say(message);
    return { ok: false, message };
  }

  const chosen = await promptImportFmu(info);
  if (chosen === null) {
    return { ok: false, canceled: true, message: '' };
  }
  const name = chosen.trim() || (info.modelIdentifier || 'FMU').trim() || 'FMU';
  const modelExchange = info.modelExchange === true && info.coSimulation !== true;
  const lib = buildFmuBlockManifest(info, { fmuPath, name, modelExchange });

  // Persist the one-block library under <prefs>/nflow-libraries/<name>/.
  const prefs = await getPreferencesPath();
  if (!prefs) {
    const message = 'Could not locate the preferences directory to store the imported block.';
    say(message);
    return { ok: false, message };
  }
  const dir = `${prefs.replace(/[\\/]+$/, '')}/nflow-libraries/${sanitizeLibraryName(name)}`;
  const libFile = `${dir}/library.json`;
  try {
    await nflowRpc('nflow.file.writeText', {
      path: libFile,
      content: JSON.stringify(lib, null, 2),
    });
  } catch (err) {
    const message = `Could not write the imported library: ${
      (err as Error)?.message ?? String(err)
    }`;
    say(message);
    return { ok: false, message };
  }

  // Load it so it appears in the palette now (the host pushes
  // 'nflow.library.loaded', which the app handles to refresh the palette).
  try {
    await nflowRpc('nflow.library.load', { path: dir });
  } catch (err) {
    const message = `Imported, but loading into the palette failed: ${
      (err as Error)?.message ?? String(err)
    }`;
    say(message);
    return { ok: false, message, libraryId: lib.id, blockLabel: name };
  }

  const nIn = info.inputs.length;
  const nOut = info.outputs.length;
  const message = `Imported ${name} (FMI ${
    info.fmiVersion ?? '?'
  }): ${nIn} input(s), ${nOut} output(s). Added to the palette.`;
  say(message);
  return { ok: true, message, libraryId: lib.id, blockLabel: name };
}
//=============================================================================
