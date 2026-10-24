//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
import { getNFlowHostMode, shouldUseNativeDialogs } from './host-mode.js';
import { FILE_PICKER_EVENT } from './components/modal-events.js';
import { getUiPref, setUiPref } from './ui-prefs.js';
//=============================================================================
export type FilePickerMode = 'open' | 'save' | 'dir';
export interface FilePickerRequest {
  mode: FilePickerMode;
  title?: string;
  defaultPath?: string;
  suggestedName?: string;
  filters?: string[];
}
//=============================================================================
export interface FilePickerRequestDetail extends FilePickerRequest {
  resolve: (path: string | null) => void;
}
//=============================================================================
export interface SaveDialogResult {
  success: boolean;
  fileName?: string;
  filePath?: string;
  canceled?: boolean;
  error?: string;
}
//=============================================================================
export interface OpenDialogResult {
  success: boolean;
  content?: string;
  fileName?: string;
  filePath?: string;
  error?: string;
}
//=============================================================================
export interface ChooseDirectoryResult {
  success: boolean;
  path?: string;
  canceled?: boolean;
}
//=============================================================================
/** Whether the Qt-free file I/O RPCs are reachable (native host present). */
export function canUseNativeFiles(): boolean {
  return canUseNFlowRpc('nflow.file.writeText');
}
//=============================================================================
function basename(p: string): string {
  const cut = p.replace(/[\\/]+$/, '');
  const idx = Math.max(cut.lastIndexOf('/'), cut.lastIndexOf('\\'));
  return idx >= 0 ? cut.slice(idx + 1) : cut;
}
//=============================================================================
function dirname(p: string): string {
  const cut = p.replace(/[\\/]+$/, '');
  const idx = Math.max(cut.lastIndexOf('/'), cut.lastIndexOf('\\'));
  return idx > 0 ? cut.slice(0, idx) : '';
}
//=============================================================================
function dialogDirectoryKey(mode: FilePickerMode): string {
  return `nf-dialog-${mode}-directory`;
}
//=============================================================================
function requestWithRememberedDirectory(request: FilePickerRequest): FilePickerRequest {
  if (request.defaultPath && request.defaultPath !== '.') return request;
  const remembered = getUiPref(dialogDirectoryKey(request.mode), request.defaultPath || '.');
  return { ...request, defaultPath: remembered || '.' };
}
//=============================================================================
function rememberPickedDirectory(mode: FilePickerMode, path: string | null): void {
  if (!path) return;
  const directory = mode === 'dir' ? path.replace(/[\\/]+$/, '') : dirname(path);
  if (directory) setUiPref(dialogDirectoryKey(mode), directory);
}
//=============================================================================
function filtersForType(typeOfFile: string): string[] {
  switch (typeOfFile) {
    case 'nflow':
      return ['.nflow'];
    case 'nblock':
      return ['.nblock'];
    case 'svg':
      return ['.svg'];
    case 'C':
      return ['.c'];
    case 'rust':
      return ['.rs'];
    default:
      return [];
  }
}
/**
 * Tries the system (native/Qt) file chooser via the backend. Resolves with the
 * chosen path or null (cancelled).
 */
async function tryNativeDialog(request: FilePickerRequest): Promise<string | null> {
  let nativeDefault = request.defaultPath ?? '';
  if (request.mode === 'save' && request.suggestedName) {
    const dir =
      nativeDefault && nativeDefault !== '.' ? nativeDefault.replace(/[\\/]+$/, '') + '/' : '';
    nativeDefault = dir + request.suggestedName;
  }
  try {
    const raw = await nflowRpc<string>('nflow.fs.nativeDialog', {
      mode: request.mode,
      title: request.title ?? '',
      defaultPath: nativeDefault,
      filters: request.filters ?? [],
    });
    const res = JSON.parse(raw) as {
      available?: boolean;
      canceled?: boolean;
      path?: string;
      reason?: string;
    };
    if (!res.available) {
      throw new Error(`Native file dialog is unavailable: ${res.reason || 'unknown reason'}`);
    }
    if (res.canceled || !res.path) return null;
    return res.path;
  } catch (err) {
    throw new Error((err as Error)?.message || String(err));
  }
}
//=============================================================================
function openHtmlFilePicker(request: FilePickerRequest): Promise<string | null> {
  return new Promise((resolve, reject) => {
    if (typeof document === 'undefined') {
      reject(new Error('HTML file picker is not available'));
      return;
    }
    const event = new CustomEvent<FilePickerRequestDetail>(FILE_PICKER_EVENT, {
      cancelable: true,
      detail: { ...request, resolve },
    });
    document.dispatchEvent(event);
    if (!event.defaultPrevented) {
      reject(new Error('HTML file picker is not available'));
    }
  });
}
//=============================================================================
export async function openFilePicker(request: FilePickerRequest): Promise<string | null> {
  if (
    request.mode === 'open' &&
    request.filters?.some((filter) => filter.replace(/^\./, '') === 'fmu') &&
    (await getNFlowHostMode()) === 'wasm'
  ) {
    return nflowRpc<string | null>('nflow.fmu.pick');
  }
  const effectiveRequest = requestWithRememberedDirectory(request);
  let path: string | null;
  if (await shouldUseNativeDialogs()) {
    try {
      path = await tryNativeDialog(effectiveRequest);
      rememberPickedDirectory(request.mode, path);
      return path;
    } catch {
      // The embedded-WebView host could not provide an OS dialog (no Qt bridge on a CLI or
      // debugging host). Fall through to the HTML picker.
    }
  }
  let htmlRequest = effectiveRequest;
  try {
    const raw = await nflowRpc<string>('nflow.fs.dialogStart', {
      mode: request.mode,
      defaultPath: effectiveRequest.defaultPath ?? '',
    });
    const result = JSON.parse(raw) as { directory?: string };
    htmlRequest = {
      ...effectiveRequest,
      defaultPath: result.directory || effectiveRequest.defaultPath || '.',
    };
  } catch {
    /* use the remembered directory without a host-side hint */
  }
  path = await openHtmlFilePicker(htmlRequest);
  rememberPickedDirectory(request.mode, path);
  return path;
}
//=============================================================================
/** Picks a destination, then writes `content` there through nflow.file.writeText. */
export async function saveViaDialog(opts: {
  typeOfFile: string;
  content: string;
  defaultName: string;
}): Promise<SaveDialogResult> {
  const suggestedName = basename(opts.defaultName);
  const parent = dirname(opts.defaultName);
  const path = await openFilePicker({
    mode: 'save',
    title: 'Save File',
    defaultPath: parent || '.',
    suggestedName,
    filters: filtersForType(opts.typeOfFile),
  });
  if (!path) return { success: false, canceled: true };

  try {
    const raw = await nflowRpc<string>('nflow.file.writeText', { path, content: opts.content });
    const res = JSON.parse(raw) as { success?: boolean; error?: string };
    if (res.success !== true) {
      return { success: false, error: res.error || 'Cannot write file' };
    }
  } catch (err) {
    return { success: false, error: (err as Error)?.message || String(err) };
  }
  return { success: true, fileName: basename(path), filePath: path };
}
//=============================================================================
/** Picks a file, then reads it through nflow.file.readText. */
export async function openViaDialog(opts: { extension?: string }): Promise<OpenDialogResult> {
  const path = await openFilePicker({
    mode: 'open',
    title: 'Open File',
    defaultPath: '.',
    filters: opts.extension ? [opts.extension] : [],
  });
  if (!path) return { success: false };

  try {
    const raw = await nflowRpc<string>('nflow.file.readText', { path });
    const res = JSON.parse(raw) as { success?: boolean; content?: string; error?: string };
    if (res.success !== true) {
      return { success: false, error: res.error || 'Cannot open file' };
    }
    return {
      success: true,
      content: res.content || '',
      fileName: basename(path),
      filePath: path,
    };
  } catch (err) {
    return { success: false, error: (err as Error)?.message || String(err) };
  }
}
//=============================================================================
/** Picks a directory (no I/O). */
export async function chooseDirectoryViaDialog(): Promise<ChooseDirectoryResult> {
  const path = await openFilePicker({
    mode: 'dir',
    title: 'Select destination directory',
    defaultPath: '.',
  });
  if (!path) return { success: false, canceled: true };
  return { success: true, path };
}
//=============================================================================
