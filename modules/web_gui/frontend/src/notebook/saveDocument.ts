//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { SavePathRequest } from '../components/DesktopFilePickerHost';
import { dirName } from '../desktop/paths';
import { bytesToBase64 } from './nlxFormat';
//=============================================================================
export type NotebookSaveFormat = 'nlx' | 'm';
export interface NotebookSaveInput {
  /** The .nlx container bytes or the .m text. */
  payload: Uint8Array | string;
  format: NotebookSaveFormat;
  /** The host path the document came from or was last saved to, if any. */
  path?: string;
  /** Always ask for a path (Save As). */
  saveAs: boolean;
  /** Suggested file name without extension. */
  baseName: string;
}
export interface NotebookSaveDeps {
  rpc: (method: string, params?: Record<string, unknown>) => Promise<unknown>;
  pickPath: (request: SavePathRequest) => Promise<string | null>;
  download: (filename: string, data: BlobPart, mime: string) => void;
  /** True when the engine has no host file system (WebAssembly in a browser). */
  browserOnly: () => boolean;
}
export type NotebookSaveResult =
  | { kind: 'saved'; path: string }
  | { kind: 'downloaded'; filename: string }
  | { kind: 'canceled' }
  | { kind: 'error'; message: string };
//=============================================================================
const MIME: Record<NotebookSaveFormat, string> = { nlx: 'application/zip', m: 'text/plain' };
//=============================================================================
export function hasFormat(path: string | undefined, format: NotebookSaveFormat): boolean {
  return !!path && new RegExp(`\\.${format}$`, 'i').test(path);
}
export function withFormat(path: string, format: NotebookSaveFormat): string {
  return hasFormat(path, format) ? path : `${path}.${format}`;
}
//=============================================================================
/** Save in place when the path has the format, else ask for a path; wasm downloads instead. */
export async function saveNotebookDocument(
  input: NotebookSaveInput,
  deps: NotebookSaveDeps
): Promise<NotebookSaveResult> {
  const filename = `${input.baseName}.${input.format}`;
  const data =
    typeof input.payload === 'string' ? new TextEncoder().encode(input.payload) : input.payload;
  if (deps.browserOnly()) {
    deps.download(filename, data as unknown as BlobPart, MIME[input.format]);
    return { kind: 'downloaded', filename };
  }
  let target = !input.saveAs && hasFormat(input.path, input.format) ? input.path : undefined;
  if (!target) {
    const picked = await deps.pickPath({
      title: input.format === 'nlx' ? 'Save Notebook' : 'Save as Script',
      suggestedName: filename,
      filters: [`.${input.format}`],
      defaultPath: input.path ? dirName(input.path) : undefined,
    });
    if (!picked) return { kind: 'canceled' };
    target = withFormat(picked, input.format);
  }
  try {
    const r = (await deps.rpc('fs.upload', { path: target, data: bytesToBase64(data) })) as {
      path?: string;
      error?: { message?: string };
    };
    if (r.error) return { kind: 'error', message: r.error.message || 'cannot write file' };
    return { kind: 'saved', path: r.path || target };
  } catch (e) {
    return { kind: 'error', message: (e as Error).message };
  }
}
//=============================================================================
