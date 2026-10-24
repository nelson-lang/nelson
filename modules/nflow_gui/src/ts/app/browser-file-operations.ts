//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { sanitizeFilename } from './file-operations.js';
//=============================================================================
declare global {
  interface Window {
    saveFile?: (content: string, path: string) => Promise<string>;
    __nfOpenModelPathBound?: boolean;
    showSaveFilePicker?: (options?: {
      suggestedName?: string;
      types?: Array<{ description?: string; accept?: Record<string, string[]> }>;
      excludeAcceptAllOption?: boolean;
    }) => Promise<FileSystemFileHandle>;
  }

  interface FileSystemFileHandle {
    readonly name?: string;
    createWritable(): Promise<FileSystemWritableFileStream>;
  }

  interface FileSystemWritableFileStream {
    write(data: Blob | string): Promise<void>;
    close(): Promise<void>;
  }
}
//=============================================================================
function browserDownload(
  content: string,
  fileName: string,
  mimeType = 'application/octet-stream'
): void {
  const blob = new Blob([content], { type: mimeType });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = fileName;
  document.body.appendChild(link);
  link.click();
  link.remove();
  URL.revokeObjectURL(url);
}
//=============================================================================
function filenameWithExtension(defaultName: string, extension: string): string {
  const normalizedExtension = extension.startsWith('.') ? extension : `.${extension}`;
  const base = sanitizeFilename(
    defaultName.replace(new RegExp(`${normalizedExtension}$`, 'i'), '')
  );
  return `${base}${normalizedExtension}`;
}
//=============================================================================
export async function saveTextWithBrowserPicker(options: {
  content: string;
  defaultName: string;
  extension: string;
  mimeType?: string;
  description?: string;
  onSaved?: (fileName: string) => void;
}): Promise<Record<string, unknown>> {
  const mimeType = options.mimeType || 'application/octet-stream';
  const extension = options.extension.startsWith('.') ? options.extension : `.${options.extension}`;
  const fileName = filenameWithExtension(options.defaultName, extension);
  if (typeof window.showSaveFilePicker === 'function') {
    try {
      const handle = await window.showSaveFilePicker({
        suggestedName: fileName,
        types: [
          {
            description: options.description || `${extension.slice(1).toUpperCase()} file`,
            accept: { [mimeType]: [extension] },
          },
        ],
      });
      const writable = await handle.createWritable();
      await writable.write(new Blob([options.content], { type: mimeType }));
      await writable.close();
      const savedName = handle.name || fileName;
      options.onSaved?.(savedName);
      return { success: true, fileName: savedName, browserFilePicker: true };
    } catch (error) {
      if ((error as Error)?.name === 'AbortError') {
        return { success: false, canceled: true };
      }
      return { success: false, error: (error as Error)?.message || String(error) };
    }
  }
  browserDownload(options.content, fileName, mimeType);
  options.onSaved?.(fileName);
  return { success: true, fileName, browserDownload: true };
}
//=============================================================================
export function saveNFlowWithBrowserDownload(
  content: string,
  defaultName: string,
  onSaved: (fileName: string) => void
): Record<string, unknown> {
  const fileName = filenameWithExtension(defaultName, '.nflow');
  const blob = new Blob([content], { type: 'application/json' });
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = fileName;
  document.body.appendChild(link);
  link.click();
  link.remove();
  URL.revokeObjectURL(url);
  onSaved(fileName);
  return { success: true, fileName, browserDownload: true };
}
//=============================================================================
export function openNFlowWithBrowserPicker(): void {
  const input = document.getElementById('fileOpenInput') as HTMLInputElement | null;
  input?.click();
}
//=============================================================================
export function hideMenubarDropdowns(): void {
  document.querySelectorAll<HTMLElement>('.menubar-dropdown').forEach((dropdown) => {
    dropdown.style.display = 'none';
  });
}
//=============================================================================
export function selectedBlockFromState<Key, Value>(
  blocks: ReadonlyMap<Key, Value>,
  selectedId: unknown
): Value | null {
  if (selectedId == null) return null;
  const direct = blocks.get(selectedId as Key);
  if (direct) return direct;
  return (
    Array.from(blocks.entries()).find(([id]) => String(id) === String(selectedId))?.[1] ?? null
  );
}
//=============================================================================
