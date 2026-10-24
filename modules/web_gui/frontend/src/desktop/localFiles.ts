//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { rpc } from '../rpc/client';
import { getCurrentDirectory } from './currentDirectory';
import { openEditor } from './desktop';
import { getDesktopCapabilities } from './capabilities';
import { runCommand } from './engineSession';
import { showStatusMessage } from './statusMessage';
//=============================================================================
export interface UploadedLocalFile {
  name: string;
  path: string;
  size: number;
  action: 'editor' | 'nflow' | 'workspace';
}
//=============================================================================
interface LocalFileDependencies {
  currentDirectory: () => { cwd: string; separator: string };
  openEditor: (path: string) => void;
  rpc: typeof rpc;
  runCommand: typeof runCommand;
  showStatusMessage: (message: string) => void;
  nflowAvailable: () => boolean;
}
//=============================================================================
function joinDirectory(directory: string, name: string, separator: string): string {
  const base = directory || '/workspace';
  return /[\\/]$/.test(base) ? `${base}${name}` : `${base}${separator || '/'}${name}`;
}
//=============================================================================
function safeDroppedName(name: string): string {
  const leaf = name.replaceAll('\\', '/').split('/').pop() || 'uploaded-file';
  return leaf.replace(/[/:*?"<>|]+/g, '_') || 'uploaded-file';
}
//=============================================================================
function readBrowserFile(file: File): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => resolve(String(reader.result).split(',')[1] ?? '');
    reader.onerror = () => reject(reader.error);
    reader.readAsDataURL(file);
  });
}
//=============================================================================
function classifyUploadedFile(
  path: string,
  dependencies: LocalFileDependencies
): UploadedLocalFile['action'] {
  if (/\.m$/i.test(path)) {
    dependencies.openEditor(path);
    return 'editor';
  }
  if (/\.nflow$/i.test(path) && dependencies.nflowAvailable()) {
    void dependencies.runCommand('nflow()');
    return 'nflow';
  }
  return 'workspace';
}
//=============================================================================
export async function uploadLocalFiles(
  files: FileList | File[],
  targetDirectory?: string,
  dependencies: LocalFileDependencies = {
    currentDirectory: getCurrentDirectory,
    openEditor,
    rpc,
    runCommand,
    showStatusMessage,
    nflowAvailable: () => Boolean(getDesktopCapabilities().nflow),
  }
): Promise<UploadedLocalFile[]> {
  const list = Array.from(files);
  if (list.length === 0) return [];
  const cwd = targetDirectory || dependencies.currentDirectory().cwd || '/workspace';
  const separator = dependencies.currentDirectory().separator || '/';
  const uploaded: UploadedLocalFile[] = [];
  for (const file of list) {
    const name = safeDroppedName(file.name);
    const path = joinDirectory(cwd, name, separator);
    const data = await readBrowserFile(file);
    const result = await dependencies.rpc<{ path?: string; size?: number }>('fs.upload', {
      path,
      data,
    });
    uploaded.push({
      name,
      path: result.path || path,
      size: Number(result.size ?? file.size),
      action: classifyUploadedFile(result.path || path, dependencies),
    });
  }
  dependencies.showStatusMessage(
    uploaded.length === 1
      ? `Uploaded ${uploaded[0].name} to ${cwd}`
      : `Uploaded ${uploaded.length} files to ${cwd}`
  );
  return uploaded;
}
//=============================================================================
