//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import { FilePickerDialog } from '@nelson/dialogs';
import { changeCurrentDirectory, getCurrentDirectory } from '../desktop/currentDirectory';
import { nelsonStringLiteral } from '../desktop/commands';
import { runCommand } from '../desktop/engineSession';
import { openEditor } from '../desktop/desktop';
import { showStatusMessage } from '../desktop/statusMessage';
import { rpc } from '../rpc/client';
//=============================================================================
export type DesktopPickerKind = 'directory' | 'editor' | 'execute';
export interface PathRequest {
  mode: 'open' | 'dir' | 'save';
  title: string;
  filters?: string[];
  /** Dialog history key, `desktop.<mode>` by default. */
  historyKey?: string;
  /** Initial file name, save mode only. */
  suggestedName?: string;
  defaultPath?: string;
}
export type SavePathRequest = Omit<PathRequest, 'mode'> & { suggestedName: string };
interface NativeDialogResult {
  available?: boolean;
  canceled?: boolean;
  path?: string;
}
interface WebPicker {
  request: PathRequest;
  historyKey: string;
  defaultPath: string;
  resolve: (path: string | null) => void;
}
// The mounted host shows the web picker when the engine has no native dialog.
let showWebPicker: ((picker: WebPicker) => void) | null = null;
//=============================================================================
function defaultHistoryKey(mode: PathRequest['mode']): string {
  return mode === 'dir' ? 'desktop.directory' : `desktop.${mode}`;
}
//=============================================================================
/** Ask for a path: native dialog when available, else the web picker; null when canceled. */
export async function requestPath(request: PathRequest): Promise<string | null> {
  const historyKey = request.historyKey || defaultHistoryKey(request.mode);
  const defaultPath = request.defaultPath || getCurrentDirectory().cwd || '.';
  try {
    const native = await rpc<NativeDialogResult>('fs.dialog.native', {
      mode: request.mode,
      title: request.title,
      filters: request.filters,
      suggestedName: request.suggestedName,
      historyKey,
      defaultPath,
    });
    if (native.available) return !native.canceled && native.path ? native.path : null;
  } catch {
    /* no native dialog: use the web picker */
  }
  if (!showWebPicker) return null;
  let directory = defaultPath;
  try {
    const start = await rpc<{ directory?: string }>('fs.dialog.start', { historyKey, defaultPath });
    directory = start.directory || defaultPath;
  } catch {
    /* keep the default directory */
  }
  return new Promise((resolve) =>
    showWebPicker?.({ request, historyKey, defaultPath: directory, resolve })
  );
}
//=============================================================================
/** Ask where to save a file. */
export function requestSavePath(request: SavePathRequest): Promise<string | null> {
  return requestPath({ ...request, mode: 'save' });
}
//=============================================================================
function pickerRequest(kind: DesktopPickerKind): PathRequest {
  if (kind === 'directory') {
    return { mode: 'dir', title: 'Choose current directory' };
  }
  return {
    mode: 'open',
    title: kind === 'editor' ? 'Open in Editor' : 'Execute a Nelson file',
    filters: kind === 'execute' ? ['.m'] : undefined,
  };
}
//=============================================================================
function useSelectedPath(kind: DesktopPickerKind, path: string): void {
  if (kind === 'editor') {
    openEditor(path);
    return;
  }
  if (kind === 'execute') {
    void runCommand(`run(${nelsonStringLiteral(path)})`);
    return;
  }
  void changeCurrentDirectory(path)
    .then((result) => showStatusMessage(`Current directory: ${result.cwd}`))
    .catch((error) => showStatusMessage((error as Error).message));
}
//=============================================================================
/** Open the desktop picker for one of its standard tasks and act on the choice. */
export function openDesktopPicker(kind: DesktopPickerKind): void {
  void requestPath(pickerRequest(kind)).then((path) => {
    if (path) useSelectedPath(kind, path);
  });
}
//=============================================================================
export function DesktopFilePickerHost(): JSX.Element | null {
  const [picker, setPicker] = useState<WebPicker | null>(null);

  useEffect(() => {
    showWebPicker = setPicker;
    return () => {
      showWebPicker = null;
    };
  }, []);

  if (!picker) return null;
  const { request, historyKey, defaultPath, resolve } = picker;
  return (
    <FilePickerDialog
      mode={request.mode}
      title={request.title}
      defaultPath={defaultPath}
      suggestedName={request.suggestedName}
      filters={request.filters}
      listDir={(path) => rpc('fs.list', { path })}
      onSubmit={(path) => {
        setPicker(null);
        void rpc('fs.dialog.accept', { historyKey, mode: request.mode, path }).catch(
          () => undefined
        );
        resolve(path);
      }}
      onCancel={() => {
        setPicker(null);
        resolve(null);
      }}
    />
  );
}
//=============================================================================
