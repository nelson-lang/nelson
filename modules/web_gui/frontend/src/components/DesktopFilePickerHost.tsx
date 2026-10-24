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
export const OPEN_DESKTOP_PICKER_EVENT = 'nelson-webview.open-desktop-picker';
export type DesktopPickerKind = 'directory' | 'editor' | 'execute';
interface NativeDialogResult {
  available?: boolean;
  canceled?: boolean;
  path?: string;
}
//=============================================================================
function pickerOptions(kind: DesktopPickerKind): {
  mode: 'dir' | 'open';
  title: string;
  filters?: string[];
  historyKey: string;
} {
  if (kind === 'directory') {
    return {
      mode: 'dir',
      title: 'Choose current directory',
      historyKey: 'desktop.directory',
    };
  }
  return {
    mode: 'open',
    title: kind === 'editor' ? 'Open in Editor' : 'Execute a Nelson file',
    filters: kind === 'execute' ? ['.m'] : undefined,
    historyKey: 'desktop.open',
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
export function openDesktopPicker(kind: DesktopPickerKind): void {
  window.dispatchEvent(new CustomEvent(OPEN_DESKTOP_PICKER_EVENT, { detail: { kind } }));
}
//=============================================================================
export function DesktopFilePickerHost(): JSX.Element | null {
  const [picker, setPicker] = useState<{ kind: DesktopPickerKind; defaultPath: string } | null>(
    null
  );

  useEffect(() => {
    const open = (event: Event) => {
      const detail = (event as CustomEvent<{ kind?: DesktopPickerKind }>).detail;
      const kind = detail?.kind ?? 'directory';
      const options = pickerOptions(kind);
      const defaultPath = getCurrentDirectory().cwd || '.';
      void rpc<NativeDialogResult>('fs.dialog.native', { ...options, defaultPath })
        .then((result) => {
          if (result.available) {
            if (!result.canceled && result.path) useSelectedPath(kind, result.path);
            return;
          }
          return rpc<{ directory?: string }>('fs.dialog.start', {
            historyKey: options.historyKey,
          }).then((start) => setPicker({ kind, defaultPath: start.directory || defaultPath }));
        })
        .catch(() => setPicker({ kind, defaultPath }));
    };
    window.addEventListener(OPEN_DESKTOP_PICKER_EVENT, open as EventListener);
    return () => window.removeEventListener(OPEN_DESKTOP_PICKER_EVENT, open as EventListener);
  }, []);

  if (!picker) return null;
  const { kind, defaultPath } = picker;
  const options = pickerOptions(kind);
  return (
    <FilePickerDialog
      mode={options.mode}
      title={options.title}
      defaultPath={defaultPath}
      filters={options.filters}
      listDir={(path) => rpc('fs.list', { path })}
      onSubmit={(path) => {
        setPicker(null);
        void rpc('fs.dialog.accept', {
          historyKey: options.historyKey,
          mode: options.mode,
          path,
        }).catch(() => undefined);
        useSelectedPath(kind, path);
      }}
      onCancel={() => setPicker(null)}
    />
  );
}
//=============================================================================
