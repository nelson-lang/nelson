//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Mounts the shared FilePickerDialog inside the editor window and wires it to
// the nflow.fs.list RPC for directory browsing. Resolves the request of the
// FILE_PICKER_EVENT once the user picks a path or cancels.
import { useEffect, useState } from 'react';
import { FilePickerDialog, type FilePickerListResult } from '@nelson/dialogs';
import { FILE_PICKER_EVENT } from './modal-events.js';
import { nflowRpc } from '../native-rpc.js';
import type { FilePickerRequestDetail } from '../file-dialogs.js';
//=============================================================================
async function listDir(path: string): Promise<FilePickerListResult> {
  const raw = await nflowRpc<string>('nflow.fs.list', { path });
  const parsed = JSON.parse(raw) as Partial<FilePickerListResult>;
  return {
    cwd: parsed.cwd || path,
    entries: Array.isArray(parsed.entries) ? parsed.entries : [],
  };
}
//=============================================================================
export function FilePickerHost(): JSX.Element | null {
  const [request, setRequest] = useState<FilePickerRequestDetail | null>(null);

  useEffect(() => {
    const onRequest = (event: Event): void => {
      const detail = (event as CustomEvent<FilePickerRequestDetail>).detail;
      if (!detail || typeof detail.resolve !== 'function') return;
      // Claim the request synchronously so the service knows a host is present.
      event.preventDefault();
      setRequest(detail);
    };
    document.addEventListener(FILE_PICKER_EVENT, onRequest);
    return () => document.removeEventListener(FILE_PICKER_EVENT, onRequest);
  }, []);

  if (!request) return null;

  const finish = (path: string | null): void => {
    request.resolve(path);
    setRequest(null);
  };

  const accept = (path: string): void => {
    void nflowRpc('nflow.fs.dialogAccept', { mode: request.mode, path })
      .catch(() => undefined)
      .finally(() => finish(path));
  };

  return (
    <FilePickerDialog
      mode={request.mode}
      title={request.title}
      defaultPath={request.defaultPath || '.'}
      suggestedName={request.suggestedName}
      filters={request.filters}
      listDir={listDir}
      onSubmit={accept}
      onCancel={() => finish(null)}
    />
  );
}
//=============================================================================
export default FilePickerHost;
//=============================================================================
