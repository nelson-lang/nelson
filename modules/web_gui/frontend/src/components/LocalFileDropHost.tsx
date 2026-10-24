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
import { getCurrentDirectory } from '../desktop/currentDirectory';
import { uploadLocalFiles } from '../desktop/localFiles';
import { showStatusMessage } from '../desktop/statusMessage';
//=============================================================================
function carriesFiles(event: DragEvent): boolean {
  return Array.from(event.dataTransfer?.types ?? []).includes('Files');
}
//=============================================================================
export function LocalFileDropHost(): JSX.Element | null {
  const [draggingFiles, setDraggingFiles] = useState(false);

  useEffect(() => {
    let depth = 0;
    const dragEnter = (event: DragEvent) => {
      if (!carriesFiles(event)) return;
      depth += 1;
      setDraggingFiles(true);
    };
    const dragOver = (event: DragEvent) => {
      if (!carriesFiles(event)) return;
      event.preventDefault();
      if (event.dataTransfer) event.dataTransfer.dropEffect = 'copy';
      setDraggingFiles(true);
    };
    const dragLeave = (event: DragEvent) => {
      if (!carriesFiles(event)) return;
      depth = Math.max(0, depth - 1);
      if (depth === 0) setDraggingFiles(false);
    };
    const drop = (event: DragEvent) => {
      if (!carriesFiles(event)) return;
      event.preventDefault();
      depth = 0;
      setDraggingFiles(false);
      const files = event.dataTransfer?.files;
      if (!files?.length) return;
      void uploadLocalFiles(files, getCurrentDirectory().cwd).catch((error) =>
        showStatusMessage((error as Error).message || 'Upload failed')
      );
    };
    window.addEventListener('dragenter', dragEnter);
    window.addEventListener('dragover', dragOver);
    window.addEventListener('dragleave', dragLeave);
    window.addEventListener('drop', drop);
    return () => {
      window.removeEventListener('dragenter', dragEnter);
      window.removeEventListener('dragover', dragOver);
      window.removeEventListener('dragleave', dragLeave);
      window.removeEventListener('drop', drop);
    };
  }, []);

  return draggingFiles ? (
    <div className="local-file-drop" aria-hidden="true">
      <div className="local-file-drop__card">Drop files into the current Nelson folder</div>
    </div>
  ) : null;
}
//=============================================================================
