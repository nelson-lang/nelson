//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useState } from 'react';
import { rpc } from '../rpc/client';
//=============================================================================
export function useFigureExport(handle: number) {
  const [exporting, setExporting] = useState(false);
  const [exportError, setExportError] = useState('');
  const exportFigure = async (format: string, scale = 1, preview = false) => {
    setExporting(true);
    setExportError('');
    // Open synchronously so a browser does not classify the preview as a popup
    // unrelated to the click that requested it.
    const windowPreview = preview ? window.open('', '_blank') : null;
    try {
      const result = await rpc<{ bytes?: number[]; queued?: boolean }>('figure.action', {
        handle,
        request: { action: 'export', format, scale },
      });
      if (!result.bytes)
        throw new Error(
          result.queued
            ? 'Export is available when execution has finished.'
            : 'No exported figure received.'
        );
      const type = { png: 'image/png', svg: 'image/svg+xml', pdf: 'application/pdf' }[format];
      const url = URL.createObjectURL(new Blob([new Uint8Array(result.bytes)], { type }));
      if (windowPreview) {
        windowPreview.location.href = url;
        window.setTimeout(() => URL.revokeObjectURL(url), 60000);
      } else {
        const anchor = document.createElement('a');
        anchor.href = url;
        anchor.download = `figure-${handle + 1}.${format}`;
        document.body.appendChild(anchor);
        anchor.click();
        anchor.remove();
        window.setTimeout(() => URL.revokeObjectURL(url), 1000);
      }
    } catch (error) {
      windowPreview?.close();
      setExportError(error instanceof Error ? error.message : String(error));
    } finally {
      setExporting(false);
    }
  };
  return { exportFigure, exporting, exportError };
}
//=============================================================================
