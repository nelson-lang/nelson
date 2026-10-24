//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
//=============================================================================
type FileDialogLockOverlayProps = {
  visible: boolean;
};
//=============================================================================
type FileDialogLockOverlayController = {
  show: () => void;
  hide: () => void;
};
//=============================================================================
let overlayRoot: Root | null = null;
let overlayHost: HTMLDivElement | null = null;
function FileDialogLockOverlay({ visible }: FileDialogLockOverlayProps): JSX.Element {
  return (
    <div
      id="nf-file-dialog-overlay"
      aria-hidden="true"
      style={{
        position: 'fixed',
        left: 0,
        top: 0,
        width: '100%',
        height: '100%',
        zIndex: 2147483646,
        background: 'transparent',
        pointerEvents: 'auto',
        display: visible ? 'block' : 'none',
      }}
    />
  );
}
//=============================================================================
function renderFileDialogLockOverlay(visible: boolean): void {
  if (!overlayRoot || !overlayHost) {
    overlayHost = document.createElement('div');
    overlayHost.id = 'nf-file-dialog-overlay-root';
    document.documentElement.appendChild(overlayHost);
    overlayRoot = createRoot(overlayHost);
  }
  flushSync(() => {
    overlayRoot?.render(<FileDialogLockOverlay visible={visible} />);
  });
}
//=============================================================================
export function createFileDialogLockOverlay(): FileDialogLockOverlayController {
  renderFileDialogLockOverlay(false);
  return {
    show: () => renderFileDialogLockOverlay(true),
    hide: () => renderFileDialogLockOverlay(false),
  };
}
//=============================================================================
