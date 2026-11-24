//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export async function copyPngDataUrl(dataUrl: string): Promise<void> {
  const blob = await fetch(dataUrl).then((response) => response.blob());
  if (!navigator.clipboard?.write || typeof ClipboardItem === 'undefined') {
    throw new Error('Image clipboard API unavailable');
  }
  await navigator.clipboard.write([new ClipboardItem({ 'image/png': blob })]);
}
//=============================================================================
export function offerClipboardRetry(dataUrl: string): HTMLButtonElement {
  document.getElementById('nelson-copy-image-retry')?.remove();
  const button = document.createElement('button');
  button.id = 'nelson-copy-image-retry';
  button.textContent = 'Copy image';
  button.style.cssText = 'position:fixed;right:18px;bottom:42px;z-index:10000;padding:8px 14px';
  button.onclick = () =>
    void copyPngDataUrl(dataUrl)
      .then(() => button.remove())
      .catch(() => undefined);
  document.body.appendChild(button);
  return button;
}
//=============================================================================
export async function handleClipboardImage(dataUrl: string): Promise<void> {
  try {
    await copyPngDataUrl(dataUrl);
  } catch {
    offerClipboardRetry(dataUrl);
  }
}
//=============================================================================
