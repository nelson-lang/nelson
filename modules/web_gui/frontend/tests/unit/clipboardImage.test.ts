//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { handleClipboardImage } from '../../src/desktop/clipboardImage';
//=============================================================================
const dataUrl = 'data:image/png;base64,iVBORw0KGgo=';
class TestClipboardItem {
  constructor(readonly items: Record<string, Blob>) {}
}
//=============================================================================
describe('browser image clipboard', () => {
  const write = vi.fn();

  beforeEach(() => {
    vi.stubGlobal('ClipboardItem', TestClipboardItem);
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({ blob: () => Promise.resolve(new Blob()) }));
    Object.defineProperty(navigator, 'clipboard', {
      configurable: true,
      value: { write },
    });
    write.mockReset();
    document.body.replaceChildren();
  });

  afterEach(() => vi.unstubAllGlobals());

  it('copies a PNG directly when permission is granted', async () => {
    write.mockResolvedValue(undefined);
    await handleClipboardImage(dataUrl);
    expect(write).toHaveBeenCalledOnce();
    expect(document.getElementById('nelson-copy-image-retry')).toBeNull();
  });

  it('offers a user-gesture retry when permission is denied', async () => {
    write.mockRejectedValue(new Error('denied'));
    await handleClipboardImage(dataUrl);
    expect(document.getElementById('nelson-copy-image-retry')).not.toBeNull();
  });

  it('retries from the action and removes it after success', async () => {
    write.mockRejectedValueOnce(new Error('gesture required')).mockResolvedValueOnce(undefined);
    await handleClipboardImage(dataUrl);
    document.getElementById('nelson-copy-image-retry')?.click();
    await vi.waitFor(() => expect(write).toHaveBeenCalledTimes(2));
    await vi.waitFor(() => expect(document.getElementById('nelson-copy-image-retry')).toBeNull());
  });

  it('keeps the retry action when a user-gesture attempt is still denied', async () => {
    write.mockRejectedValue(new Error('denied'));
    await handleClipboardImage(dataUrl);
    document.getElementById('nelson-copy-image-retry')?.click();
    await vi.waitFor(() => expect(write).toHaveBeenCalledTimes(2));
    expect(document.getElementById('nelson-copy-image-retry')).not.toBeNull();
  });
});
//=============================================================================
