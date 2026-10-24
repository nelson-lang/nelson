//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { createRoot } from 'react-dom/client';
import { expect, it, vi } from 'vitest';
import { PromptHost, webPrompt } from '../../src/components/PromptHost';
//=============================================================================
it('keeps a figure save prompt interactive and returns its filename', async () => {
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  try {
    act(() => root.render(<PromptHost />));
    let answer!: Promise<string | null>;
    act(() => {
      answer = webPrompt('Save figure to file (.png / .svg / .pdf):', 'undocked.svg');
    });
    expect(host.querySelector('[role="dialog"]')).not.toBeNull();
    expect(host.querySelector('input')!.value).toBe('undocked.svg');
    const accept = [...host.querySelectorAll('button')].find((b) => b.textContent === 'OK')!;
    act(() => accept.click());
    await expect(answer).resolves.toBe('undocked.svg');
    expect(host.querySelector('[role="dialog"]')).toBeNull();
    act(() => {
      answer = webPrompt('Save figure to file', 'cancelled.svg');
    });
    const cancel = [...host.querySelectorAll('button')].find((b) => b.textContent === 'Cancel')!;
    act(() => cancel.click());
    await expect(answer).resolves.toBeNull();
  } finally {
    act(() => root.unmount());
    host.remove();
    vi.unstubAllGlobals();
  }
});
//=============================================================================
