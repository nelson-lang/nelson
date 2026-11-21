//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import { canFitBlockContent, fitBlockToContent } from './fit-block-content.js';

describe('fit block to content', () => {
  it('uses the natural formula width and requests a resize', () => {
    const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    group.innerHTML = '<g class="nf-constant-formula"><text data-natural-width="250"></text></g>';
    const handle = document.createElement('div');
    const resized = vi.fn();
    handle.addEventListener('nflow:resize-to', resized);
    expect(fitBlockToContent({ type: 'constant', group, resizeHandle: handle })).toBe(true);
    expect(resized.mock.calls[0][0].detail).toEqual({ width: 270, height: 40 });
  });

  it('does not offer fitting for blocks without supported text or a resize handle', () => {
    expect(canFitBlockContent({ type: 'scope', resizeHandle: document.createElement('div') })).toBe(
      false
    );
    expect(canFitBlockContent({ type: 'constant' })).toBe(false);
    expect(
      fitBlockToContent({
        type: 'constant',
        group: document.createElement('div'),
        resizeHandle: document.createElement('div'),
      })
    ).toBe(false);
  });
});
