//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { ensureKatexLoaded } from './block-library-handlers';
//=============================================================================
describe('ensureKatexLoaded', () => {
  beforeEach(() => {
    document.head.innerHTML = '';
    // @ts-ignore
    window.katex = undefined;
    // @ts-ignore
    window.__katexLoading = undefined;
  });

  test('injects link and script when katex not present', async () => {
    const appended: Element[] = [];
    const orig = document.head.appendChild;
    const mock = vi
      .spyOn(document.head, 'appendChild')
      .mockImplementation(function (this: Node, node: Node) {
        appended.push(node as Element);
        setTimeout(() => {
          try {
            (node as any).onload && (node as any).onload();
          } catch {}
        }, 0);
        return orig.call(this, node as Node);
      } as unknown as (node: Node) => Node);

    await ensureKatexLoaded();

    const hasLink = appended.some((el) => el.tagName && el.tagName.toLowerCase() === 'link');
    const hasScript = appended.some((el) => el.tagName && el.tagName.toLowerCase() === 'script');
    expect(hasLink).toBe(true);
    expect(hasScript).toBe(true);

    mock.mockRestore();
  });

  test('does nothing when window.katex exists', async () => {
    // @ts-ignore
    window.katex = { render: () => {} };
    const spy = vi.spyOn(document.head, 'appendChild');
    await ensureKatexLoaded();
    expect(spy).not.toHaveBeenCalled();
    spy.mockRestore();
  });
});
//=============================================================================
