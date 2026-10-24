//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { buildRegistryFromManifest } from './generic-block-renderer.js';
//=============================================================================
describe('image render descriptor', () => {
  test('element mode renders <image> with attributes and resolves _src when manifest._dir present', () => {
    const manifest: any = {
      id: 'test.img',
      blocks: [
        {
          type: 'imgBlock',
          width: 80,
          height: 60,
          defaultParams: {},
          render: {
            type: 'image',
            src: 'icons/test.svg',
            svgMode: 'element',
            bodyClass: 'nf-block',
            preserveAspectRatio: 'xMidYMid meet',
            x: 4,
            y: 4,
            width: 72,
            height: 52,
          },
        },
      ],
      _dir: 'testdir',
    };

    const registry = buildRegistryFromManifest(manifest as any);
    const defs = (registry as any).getDefinitions();
    const def = defs['imgBlock'];
    expect(def).toBeDefined();
    expect(typeof def.render).toBe('function');

    const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const block = { group, width: 80, height: 60, id: 'b1', params: {} as any };
    def.render(block, {} as any);

    const img = group.querySelector('image');
    expect(img).not.toBeNull();
    expect(img?.getAttribute('x')).toBe('4');
    expect(img?.getAttribute('y')).toBe('4');
    expect(img?.getAttribute('width')).toBe('72');
    const href =
      img?.getAttribute('href') || img?.getAttributeNS('http://www.w3.org/1999/xlink', 'href');
    expect(href).toEqual(expect.stringContaining('libraries/testdir/icons/test.svg'));
  });

  test('inline mode fetches and injects svg children', async () => {
    const manifest: any = {
      id: 'test.img',
      blocks: [
        {
          type: 'imgInline',
          width: 80,
          height: 60,
          defaultParams: {},
          render: {
            type: 'image',
            src: 'icons/inline.svg',
            svgMode: 'inline',
            x: 0,
            y: 0,
            width: 80,
            height: 60,
          },
        },
      ],
      _dir: 'testdir',
    };

    const registry = buildRegistryFromManifest(manifest as any);
    const defs = registry.getDefinitions();
    const def = defs['imgInline'];
    expect(def).toBeDefined();

    (global as any).fetch = vi.fn().mockResolvedValue({
      ok: true,
      text: async () => '<svg xmlns="http://www.w3.org/2000/svg"><rect id="r1"/></svg>',
    });

    const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const block = { group, width: 80, height: 60, id: 'b2', params: {} as any };
    def.render(block, {} as any);

    await new Promise((r) => setTimeout(r, 0));

    expect(group.querySelector('#r1')).not.toBeNull();
  });
});
//=============================================================================
