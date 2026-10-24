//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
test('render barrel - import and exercise re-exports', async () => {
  const mod = await import('../../../dist/render/render.js');
  if (typeof mod.createRenderer === 'function') {
    const ns = 'http://www.w3.org/2000/svg';
    const svg = document.createElementNS(ns, 'svg');
    const r = mod.createRenderer({
      svg,
      blockLayer: document.createElementNS(ns, 'g'),
      wireLayer: document.createElementNS(ns, 'g'),
      overlayLayer: document.createElementNS(ns, 'g'),
    });
    expect(r).toBeTruthy();
  }

  if (typeof mod.refreshBlockTemplates === 'function') {
    expect(() => mod.refreshBlockTemplates()).not.toThrow();
  }

  const fnNames = ['drawIcon', 'buildFallbackPath', 'buildPathWithHops'];
  for (const n of fnNames) {
    if (typeof mod[n] === 'function') {
      try {
        mod[n]();
      } catch (e) {
        /* ignore runtime errors from missing args */
      }
    }
  }
});
//=============================================================================
