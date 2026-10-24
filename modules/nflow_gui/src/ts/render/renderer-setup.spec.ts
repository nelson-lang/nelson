//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests for renderer-setup
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/core/renderer-setup.js');
  document.body.innerHTML = '';
});
//=============================================================================
test('setupRendererDom creates defs, marker, selection and marquee elements and wire layers', () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const overlay = document.createElement('div');
  const ctx: any = { svg, overlayLayer: overlay };

  mod.setupRendererDom(ctx);

  const defs = svg.querySelector('defs');
  expect(defs).not.toBeNull();
  const marker = defs!.querySelector('#wire-arrow');
  expect(marker).not.toBeNull();
  const path = marker!.querySelector('path');
  expect(path).not.toBeNull();

  expect(ctx.selectionRect).not.toBeNull();
  expect(overlay.contains(ctx.selectionRect)).toBe(true);
  expect(ctx.marqueeRect).not.toBeNull();
  expect(overlay.contains(ctx.marqueeRect)).toBe(true);

  expect(ctx.wireJunctionLayer).not.toBeNull();
  expect(overlay.contains(ctx.wireJunctionLayer)).toBe(true);
  expect(ctx.wireCornerLayer).not.toBeNull();
  expect(overlay.contains(ctx.wireCornerLayer)).toBe(true);
});
//=============================================================================
test('setupRendererDom is safe when debug elements are missing', () => {
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  const overlay = document.createElement('div');
  const ctx: any = { svg, overlayLayer: overlay };
  expect(() => mod.setupRendererDom(ctx)).not.toThrow();
});
//=============================================================================
