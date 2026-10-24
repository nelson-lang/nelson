//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('render barrel exports', () => {
  test('barrel exports expected symbols and types', async () => {
    const mod = await import('../../../dist/render/render.js');

    expect(typeof mod.createSvgElement).toBe('function');
    expect(typeof mod.renderTeXMath).toBe('function');
    expect(typeof mod.buildFallbackPath).toBe('function');
    expect(typeof mod.buildPathWithHops).toBe('function');
    expect(typeof mod.buildTransferMathML).toBe('function');
    expect(typeof mod.drawIcon).toBe('function');
    expect(typeof mod.refreshBlockTemplates).toBe('function');
    expect(typeof mod.createRenderer).toBe('function');
  });
});
//=============================================================================
