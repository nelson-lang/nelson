//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let renderBarrel: any;
beforeAll(async () => {
  renderBarrel = await import('../../../dist/render/render.js');
});
//=============================================================================
describe('render barrel exports', () => {
  test('re-exports public API and constant types', () => {
    expect(typeof renderBarrel.createSvgElement).toBe('function');
    expect(typeof renderBarrel.renderTeXMath).toBe('function');
    expect(typeof renderBarrel.buildFallbackPath).toBe('function');
    expect(typeof renderBarrel.buildPathWithHops).toBe('function');
    expect(typeof renderBarrel.buildTransferMathML).toBe('function');
    expect(typeof renderBarrel.buildPolyMathML).toBe('function');
    expect(typeof renderBarrel.buildTransferTeX).toBe('function');
    expect(typeof renderBarrel.buildPolyTeX).toBe('function');
    expect(typeof renderBarrel.drawIcon).toBe('function');
    expect(typeof renderBarrel.refreshBlockTemplates).toBe('function');
    expect(typeof renderBarrel.createRenderer).toBe('function');
    expect(typeof renderBarrel.FORCE_FULL_ROUTE_TIME_LIMIT_MS).toBe('number');
  });

  test('createSvgElement produces an Element with expected tag', () => {
    const el = renderBarrel.createSvgElement('g');
    expect(el instanceof Element).toBe(true);
    expect(el.tagName.toLowerCase()).toBe('g');
  });

  test('buildFallbackPath returns array of points', () => {
    const p = renderBarrel.buildFallbackPath({ x: 0, y: 0 }, { x: 10, y: 0 });
    expect(Array.isArray(p)).toBe(true);
    p.forEach((pt: any) => {
      expect(typeof pt.x).toBe('number');
      expect(typeof pt.y).toBe('number');
    });
  });
});
//=============================================================================
