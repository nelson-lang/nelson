//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('render module - exported helpers', () => {
  let renderModule: any;

  beforeEach(async () => {
    document.body.innerHTML = '';
    renderModule = await import('../../../dist/render/render.js');
  });

  test('createSvgElement creates namespaced element with attributes and text', () => {
    const el = renderModule.createSvgElement('text', { x: 1, y: 2, class: 'foo' }, 'hello');
    expect(el.namespaceURI).toBe('http://www.w3.org/2000/svg');
    expect(el.getAttribute('x')).toBe('1');
    expect(el.getAttribute('y')).toBe('2');
    expect(el.getAttribute('class')).toBe('foo');
    expect(el.textContent).toBe('hello');
  });

  test('buildFallbackPath returns straight line for aligned points and elbow otherwise', () => {
    const a = { x: 0, y: 0 };
    const b = { x: 0, y: 10 };
    expect(renderModule.buildFallbackPath(a, b)).toEqual([a, b]);
    const c = { x: 10, y: 0 };
    const d = { x: 20, y: 10 };
    expect(renderModule.buildFallbackPath(c, d)).toEqual([c, { x: 20, y: 0 }, { x: 20, y: 10 }, d]);
  });

  test('buildPolyTeX and buildTransferTeX produce reasonable TeX strings', () => {
    expect(renderModule.buildPolyTeX([], 's')).toBe('0');
    expect(renderModule.buildPolyTeX([1, 2], 's')).toBe('s+2');
    expect(renderModule.buildTransferTeX([1], [1], 's')).toBe('\\frac{1}{1}');
  });

  test('collectSharedSourceSplitPoints finds split for two paths with shared start and divergence', () => {
    const pathA = [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
      { x: 10, y: 10 },
    ];
    const pathB = [
      { x: 0, y: 0 },
      { x: 5, y: 0 },
      { x: 5, y: 5 },
    ];
    const conns = [
      { from: 'b1', fromIndex: 0, points: pathA },
      { from: 'b1', fromIndex: 0, points: pathB },
    ];
    const splits = renderModule.collectSharedSourceSplitPoints(conns, (c: any) => c.points);
    expect(Array.isArray(splits)).toBe(true);
    expect(splits).toEqual(expect.arrayContaining([{ x: 5, y: 0 }]));
  });

  test('drawIcon returns an SVG group containing expected text for type tf', () => {
    const g = renderModule.drawIcon('tf', 0, 0);
    expect(g).toBeTruthy();
    const text = g.querySelector('text');
    expect(text).toBeTruthy();
    expect(text.textContent).toContain('TF');
  });
});
//=============================================================================
