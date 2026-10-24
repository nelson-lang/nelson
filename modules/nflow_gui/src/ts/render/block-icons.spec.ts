//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  svgRect,
  svgText,
  drawIcon,
  renderCenteredAxesPlot,
  renderSourcePlot,
} from './block/block-icons.js';
//=============================================================================
describe('block-icons', () => {
  test('svgRect and svgText create elements with correct attributes', () => {
    const rect = svgRect(1, 2, 10, 20, 'cls');
    expect(rect.tagName).toBe('rect');
    expect(rect.getAttribute('x')).toBe('1');
    expect(rect.getAttribute('width')).toBe('10');
    const text = svgText(5, 6, 'hello');
    expect(text.tagName).toBe('text');
    expect(text.textContent).toBe('hello');
    expect(text.getAttribute('class')).toContain('block-text');
  });

  test('drawIcon returns group with expected children for known types', () => {
    let called = false;
    (globalThis as any).queueMathJaxTypeset = () => {
      called = true;
    };
    const g1 = drawIcon('constant', 0, 0, 16);
    expect(g1.tagName).toBe('g');
    expect(g1.querySelector('line')).not.toBeNull();

    const g2 = drawIcon('tf', 0, 0, 20);
    expect(g2.querySelector('text')?.textContent).toBe('TF');

    const g3 = drawIcon('dtf', 0, 0, 18);
    expect(g3.querySelector('foreignObject')).not.toBeNull();
    expect(called).toBe(true);
  });

  test('renderCenteredAxesPlot and renderSourcePlot append expected children', () => {
    const svgNS = 'http://www.w3.org/2000/svg';
    const group = document.createElementNS(svgNS, 'g');
    renderSourcePlot(group, 100, 40, 'M0 0 L10 10');
    expect(group.querySelectorAll('line').length).toBeGreaterThanOrEqual(6);
    expect(group.querySelector('path')).not.toBeNull();

    const g2 = document.createElementNS(svgNS, 'g');
    renderCenteredAxesPlot(g2, 80, 60, ['M0 0 L10 10', 'M0 5 L10 15']);
    expect(g2.querySelectorAll('line').length).toBeGreaterThanOrEqual(6);
    expect(g2.querySelectorAll('path').length).toBe(2);
  });
});
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
let mod: any;
beforeEach(async () => {
  mod = await import('../../../dist/render/block/block-icons.js');
  document.body.innerHTML = '';
});
//=============================================================================
test('svgRect and svgText produce correct elements and attributes', () => {
  const r = mod.svgRect(1, 2, 3, 4, 'cls');
  expect(r.tagName.toLowerCase()).toBe('rect');
  expect(r.getAttribute('x')).toBe('1');
  expect(r.getAttribute('y')).toBe('2');
  expect(r.getAttribute('width')).toBe('3');
  expect(r.getAttribute('height')).toBe('4');
  expect(r.getAttribute('class')).toBe('cls');

  const t = mod.svgText(5, 6, 'hello');
  expect(t.tagName.toLowerCase()).toBe('text');
  expect(t.textContent).toBe('hello');
  expect(t.getAttribute('x')).toBe('5');
  expect(t.getAttribute('y')).toBe('6');
});
//=============================================================================
test('renderSourcePlot appends axes and path', () => {
  const g = document.createElement('g');
  mod.renderSourcePlot(g, 120, 60, 'M0 0 L10 10');
  const rect = g.querySelector('rect');
  expect(rect).toBeTruthy();
  const lines = g.querySelectorAll('line');
  expect(lines.length).toBeGreaterThanOrEqual(6);
  const path = g.querySelector('path.source-plot');
  expect(path).toBeTruthy();
});
//=============================================================================
test('renderCenteredAxesPlot handles single and multiple plot paths', () => {
  const g1 = document.createElement('g');
  mod.renderCenteredAxesPlot(g1, 100, 80, 'M0 0 L5 5');
  expect(g1.querySelectorAll('line').length).toBeGreaterThanOrEqual(6);
  expect(g1.querySelectorAll('path.source-plot').length).toBe(1);

  const g2 = document.createElement('g');
  mod.renderCenteredAxesPlot(g2, 100, 80, ['M0 0 L5 5', 'M1 1 L6 6']);
  expect(g2.querySelectorAll('path.source-plot').length).toBe(2);
});
//=============================================================================
test('renderRectBlock appends title, lines and optional icon', () => {
  const group = document.createElement('g');
  const block: any = { group, width: 140, height: 60 };
  mod.renderRectBlock(block, 'MyTitle', ['one', 'two'], null);
  expect(group.querySelector('rect')).toBeTruthy();
  expect(group.querySelector('text')).toBeTruthy();

  const group2 = document.createElement('g');
  const block2: any = { group: group2, width: 140, height: 60 };
  mod.renderRectBlock(block2, 'T', [], 'pid');
  expect(group2.querySelector('.block-icon')).toBeTruthy();
});
//=============================================================================
test('drawIcon creates expected child shapes for types and falls back', () => {
  const gConst = mod.drawIcon('constant', 0, 0);
  expect(gConst.querySelector('line')).toBeTruthy();

  const gDtf = mod.drawIcon('dtf', 0, 0);
  expect(gDtf.querySelector('foreignObject')).toBeTruthy();
  expect(gDtf.querySelector('span.mathjax-tex')).toBeTruthy();

  const gUnknown = mod.drawIcon('nonexistent', 0, 0);
  const txt = gUnknown.querySelector('text');
  expect(txt).toBeTruthy();
  expect(txt.textContent).toBe('fx');
});
//=============================================================================
