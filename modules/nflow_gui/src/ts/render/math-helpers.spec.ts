//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { makeMathHelpers } from './math-helpers.js';
import * as expr from '../utils/expr.js';
import * as svgElements from './svg-elements.js';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
describe('math-helpers', () => {
  let svg: any;
  let helpers: any;

  beforeEach(() => {
    svg = document.createElement('svg');
    svg.setAttribute('viewBox', '0 0 200 100');
    Object.defineProperty(svg, 'clientWidth', { value: 400, configurable: true });
    Object.defineProperty(svg, 'clientHeight', { value: 200, configurable: true });
    helpers = makeMathHelpers({ svg });
  });

  test('getSvgScale and pxToSvg', () => {
    const scale = helpers.getSvgScale();
    const pxX = helpers.pxToSvg(40, 'x', scale);
    const pxY = helpers.pxToSvg(40, 'y', scale);

    expect(scale).toEqual({ x: 2, y: 2 });
    expect(pxX).toBeCloseTo(20);
    expect(pxY).toBeCloseTo(20);
  });

  test('toLeadingMinusTex handles negatives and fallbacks', () => {
    const neg = helpers.toLeadingMinusTex('-5');
    const pos = helpers.toLeadingMinusTex('3');
    const empty = helpers.toLeadingMinusTex('', 'Z');

    expect(neg).toEqual({ tex: '\\text{-}5', hasLeadingNegative: true });
    expect(pos).toEqual({ tex: '3', hasLeadingNegative: false });
    expect(empty).toEqual({ tex: 'Z', hasLeadingNegative: false });
  });

  test('getAutoMathConfig for different block types', () => {
    expect(helpers.getAutoMathConfig({ type: 'expression' })).toMatchObject({
      className: '.nf-userfunc-formula',
    });
    expect(helpers.getAutoMathConfig({ type: 'tf' })).toMatchObject({
      className: '.nf-tf-formula',
    });
    expect(helpers.getAutoMathConfig({ type: 'dtf' })).toMatchObject({ className: '.dtf-math' });
    expect(helpers.getAutoMathConfig({ type: 'unknown' })).toBeNull();
  });

  test('setUserFuncMathBox and _computeRenderedUserFuncSize work with DOM sizes', () => {
    const mathGroup = document.createElement('div');
    const foreign = document.createElement('foreignObject');
    const span = document.createElement('span');
    span.className = 'katex';
    mathGroup.appendChild(foreign);
    mathGroup.appendChild(span);
    span.getBoundingClientRect = () => ({
      width: 60,
      height: 10,
      left: 0,
      top: 0,
      right: 0,
      bottom: 0,
    });
    Object.defineProperty(span, 'scrollWidth', { value: 60, configurable: true });
    Object.defineProperty(span, 'scrollHeight', { value: 10, configurable: true });

    helpers.setUserFuncMathBox(mathGroup, 123, 45);
    const res = helpers._computeRenderedUserFuncSize(mathGroup);

    expect(foreign.getAttribute('width')).toBe('123');
    expect(foreign.getAttribute('height')).toBe('45');
    expect(res).toEqual({ width: 80, height: 80 });
  });

  test('computeUserFuncSize uses exprToLatex and measureUserFuncTex', () => {
    const block: any = { params: { Expr: 'x+1' } };

    const info = helpers.computeUserFuncSize(block);

    expect(info).toHaveProperty(FieldNames.kWidth);
    expect(info).toHaveProperty(FieldNames.kHeight);
    expect(info.latex).toBe(expr.exprToLatex(String(block.params.Expr)));
  });

  test('getMathSpanSize returns scaled sizes', () => {
    const mathGroup = document.createElement('div');
    const span = document.createElement('span');
    mathGroup.appendChild(span);
    span.getBoundingClientRect = () => ({
      width: 80,
      height: 40,
      left: 0,
      top: 0,
      right: 0,
      bottom: 0,
    });
    Object.defineProperty(span, 'scrollWidth', { value: 80, configurable: true });
    Object.defineProperty(span, 'scrollHeight', { value: 40, configurable: true });

    const size = helpers.getMathSpanSize(mathGroup);

    expect(size).toEqual({ w: 40, h: 20 });
  });

  test('positionGainMath clamps and sets transform', () => {
    const mathGroup = document.createElement('g');
    const span = document.createElement('span');
    mathGroup.appendChild(span);
    span.getBoundingClientRect = () => ({
      width: 40,
      height: 10,
      left: 0,
      top: 0,
      right: 0,
      bottom: 0,
    });
    Object.defineProperty(span, 'scrollWidth', { value: 40, configurable: true });
    Object.defineProperty(span, 'scrollHeight', { value: 10, configurable: true });
    const block = { width: 100 } as any;

    helpers.positionGainMath(mathGroup, block, 8);

    const transform = mathGroup.getAttribute('transform');
    expect(transform).toMatch(/^translate\([\-0-9\.]+, 0\)$/);
  });
});
//=============================================================================
