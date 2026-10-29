//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/** @jest-environment jsdom */
/* eslint-disable @typescript-eslint/no-explicit-any */
import { renderXYScope } from './render-xy-scope';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const makePlot = (w = 200, h = 200) => {
  const el = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  el.setAttribute('x', '10');
  el.setAttribute('y', '10');
  el.setAttribute('width', String(w));
  el.setAttribute('height', String(h));
  return el;
};
//=============================================================================
const makeLine = () => document.createElementNS('http://www.w3.org/2000/svg', 'line');
const makePath = () => document.createElementNS('http://www.w3.org/2000/svg', 'path');
const makeText = () => document.createElementNS('http://www.w3.org/2000/svg', 'text');
beforeEach(() => {
  document.body.innerHTML = '<svg></svg>';
});
//=============================================================================
describe('renderXYScope', () => {
  test('returns early when required fields are missing', () => {
    const block: any = {};

    expect(() => renderXYScope(block)).not.toThrow();
  });

  test('clears paths when connected[0] is false', () => {
    const path1 = makePath();
    path1.setAttribute('d', 'M1 1');
    const label = makeText();
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [path1],
      scopeAxes: { xTickLabels: [label], yTickLabels: [label] },
      params: {},
      xyScopeData: { series: { x: [1], y: [1] }, connected: [false, true] },
    };

    renderXYScope(block);

    expect(path1.getAttribute('d')).toBe('');
    expect(label.getAttribute('display')).toBe('none');
  });

  test('renders when legacy connected is true', () => {
    const path = makePath();
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [path],
      params: {},
      xyScopeData: { series: { x: [0, 1], y: [0, 1] }, connected: true },
    };

    renderXYScope(block);

    expect(path.getAttribute('d')).toMatch(/M/);
  });

  test('clears paths when series is empty', () => {
    const p = makePath();
    p.setAttribute('d', 'M0 0');
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [p],
      params: {},
      xyScopeData: { series: { x: [], y: [] } },
    };

    renderXYScope(block);

    expect(p.getAttribute('d')).toBe('');
    expect(block.computedLimits).toHaveProperty('xMin');
    expect(block.computedLimits).toHaveProperty('yMin');
  });

  test('renders XY scope with series points and updates computedLimits', () => {
    const axes: any = {
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: [makeLine()],
      yTicks: [makeLine()],
      xTickLabels: [makeText()],
      yTickLabels: [makeText()],
    };
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      scopeAxes: axes,
      params: {},
      xyScopeData: { series: { x: [0, 1, 2], y: [0, 1, 2] } },
    };

    renderXYScope(block);

    expect(block.scopePaths[0].getAttribute('d')).toMatch(/M/);
    expect(block.computedLimits).toHaveProperty('xMin');
    expect(block.computedLimits).toHaveProperty('yMin');
    expect(block.computedLimits).toHaveProperty('xMax');
    expect(block.computedLimits).toHaveProperty('yMax');
  });

  test('respects explicit xMin/xMax/yMin/yMax params', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      params: { XMin: -5, XMax: 5, YMin: -5, YMax: 5 },
      xyScopeData: { series: { x: [0, 1], y: [0, 1] } },
    };

    renderXYScope(block);

    expect(block.computedLimits.xMin).toBe(-5);
    expect(block.computedLimits.xMax).toBe(5);
    expect(block.computedLimits.yMin).toBe(-5);
    expect(block.computedLimits.yMax).toBe(5);
  });

  test('renders showTickLabels mode', () => {
    const axes: any = {
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: [makeLine(), makeLine()],
      yTicks: [makeLine(), makeLine()],
      xTickLabels: [makeText(), makeText()],
      yTickLabels: [makeText(), makeText()],
    };
    const block: any = {
      scopePlot: makePlot(300, 300),
      scopePaths: [makePath()],
      scopeAxes: axes,
      params: { ShowTickLabels: true },
      xyScopeData: { series: { x: [0, 1, 2, 3], y: [0, 1, 2, 3] } },
    };

    renderXYScope(block);

    const visX = axes.xTickLabels.filter((l: any) => l.getAttribute('display') === 'block');
    const visY = axes.yTickLabels.filter((l: any) => l.getAttribute('display') === 'block');
    expect(visX.length + visY.length).toBeGreaterThanOrEqual(1);
  });

  test('skips non-finite XY pairs', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      params: {},
      xyScopeData: { series: { x: [0, NaN, 2], y: [0, 1, 2] } },
    };

    expect(() => renderXYScope(block)).not.toThrow();
    expect(block.scopePaths[0].getAttribute('d')).toMatch(/M/);
  });

  test('applies per-signal color, width and dash from params.signals', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      params: { signals: [{ color: '#ff0000', strokeWidth: 4, dash: 'dashed' }] },
      xyScopeData: { series: { x: [0, 1, 2], y: [0, 1, 2] } },
    };

    renderXYScope(block);

    const style = block.scopePaths[0].getAttribute('style') || '';
    expect(style).toContain('stroke:#ff0000');
    expect(style).toContain('stroke-width:4');
    expect(style).toContain('stroke-dasharray:6 3');
  });

  test('leaves the fallback palette untouched when no signal style is set', () => {
    const path = makePath();
    path.setAttribute('class', 'scope-path-1');
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [path],
      params: {},
      xyScopeData: { series: { x: [0, 1], y: [0, 1] } },
    };

    renderXYScope(block);

    expect(path.getAttribute('class')).toBe('scope-path-1');
    expect(path.getAttribute('style')).toBeNull();
  });
});
//=============================================================================
