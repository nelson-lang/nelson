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
import { renderXYZScope } from './render-xyz-scope';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const makePlot = (w = 300, h = 300) => {
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
const makeAxes = () => ({
  xAxis: makeLine(),
  yAxis: makeLine(),
  zAxis: makeLine(),
  xTicks: [makeLine()],
  yTicks: [makeLine()],
  zTicks: [makeLine()],
  xTickLabels: [makeText()],
  yTickLabels: [makeText()],
  zTickLabels: [makeText()],
});
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '<svg></svg>';
});
//=============================================================================
describe('renderXYZScope', () => {
  test('returns early when required fields are missing', () => {
    const block: any = {};

    expect(() => renderXYZScope(block)).not.toThrow();
  });

  test('clears paths when connected[0] is false', () => {
    const p = makePath();
    p.setAttribute('d', 'M1 1');
    const label = makeText();
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [p],
      scopeAxes: { xTickLabels: [label], yTickLabels: [label], zTickLabels: [label] },
      params: {},
      xyzScopeData: { series: { x: [1], y: [1], z: [1] }, connected: [false, true, true] },
    };

    renderXYZScope(block);

    expect(p.getAttribute('d')).toBe('');
  });

  test('renders when legacy connected is true', () => {
    const path = makePath();
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [path],
      scopeAxes: makeAxes(),
      params: {},
      xyzScopeData: {
        series: { x: [0, 1], y: [0, 1], z: [0, 1] },
        connected: true,
      },
    };

    renderXYZScope(block);

    expect(path.getAttribute('d')).toMatch(/M/);
  });

  test('clears paths and sets default limits when series is empty', () => {
    const p = makePath();
    p.setAttribute('d', 'M0 0');
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [p],
      scopeAxes: makeAxes(),
      params: {},
      xyzScopeData: { series: { x: [], y: [], z: [] } },
    };

    renderXYZScope(block);

    expect(p.getAttribute('d')).toBe('');
    expect(block.computedLimits).toHaveProperty('zMin');
    expect(block.computedLimits).toHaveProperty('zMax');
  });

  test('renders XYZ points and updates computedLimits', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      scopeAxes: makeAxes(),
      params: {},
      xyzScopeData: { series: { x: [0, 1, 2], y: [0, 1, 2], z: [0, 1, 2] } },
    };

    renderXYZScope(block);

    expect(block.scopePaths[0].getAttribute('d')).toMatch(/M/);
    expect(block.computedLimits).toHaveProperty('xMin');
    expect(block.computedLimits).toHaveProperty('yMin');
    expect(block.computedLimits).toHaveProperty('zMin');
  });

  test('respects explicit axis limit params', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      params: { XMin: -5, XMax: 5, YMin: -5, YMax: 5, ZMin: -5, ZMax: 5 },
      xyzScopeData: { series: { x: [0, 1], y: [0, 1], z: [0, 1] } },
    };

    renderXYZScope(block);

    expect(block.computedLimits.xMin).toBe(-5);
    expect(block.computedLimits.xMax).toBe(5);
    expect(block.computedLimits.zMin).toBe(-5);
    expect(block.computedLimits.zMax).toBe(5);
  });

  test('skips non-finite XYZ triples', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      params: {},
      xyzScopeData: { series: { x: [0, NaN, 2], y: [0, 1, 2], z: [0, 1, 2] } },
    };

    expect(() => renderXYZScope(block)).not.toThrow();
    expect(block.scopePaths[0].getAttribute('d')).toMatch(/M/);
  });

  test('handles custom rotation params', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      scopeAxes: makeAxes(),
      params: { RotationX: 45, RotationY: 60 },
      xyzScopeData: { series: { x: [0, 1], y: [0, 1], z: [0, 1] } },
    };

    expect(() => renderXYZScope(block)).not.toThrow();
    expect(block.computedLimits).toHaveProperty('zMin');
  });

  test('applies per-signal color, width and dash from params.signals', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      scopeAxes: makeAxes(),
      params: { signals: [{ color: '#00ff88', strokeWidth: 3, dash: 'dotted' }] },
      xyzScopeData: { series: { x: [0, 1, 2], y: [0, 1, 2], z: [0, 1, 2] } },
    };

    renderXYZScope(block);

    const style = block.scopePaths[0].getAttribute('style') || '';
    expect(style).toContain('stroke:#00ff88');
    expect(style).toContain('stroke-width:3');
    expect(style).toContain('stroke-dasharray:2 3');
  });
});
//=============================================================================
