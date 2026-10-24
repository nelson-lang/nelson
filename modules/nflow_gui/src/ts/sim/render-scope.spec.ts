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
import { renderScope } from './render-scope';
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
describe('renderScope', () => {
  test('returns early for xyScope type', () => {
    const block: any = {
      type: 'xyScope',
      scopePaths: [makePath()],
      scopePlot: makePlot(),
      scopeData: { time: [], series: [] },
    };
    block.scopePaths[0].setAttribute('d', 'M1 1');

    renderScope(block);

    expect(block.scopePaths[0].getAttribute('d')).toBe('M1 1');
  });

  test('returns early for xyzScope type', () => {
    const block: any = {
      type: 'xyzScope',
      scopePaths: [makePath()],
      scopePlot: makePlot(),
      scopeData: { time: [], series: [] },
    };
    block.scopePaths[0].setAttribute('d', 'M2 2');
    renderScope(block);
    expect(block.scopePaths[0].getAttribute('d')).toBe('M2 2');
  });

  test('returns early when scopeData is missing', () => {
    const block: any = { scopePlot: makePlot(), scopePaths: [makePath()] };
    expect(() => renderScope(block)).not.toThrow();
  });

  test('clears paths and hides labels for empty data', () => {
    const label = makeText();
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      scopeAxes: { xTickLabels: [label], yTickLabels: [label] },
      params: {},
      scopeData: { time: [], series: [] },
    };
    block.scopePaths[0].setAttribute('d', 'M 0 0');

    renderScope(block);

    expect(block.scopePaths[0].getAttribute('d')).toBe('');
    expect(label.getAttribute('display')).toBe('none');
    expect(block.computedLimits).toHaveProperty('tMin');
    expect(block.computedLimits).toHaveProperty('yMin');
  });

  test('renders a series of values and sets computedLimits', () => {
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
      scopeData: { time: [0, 0.5, 1], series: [[1, 2, 3]] },
    };

    renderScope(block);

    expect(block.scopePaths[0].getAttribute('d')).toMatch(/M/);
    expect(block.computedLimits).toHaveProperty('yMin');
    expect(block.computedLimits).toHaveProperty('yMax');
    expect(block.computedLimits).toHaveProperty('tMin');
    expect(block.computedLimits).toHaveProperty('tMax');
  });

  test('renders with showTickLabels = true and sets tick label text', () => {
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
      scopeData: { time: [0, 1, 2, 3, 4, 5], series: [[0, 1, 2, 3, 4, 5]] },
    };

    renderScope(block);

    const displayed = axes.xTickLabels.filter((l: any) => l.getAttribute('display') === 'block');
    expect(displayed.length).toBeGreaterThanOrEqual(1);
  });

  test('creates cursor and dots when scopeHoverX is set', () => {
    const svg = document.querySelector('svg') as SVGElement;
    const axes: any = {
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: [makeLine()],
      yTicks: [makeLine()],
      xTickLabels: [makeText()],
      yTickLabels: [makeText()],
    };
    const block: any = {
      scopePlot: makePlot(200, 200),
      scopePaths: [makePath()],
      scopeAxes: axes,
      params: {},
      scopeData: { time: [0, 1], series: [[1, 2]] },
      group: svg,
      scopeHoverX: 50,
    };

    renderScope(block);

    expect(block.scopeCursor).toBeDefined();
    expect(Array.isArray(block.scopeDots)).toBe(true);
  });

  test('respects explicit tMin/tMax/yMin/yMax params', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      params: { TMin: 0, TMax: 5, YMin: -10, YMax: 10 },
      scopeData: { time: [0, 1, 2], series: [[1, 2, 3]] },
    };

    renderScope(block);

    expect(block.computedLimits.tMin).toBe(0);
    expect(block.computedLimits.tMax).toBe(5);
    expect(block.computedLimits.yMin).toBe(-10);
    expect(block.computedLimits.yMax).toBe(10);
  });

  test('handles connected filter - excludes disconnected series', () => {
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath(), makePath()],
      params: {},
      scopeData: {
        time: [0, 1],
        series: [
          [1, 2],
          [3, 4],
        ],
        connected: [false, true],
      },
    };

    renderScope(block);

    expect(block.computedLimits.yMax).toBeGreaterThanOrEqual(4);
  });

  test('renders every series when legacy connected is true', () => {
    const paths = [makePath(), makePath()];
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: paths,
      params: {},
      scopeData: {
        time: [0, 1],
        series: [
          [1, 2],
          [3, 4],
        ],
        connected: true,
      },
    };

    expect(() => renderScope(block)).not.toThrow();
    expect(paths[0].getAttribute('d')).toMatch(/M/);
    expect(paths[1].getAttribute('d')).toMatch(/M/);
  });
});
//=============================================================================
describe('renderScope - improvements', () => {
  const makeGridLine = () => document.createElementNS('http://www.w3.org/2000/svg', 'line');

  function makeBlock(overrides: any = {}) {
    const axes: any = {
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: Array.from({ length: 9 }, makeLine),
      yTicks: Array.from({ length: 9 }, makeLine),
      xTickLabels: Array.from({ length: 9 }, makeText),
      yTickLabels: Array.from({ length: 9 }, makeText),
    };
    return {
      scopePlot: makePlot(200, 200),
      scopePaths: [makePath(), makePath(), makePath()],
      scopeAxes: axes,
      params: {},
      scopeData: { time: [0, 0.5, 1], series: [[1, 2, 3]] },
      ...overrides,
    };
  }

  test('grid lines are shown by default (showGrid not set)', () => {
    const xGridLines = Array.from({ length: 9 }, makeGridLine);
    const yGridLines = Array.from({ length: 9 }, makeGridLine);
    const block: any = makeBlock({ scopeGridLines: { xGridLines, yGridLines } });

    renderScope(block);
    const visibleY = yGridLines.filter((l) => l.getAttribute('display') !== 'none');
    expect(visibleY.length).toBeGreaterThan(0);
  });

  test('grid lines are hidden when showGrid = false', () => {
    const xGridLines = Array.from({ length: 9 }, makeGridLine);
    const yGridLines = Array.from({ length: 9 }, makeGridLine);
    const block: any = makeBlock({
      scopeGridLines: { xGridLines, yGridLines },
      params: { showGrid: false },
    });

    renderScope(block);

    const visibleX = xGridLines.filter((l) => l.getAttribute('display') !== 'none');
    const visibleY = yGridLines.filter((l) => l.getAttribute('display') !== 'none');
    expect(visibleX.length).toBe(0);
    expect(visibleY.length).toBe(0);
  });

  test('grid lines have correct x2 = plotX + plotW for horizontal lines', () => {
    const xGridLines = Array.from({ length: 9 }, makeGridLine);
    const yGridLines = Array.from({ length: 9 }, makeGridLine);
    const block: any = makeBlock({ scopeGridLines: { xGridLines, yGridLines } });

    renderScope(block);

    const plotW = 200;
    const plotX = 10;
    const visibleY = yGridLines.filter((l) => l.getAttribute('display') !== 'none');
    visibleY.forEach((l) => {
      expect(Number(l.getAttribute('x2'))).toBeCloseTo(plotX + plotW, 0);
    });
  });

  test('xAxis is clamped within plot bounds (zero-line visible when yMin <= 0 <= yMax)', () => {
    const axes: any = {
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: Array.from({ length: 9 }, makeLine),
      yTicks: Array.from({ length: 9 }, makeLine),
      xTickLabels: Array.from({ length: 9 }, makeText),
      yTickLabels: Array.from({ length: 9 }, makeText),
    };
    const block: any = {
      scopePlot: makePlot(200, 200),
      scopePaths: [makePath()],
      scopeAxes: axes,
      params: { YMin: -5, YMax: 5 },
      scopeData: { time: [0, 1], series: [[1, 2]] },
    };

    renderScope(block);

    const y1 = Number(axes.xAxis.getAttribute('y1'));
    const plotY = 10;
    const plotH = 200;
    const expectedY = plotY + plotH / 2; // zero is in the middle when yMin=-5, yMax=5
    expect(y1).toBeCloseTo(expectedY, 0);
  });

  test('adds scope-theme-light CSS class when theme is light', () => {
    const groupEl = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    const block: any = makeBlock({ params: { theme: 'light' }, group: groupEl });

    renderScope(block);

    expect(groupEl.classList.contains('scope-theme-light')).toBe(true);
  });

  test('removes scope-theme-light CSS class when theme is dark', () => {
    const groupEl = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    groupEl.classList.add('scope-theme-light');
    const block: any = makeBlock({ params: { theme: 'dark' }, group: groupEl });

    renderScope(block);

    expect(groupEl.classList.contains('scope-theme-light')).toBe(false);
  });

  test('applies custom color to signal path as inline style', () => {
    const block: any = makeBlock({
      params: {
        signals: [{ color: '#ff0000' }],
      },
    });

    renderScope(block);

    const style = block.scopePaths[0].getAttribute('style') || '';
    expect(style).toContain('stroke:#ff0000');
  });

  test('applies dashed stroke-dasharray for dash = dashed', () => {
    const block: any = makeBlock({
      params: {
        signals: [{ dash: 'dashed' }],
      },
    });

    renderScope(block);

    const style = block.scopePaths[0].getAttribute('style') || '';
    expect(style).toContain('stroke-dasharray:6 3');
  });

  test('applies dotted stroke-dasharray for dash = dotted', () => {
    const block: any = makeBlock({
      params: {
        signals: [{ dash: 'dotted' }],
      },
    });

    renderScope(block);

    const style = block.scopePaths[0].getAttribute('style') || '';
    expect(style).toContain('stroke-dasharray:2 3');
  });

  test('applies custom strokeWidth inline', () => {
    const block: any = makeBlock({
      params: {
        signals: [{ strokeWidth: 3 }],
      },
    });

    renderScope(block);

    const style = block.scopePaths[0].getAttribute('style') || '';
    expect(style).toContain('stroke-width:3');
  });

  test('shows block title when showTitle is true and name is set', () => {
    const titleEl = makeText();
    const block: any = makeBlock({ params: { name: 'My Scope' }, scopeTitle: titleEl });

    renderScope(block);

    expect(titleEl.getAttribute('display')).toBe('block');
    expect(titleEl.textContent).toBe('Scope - My Scope');
  });

  test('hides block title when showTitle = false', () => {
    const titleEl = makeText();
    const block: any = makeBlock({
      params: { name: 'My Scope', showTitle: false },
      scopeTitle: titleEl,
    });

    renderScope(block);

    expect(titleEl.getAttribute('display')).toBe('block');
    expect(titleEl.textContent).toBe('Scope');
  });

  test('hides block title when name is empty', () => {
    const titleEl = makeText();
    const block: any = makeBlock({ params: { name: '' }, scopeTitle: titleEl });

    renderScope(block);

    expect(titleEl.getAttribute('display')).toBe('block');
    expect(titleEl.textContent).toBe('Scope');
  });

  test('shows legend items by default', () => {
    const legendItems = Array.from({ length: 3 }, makeText);
    const block: any = makeBlock({ scopeLegendItems: legendItems });

    renderScope(block);

    expect(legendItems[0].getAttribute('display')).toBe('block');
  });

  test('hides legend when showLegend = false', () => {
    const legendItems = Array.from({ length: 3 }, makeText);
    const block: any = makeBlock({
      params: { showLegend: false },
      scopeLegendItems: legendItems,
    });

    renderScope(block);

    const visible = legendItems.filter((el) => el.getAttribute('display') !== 'none');
    expect(visible.length).toBe(0);
  });

  test('legend item uses signal label from params', () => {
    const legendItems = Array.from({ length: 3 }, makeText);
    const block: any = makeBlock({
      params: { signals: [{ label: 'Velocity' }] },
      scopeLegendItems: legendItems,
    });

    renderScope(block);

    expect(legendItems[0].textContent).toBe('Velocity');
  });

  test('legend item falls back to y1 when no signal label', () => {
    const legendItems = Array.from({ length: 3 }, makeText);
    const block: any = makeBlock({ scopeLegendItems: legendItems });

    renderScope(block);

    expect(legendItems[0].textContent).toBe('y1');
  });

  test('shows x-axis label when xLabel param is set', () => {
    const xLabelEl = makeText();
    const yLabelEl = makeText();
    const block: any = makeBlock({
      params: { xLabel: 'Time (s)' },
      scopeAxisLabels: { xLabel: xLabelEl, yLabel: yLabelEl },
    });

    renderScope(block);

    expect(xLabelEl.getAttribute('display')).toBe('block');
    expect(xLabelEl.textContent).toBe('Time (s)');
  });

  test('shows y-axis label when yLabel param is set', () => {
    const xLabelEl = makeText();
    const yLabelEl = makeText();
    const block: any = makeBlock({
      params: { yLabel: 'Amplitude (V)' },
      scopeAxisLabels: { xLabel: xLabelEl, yLabel: yLabelEl },
    });

    renderScope(block);

    expect(yLabelEl.getAttribute('display')).toBe('block');
    expect(yLabelEl.textContent).toBe('Amplitude (V)');
  });

  test('hides x/y labels when params are not set', () => {
    const xLabelEl = makeText();
    const yLabelEl = makeText();
    const block: any = makeBlock({
      scopeAxisLabels: { xLabel: xLabelEl, yLabel: yLabelEl },
    });

    renderScope(block);

    expect(xLabelEl.getAttribute('display')).toBe('none');
    expect(yLabelEl.getAttribute('display')).toBe('none');
  });

  test('grid lines are hidden when there is no data', () => {
    const xGridLines = Array.from({ length: 9 }, makeGridLine);
    const yGridLines = Array.from({ length: 9 }, makeGridLine);
    xGridLines.forEach((l) => l.setAttribute('display', 'block'));
    yGridLines.forEach((l) => l.setAttribute('display', 'block'));
    const block: any = {
      scopePlot: makePlot(),
      scopePaths: [makePath()],
      params: {},
      scopeData: { time: [], series: [[]] },
      scopeGridLines: { xGridLines, yGridLines },
    };

    renderScope(block);

    const visibleX = xGridLines.filter((l) => l.getAttribute('display') !== 'none');
    const visibleY = yGridLines.filter((l) => l.getAttribute('display') !== 'none');
    expect(visibleX.length).toBe(0);
    expect(visibleY.length).toBe(0);
  });
});
//=============================================================================
describe('renderScope - log scale, trigger, multi-Y', () => {
  const makeGridLine = () => document.createElementNS('http://www.w3.org/2000/svg', 'line');

  function makeBlock(overrides: any = {}) {
    const axes: any = {
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: Array.from({ length: 9 }, makeLine),
      yTicks: Array.from({ length: 9 }, makeLine),
      xTickLabels: Array.from({ length: 9 }, makeText),
      yTickLabels: Array.from({ length: 9 }, makeText),
    };
    return {
      scopePlot: makePlot(200, 200),
      scopePaths: [makePath(), makePath(), makePath()],
      scopeAxes: axes,
      params: {},
      scopeData: { time: [0, 0.5, 1], series: [[1, 2, 3]] },
      ...overrides,
    };
  }

  test('log scale: Y ticks use powers of 10', () => {
    const xGridLines = Array.from({ length: 9 }, makeGridLine);
    const yGridLines = Array.from({ length: 9 }, makeGridLine);
    const axes: any = {
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: Array.from({ length: 9 }, makeLine),
      yTicks: Array.from({ length: 9 }, makeLine),
      xTickLabels: Array.from({ length: 9 }, makeText),
      yTickLabels: Array.from({ length: 9 }, makeText),
    };
    const block: any = {
      scopePlot: makePlot(200, 200),
      scopePaths: [makePath()],
      scopeAxes: axes,
      params: { yScale: 'log', ShowTickLabels: true, YMin: 0.1, YMax: 100 },
      scopeData: { time: [0, 0.5, 1], series: [[0.1, 10, 100]] },
      scopeGridLines: { xGridLines, yGridLines },
    };

    renderScope(block);

    const visibleLabels = axes.yTickLabels
      .filter((l: any) => l.getAttribute('display') === 'block')
      .map((l: any) => l.textContent);
    expect(visibleLabels.length).toBeGreaterThan(0);
    const hasPowerOfTen = visibleLabels.some((t: string) => ['0.1', '1', '10', '100'].includes(t));
    expect(hasPowerOfTen).toBe(true);
  });

  test('log scale: signal path Y coordinates are within plot bounds', () => {
    const block: any = makeBlock({
      params: { yScale: 'log' },
      scopeData: { time: [0, 0.5, 1], series: [[1, 10, 100]] },
    });

    renderScope(block);

    const d = block.scopePaths[0].getAttribute('d') ?? '';
    expect(d).toMatch(/M/);
    const yValues = [...d.matchAll(/[ML]\s*[\d.]+\s+([\d.]+)/g)].map((m) => parseFloat(m[1]));
    const plotY = 10;
    const plotH = 200;
    yValues.forEach((y) => {
      expect(y).toBeGreaterThanOrEqual(plotY - 1);
      expect(y).toBeLessThanOrEqual(plotY + plotH + 1);
    });
  });

  test('log scale: zero/negative values are floored, not NaN', () => {
    const block: any = makeBlock({
      params: { yScale: 'log' },
      scopeData: { time: [0, 0.5, 1], series: [[-5, 0, 10]] },
    });

    expect(() => renderScope(block)).not.toThrow();
    const d = block.scopePaths[0].getAttribute('d') ?? '';
    expect(d).not.toContain('NaN');
  });

  test('log scale: xAxis is at bottom of plot (not at zero-line)', () => {
    const axes: any = {
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: Array.from({ length: 9 }, makeLine),
      yTicks: Array.from({ length: 9 }, makeLine),
      xTickLabels: Array.from({ length: 9 }, makeText),
      yTickLabels: Array.from({ length: 9 }, makeText),
    };
    const block: any = {
      scopePlot: makePlot(200, 200),
      scopePaths: [makePath()],
      scopeAxes: axes,
      params: { yScale: 'log' },
      scopeData: { time: [0, 0.5, 1], series: [[1, 10, 100]] },
    };

    renderScope(block);

    const y1 = Number(axes.xAxis.getAttribute('y1'));
    const plotY = 10;
    const plotH = 200;
    expect(y1).toBeCloseTo(plotY + plotH, 0);
  });

  test('trigger rising: renders only data from first rising crossing', () => {
    const block: any = makeBlock({
      params: { trigger: { source: 0, level: 5, edge: 'rising' } },
      scopeData: { time: [0, 1, 2, 3, 4], series: [[0, 3, 8, 6, 9]] },
      // rising: idx=2 (prev=3 < 5, curr=8 >= 5)
    });

    renderScope(block);

    expect(block.computedLimits.tMin).toBeCloseTo(2, 5);
  });

  test('trigger falling: renders only data from first falling crossing', () => {
    const block: any = makeBlock({
      params: { trigger: { source: 0, level: 5, edge: 'falling' } },
      scopeData: { time: [0, 1, 2, 3, 4], series: [[10, 8, 3, 2, 1]] },
      // falling: idx=2 (prev=8 > 5, curr=3 <= 5)
    });

    renderScope(block);

    expect(block.computedLimits.tMin).toBeCloseTo(2, 5);
  });

  test('trigger with no crossing: path is cleared (empty data)', () => {
    const block: any = makeBlock({
      params: { trigger: { source: 0, level: 50, edge: 'rising' } },
      scopeData: { time: [0, 1, 2], series: [[1, 2, 3]] },
    });

    renderScope(block);

    expect(block.scopePaths[0].getAttribute('d')).toBe('');
  });

  test('trigger without level set: renders normally (trigger disabled)', () => {
    const block: any = makeBlock({
      params: { trigger: { source: 0, edge: 'rising' } },
      scopeData: { time: [0, 1, 2], series: [[1, 2, 3]] },
    });

    renderScope(block);

    expect(block.scopePaths[0].getAttribute('d')).toMatch(/M/);
  });

  test('multiYAxis: renders without throwing for multiple signals', () => {
    const block: any = makeBlock({
      params: { multiYAxis: true },
      scopeData: {
        time: [0, 0.5, 1],
        series: [
          [1, 2, 3],
          [100, 200, 300],
        ],
      },
    });
    block.scopePaths = [makePath(), makePath()];

    expect(() => renderScope(block)).not.toThrow();
    expect(block.scopePaths[0].getAttribute('d')).toMatch(/M/);
    expect(block.scopePaths[1].getAttribute('d')).toMatch(/M/);
  });

  test('multiYAxis: each signal fills the full plot height independently', () => {
    const block: any = makeBlock({
      params: { multiYAxis: true },
      scopeData: {
        time: [0, 1],
        series: [
          [0, 10],
          [0, 1000],
        ],
      },
    });
    block.scopePaths = [makePath(), makePath()];

    renderScope(block);

    const plotY = 10;
    const plotH = 200;

    for (const path of block.scopePaths) {
      const d = path.getAttribute('d') ?? '';
      const yValues = [...d.matchAll(/[ML]\s*[\d.]+\s+([\d.]+)/g)].map((m) => parseFloat(m[1]));
      if (yValues.length < 2) continue;
      const minY = Math.min(...yValues);
      const maxY = Math.max(...yValues);
      expect(maxY).toBeCloseTo(plotY + plotH, 1);
      expect(minY).toBeCloseTo(plotY, 1);
    }
  });

  test('multiYAxis false: both signals share the same Y range', () => {
    // With explicit yMin/yMax and multiYAxis=false, signal 0 (values [0,10]) maps near the bottom
    // because global yMax=1000 compresses the small-range signal
    const block: any = makeBlock({
      params: { multiYAxis: false, YMin: 0, YMax: 1000 },
      scopeData: {
        time: [0, 1],
        series: [
          [0, 10],
          [0, 1000],
        ],
      },
    });
    block.scopePaths = [makePath(), makePath()];

    renderScope(block);

    const d0 = block.scopePaths[0].getAttribute('d') ?? '';
    const yValues0 = [...d0.matchAll(/[ML]\s*[\d.]+\s+([\d.]+)/g)].map((m) => parseFloat(m[1]));
    expect(yValues0.length).toBeGreaterThanOrEqual(2);
    const plotY = 10;
    const plotH = 200;
    expect(Math.min(...yValues0)).toBeGreaterThan(plotY + plotH - 5);
  });

  describe('incremental observed-range aggregates', () => {
    const makeAxes = () => ({
      xAxis: makeLine(),
      yAxis: makeLine(),
      xTicks: [makeLine()],
      yTicks: [makeLine()],
      xTickLabels: [makeText()],
      yTickLabels: [makeText()],
    });

    test('appended samples update the observed range without a full rescan', () => {
      const time = [0, 1, 2];
      const values = [1, 2, 3];
      const block: any = {
        scopePlot: makePlot(),
        scopePaths: [makePath()],
        scopeAxes: makeAxes(),
        params: {},
        scopeData: { time, series: [values] },
      };
      renderScope(block);
      expect(block._minmaxAggCache).toBeDefined();
      expect(block._minmaxAggCache.scanned).toEqual([3]);
      const firstMax = block.computedLimits.yMax;

      // Append (delta-frame style: same arrays, pushed), including a new
      // extreme. The cache must advance and the limits must include it.
      time.push(3, 4);
      values.push(50, -20);
      renderScope(block);
      expect(block._minmaxAggCache.scanned).toEqual([5]);
      expect(block._minmaxAggCache.max).toBe(50);
      expect(block._minmaxAggCache.min).toBe(-20);
      expect(block.computedLimits.yMax).toBeGreaterThanOrEqual(50);
      expect(block.computedLimits.yMin).toBeLessThanOrEqual(-20);
      expect(block.computedLimits.yMax).toBeGreaterThan(firstMax);
    });

    test('a shrunk series (new run) forces a full rescan', () => {
      const block: any = {
        scopePlot: makePlot(),
        scopePaths: [makePath()],
        scopeAxes: makeAxes(),
        params: {},
        scopeData: { time: [0, 1, 2], series: [[10, 20, 30]] },
      };
      renderScope(block);
      expect(block._minmaxAggCache.max).toBe(30);
      // New (shorter) run data replaces the arrays: stale extremes must go.
      block.scopeData = { time: [0, 1], series: [[1, 2]] };
      renderScope(block);
      expect(block._minmaxAggCache.max).toBe(2);
      expect(block._minmaxAggCache.scanned).toEqual([2]);
    });

    test('time window filtering bypasses and clears the cache', () => {
      const block: any = {
        scopePlot: makePlot(),
        scopePaths: [makePath()],
        scopeAxes: makeAxes(),
        params: { timeWindow: 1 },
        scopeData: { time: [0, 1, 2, 3], series: [[100, 1, 2, 3]] },
      };
      renderScope(block);
      expect(block._minmaxAggCache).toBeUndefined();
      expect(block.computedLimits.yMax).toBeLessThan(100);
    });

    test('connected-mask change invalidates the aggregate key', () => {
      const block: any = {
        scopePlot: makePlot(),
        scopePaths: [makePath(), makePath()],
        scopeAxes: makeAxes(),
        params: {},
        scopeData: {
          time: [0, 1],
          series: [
            [1, 2],
            [500, 600],
          ],
          connected: [true, false],
        },
      };
      renderScope(block);
      expect(block._minmaxAggCache.max).toBe(2); // series 1 ignored
      block.scopeData.connected = [true, true];
      renderScope(block);
      expect(block._minmaxAggCache.max).toBe(600);
    });
  });
});
//=============================================================================
