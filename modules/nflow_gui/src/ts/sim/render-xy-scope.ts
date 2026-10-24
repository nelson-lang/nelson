//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { createSvgElement as _createSvgElement } from '../render/svg-elements.js';
import {
  parseLimit,
  niceStep,
  buildTicks,
  formatTick,
  roundToSignificant,
  refreshParamDisplay,
} from './scope-utils.js';
import { stridePoints } from './minmax-reduce.js';
import { areScopeSeriesConnected } from './scope-connections.js';
//=============================================================================
export function renderXYScope(scopeBlock: any) {
  if (!scopeBlock.scopePaths || !scopeBlock.scopePlot || !scopeBlock.xyScopeData) return;
  const { connected } = scopeBlock.xyScopeData;
  if (!areScopeSeriesConnected(connected, 2)) {
    scopeBlock.scopePaths.forEach((path: any) => path.setAttribute('d', ''));
    if (scopeBlock.scopeAxes?.xTickLabels)
      scopeBlock.scopeAxes.xTickLabels.forEach((label: any) =>
        label.setAttribute('display', 'none')
      );
    if (scopeBlock.scopeAxes?.yTickLabels)
      scopeBlock.scopeAxes.yTickLabels.forEach((label: any) =>
        label.setAttribute('display', 'none')
      );
    refreshParamDisplay(scopeBlock);
    return;
  }
  const plot = scopeBlock.scopePlot;
  const plotX = Number(plot.getAttribute('x'));
  const plotY = Number(plot.getAttribute('y'));
  const plotW = Number(plot.getAttribute('width'));
  const plotH = Number(plot.getAttribute('height'));
  const axes = scopeBlock.scopeAxes;
  const showTickLabels = scopeBlock.params?.ShowTickLabels === true;
  const { series } = scopeBlock.xyScopeData;
  const xSeries = series.x || [];
  const ySeries = series.y || [];
  const xMinParam = parseLimit(scopeBlock.params?.XMin);
  const xMaxParam = parseLimit(scopeBlock.params?.XMax);
  const yMinParam = parseLimit(scopeBlock.params?.YMin);
  const yMaxParam = parseLimit(scopeBlock.params?.YMax);
  const pairs: any[] = [];
  let observedXMin = Infinity;
  let observedXMax = -Infinity;
  let observedYMin = Infinity;
  let observedYMax = -Infinity;
  const pairCount = Math.min(xSeries.length, ySeries.length);
  for (let idx = 0; idx < pairCount; idx += 1) {
    const x = xSeries[idx];
    const y = ySeries[idx];
    if (x == null || y == null || !Number.isFinite(x) || !Number.isFinite(y)) continue;
    pairs.push({ x, y });
    if (x < observedXMin) observedXMin = x;
    if (x > observedXMax) observedXMax = x;
    if (y < observedYMin) observedYMin = y;
    if (y > observedYMax) observedYMax = y;
  }
  if (pairs.length === 0) {
    scopeBlock.scopePaths.forEach((path: any) => path.setAttribute('d', ''));
    if (axes?.xTickLabels)
      axes.xTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
    if (axes?.yTickLabels)
      axes.yTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
    if (scopeBlock.scopeGridLines) {
      scopeBlock.scopeGridLines.xGridLines.forEach((l: any) => l.setAttribute('display', 'none'));
      scopeBlock.scopeGridLines.yGridLines.forEach((l: any) => l.setAttribute('display', 'none'));
    }
    scopeBlock.computedLimits = {
      xMin: xMinParam != null ? xMinParam : -1.2,
      xMax: xMaxParam != null ? xMaxParam : 1.2,
      yMin: yMinParam != null ? yMinParam : -1.2,
      yMax: yMaxParam != null ? yMaxParam : 1.2,
    };
    refreshParamDisplay(scopeBlock);
    return;
  }
  let xMin = observedXMin;
  let xMax = observedXMax;
  let yMin = observedYMin;
  let yMax = observedYMax;
  if (xMinParam != null) xMin = xMinParam;
  if (xMaxParam != null) xMax = xMaxParam;
  if (yMinParam != null) yMin = yMinParam;
  if (yMaxParam != null) yMax = yMaxParam;
  if (xMax === xMin) {
    xMax += 1;
    xMin -= 1;
  }
  if (yMax === yMin) {
    yMax += 1;
    yMin -= 1;
  }
  if (xMinParam == null && xMaxParam == null) {
    const maxAbs = Math.max(Math.abs(xMax), Math.abs(xMin), 1e-6);
    xMax = maxAbs * 1.2;
    xMin = -maxAbs * 1.2;
  }
  if (yMinParam == null && yMaxParam == null) {
    const maxAbs = Math.max(Math.abs(yMax), Math.abs(yMin), 1e-6);
    yMax = roundToSignificant(maxAbs * 1.2, 2);
    yMin = roundToSignificant(-maxAbs * 1.2, 2);
  }
  const xRange = xMax - xMin;
  const yRange = yMax - yMin;
  if (axes) {
    const xZero = plotX + ((0 - xMin) / xRange) * plotW;
    const yZero = plotY + plotH - ((0 - yMin) / yRange) * plotH;
    const xAxisY = Math.max(plotY, Math.min(plotY + plotH, yZero));
    const yAxisX = Math.max(plotX, Math.min(plotX + plotW, xZero));
    axes.xAxis.setAttribute('x1', plotX);
    axes.xAxis.setAttribute('y1', xAxisY);
    axes.xAxis.setAttribute('x2', plotX + plotW);
    axes.xAxis.setAttribute('y2', xAxisY);
    axes.yAxis.setAttribute('x1', yAxisX);
    axes.yAxis.setAttribute('y1', plotY);
    axes.yAxis.setAttribute('x2', yAxisX);
    axes.yAxis.setAttribute('y2', plotY + plotH);
    const tickLen = 5;
    const xStep = niceStep(xRange, 5);
    const yStep = niceStep(yRange, 5);
    const xTickVals = buildTicks(xMin, xMax, xStep);
    const yTickVals = buildTicks(yMin, yMax, yStep);
    axes.xTicks.forEach((tick: any, idx: number) => {
      if (idx >= xTickVals.length) {
        tick.setAttribute('display', 'none');
        axes.xTickLabels?.[idx]?.setAttribute('display', 'none');
        return;
      }
      tick.setAttribute('display', 'block');
      const v = xTickVals[idx];
      const x = plotX + ((v - xMin) / xRange) * plotW;
      tick.setAttribute('x1', x);
      tick.setAttribute('y1', xAxisY - tickLen);
      tick.setAttribute('x2', x);
      tick.setAttribute('y2', xAxisY + tickLen);
      const label = axes.xTickLabels?.[idx];
      if (label) {
        if (showTickLabels) {
          label.setAttribute('display', 'block');
          label.setAttribute('x', x + 2);
          label.setAttribute('y', xAxisY - 6);
          label.textContent = formatTick(v);
        } else {
          label.setAttribute('display', 'none');
        }
      }
    });
    axes.yTicks.forEach((tick: any, idx: number) => {
      if (idx >= yTickVals.length) {
        tick.setAttribute('display', 'none');
        axes.yTickLabels?.[idx]?.setAttribute('display', 'none');
        return;
      }
      tick.setAttribute('display', 'block');
      const v = yTickVals[idx];
      const y = plotY + plotH - ((v - yMin) / yRange) * plotH;
      tick.setAttribute('x1', yAxisX - tickLen);
      tick.setAttribute('y1', y);
      tick.setAttribute('x2', yAxisX + tickLen);
      tick.setAttribute('y2', y);
      const label = axes.yTickLabels?.[idx];
      if (label) {
        if (showTickLabels) {
          label.setAttribute('display', 'block');
          label.setAttribute('x', yAxisX + 8);
          label.setAttribute('y', y - 2);
          label.textContent = formatTick(v);
        } else {
          label.setAttribute('display', 'none');
        }
      }
    });

    const showGrid = scopeBlock.params?.showGrid !== false;
    if (scopeBlock.scopeGridLines) {
      const { xGridLines, yGridLines } = scopeBlock.scopeGridLines;
      xTickVals.forEach((v: number, idx: number) => {
        const line = xGridLines[idx];
        if (!line) return;
        if (!showGrid) {
          line.setAttribute('display', 'none');
          return;
        }
        const x = plotX + ((v - xMin) / xRange) * plotW;
        line.setAttribute('display', 'block');
        line.setAttribute('x1', String(x));
        line.setAttribute('y1', String(plotY));
        line.setAttribute('x2', String(x));
        line.setAttribute('y2', String(plotY + plotH));
      });
      for (let i = xTickVals.length; i < xGridLines.length; i++) {
        xGridLines[i].setAttribute('display', 'none');
      }
      yTickVals.forEach((v: number, idx: number) => {
        const line = yGridLines[idx];
        if (!line) return;
        if (!showGrid) {
          line.setAttribute('display', 'none');
          return;
        }
        const y = plotY + plotH - ((v - yMin) / yRange) * plotH;
        line.setAttribute('display', 'block');
        line.setAttribute('x1', String(plotX));
        line.setAttribute('y1', String(y));
        line.setAttribute('x2', String(plotX + plotW));
        line.setAttribute('y2', String(y));
      });
      for (let i = yTickVals.length; i < yGridLines.length; i++) {
        yGridLines[i].setAttribute('display', 'none');
      }
    }
  }
  // Bound the parametric trace by the plot resolution: an index
  // stride keeps first/last and the overall shape while capping segments.
  const strideIdx = stridePoints(pairs.length, Math.max(64, 4 * Math.ceil(plotW)));
  const drawnPairs = strideIdx ? strideIdx.map((k) => pairs[k]) : pairs;
  const path = drawnPairs
    .map((p, i) => {
      const x = plotX + ((p.x - xMin) / xRange) * plotW;
      const y = plotY + plotH - ((p.y - yMin) / yRange) * plotH;
      return `${i === 0 ? 'M' : 'L'} ${x} ${y}`;
    })
    .join(' ');
  const pathEl = scopeBlock.scopePaths[0];
  if (pathEl) pathEl.setAttribute('d', path);

  scopeBlock.computedLimits = {
    xMin,
    xMax,
    yMin,
    yMax,
  };
  refreshParamDisplay(scopeBlock);
}
//=============================================================================
