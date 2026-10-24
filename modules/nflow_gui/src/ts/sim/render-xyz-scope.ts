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
  niceStep as _niceStep,
  buildTicks as _buildTicks,
  formatTick as _formatTick,
  roundToSignificant,
  refreshParamDisplay,
} from './scope-utils.js';
import { stridePoints } from './minmax-reduce.js';
import { areScopeSeriesConnected } from './scope-connections.js';
//=============================================================================
export function renderXYZScope(scopeBlock: any) {
  if (!scopeBlock.scopePaths || !scopeBlock.scopePlot || !scopeBlock.xyzScopeData) return;
  const { connected } = scopeBlock.xyzScopeData;
  if (!areScopeSeriesConnected(connected, 3)) {
    scopeBlock.scopePaths.forEach((path: any) => path.setAttribute('d', ''));
    if (scopeBlock.scopeAxes?.xTickLabels)
      scopeBlock.scopeAxes.xTickLabels.forEach((label: any) =>
        label.setAttribute('display', 'none')
      );
    if (scopeBlock.scopeAxes?.yTickLabels)
      scopeBlock.scopeAxes.yTickLabels.forEach((label: any) =>
        label.setAttribute('display', 'none')
      );
    if (scopeBlock.scopeAxes?.zTickLabels)
      scopeBlock.scopeAxes.zTickLabels.forEach((label: any) =>
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
  const { series } = scopeBlock.xyzScopeData;
  const xSeries = series.x || [];
  const ySeries = series.y || [];
  const zSeries = series.z || [];
  const xMinParam = parseLimit(scopeBlock.params?.XMin);
  const xMaxParam = parseLimit(scopeBlock.params?.XMax);
  const yMinParam = parseLimit(scopeBlock.params?.YMin);
  const yMaxParam = parseLimit(scopeBlock.params?.YMax);
  const zMinParam = parseLimit(scopeBlock.params?.ZMin);
  const zMaxParam = parseLimit(scopeBlock.params?.ZMax);
  const points: any[] = [];
  let observedXMin = Infinity;
  let observedXMax = -Infinity;
  let observedYMin = Infinity;
  let observedYMax = -Infinity;
  let observedZMin = Infinity;
  let observedZMax = -Infinity;
  const pointCount = Math.min(xSeries.length, ySeries.length, zSeries.length);
  for (let idx = 0; idx < pointCount; idx += 1) {
    const x = xSeries[idx];
    const y = ySeries[idx];
    const z = zSeries[idx];
    if (
      x == null ||
      y == null ||
      z == null ||
      !Number.isFinite(x) ||
      !Number.isFinite(y) ||
      !Number.isFinite(z)
    )
      continue;
    points.push({ x, y, z });
    if (x < observedXMin) observedXMin = x;
    if (x > observedXMax) observedXMax = x;
    if (y < observedYMin) observedYMin = y;
    if (y > observedYMax) observedYMax = y;
    if (z < observedZMin) observedZMin = z;
    if (z > observedZMax) observedZMax = z;
  }
  if (points.length === 0) {
    scopeBlock.scopePaths.forEach((path: any) => path.setAttribute('d', ''));
    if (axes?.xTickLabels)
      axes.xTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
    if (axes?.yTickLabels)
      axes.yTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
    if (axes?.zTickLabels)
      axes.zTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
    scopeBlock.computedLimits = {
      xMin: xMinParam != null ? xMinParam : -1.2,
      xMax: xMaxParam != null ? xMaxParam : 1.2,
      yMin: yMinParam != null ? yMinParam : -1.2,
      yMax: yMaxParam != null ? yMaxParam : 1.2,
      zMin: zMinParam != null ? zMinParam : -1.2,
      zMax: zMaxParam != null ? zMaxParam : 1.2,
    };
    refreshParamDisplay(scopeBlock);
    return;
  }
  let xMin = observedXMin;
  let xMax = observedXMax;
  let yMin = observedYMin;
  let yMax = observedYMax;
  let zMin = observedZMin;
  let zMax = observedZMax;
  if (xMinParam != null) xMin = xMinParam;
  if (xMaxParam != null) xMax = xMaxParam;
  if (yMinParam != null) yMin = yMinParam;
  if (yMaxParam != null) yMax = yMaxParam;
  if (zMinParam != null) zMin = zMinParam;
  if (zMaxParam != null) zMax = zMaxParam;
  if (xMax === xMin) {
    xMax += 1;
    xMin -= 1;
  }
  if (yMax === yMin) {
    yMax += 1;
    yMin -= 1;
  }
  if (zMax === zMin) {
    zMax += 1;
    zMin -= 1;
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
  if (zMinParam == null && zMaxParam == null) {
    const maxAbs = Math.max(Math.abs(zMax), Math.abs(zMin), 1e-6);
    zMax = roundToSignificant(maxAbs * 1.2, 2);
    zMin = roundToSignificant(-maxAbs * 1.2, 2);
  }
  const xRange = xMax - xMin;
  const yRange = yMax - yMin;
  const zRange = zMax - zMin;
  const centerX = plotX + plotW / 2;
  const centerY = plotY + plotH / 2;
  const maxRange = Math.max(xRange, yRange, zRange);
  const scale = (Math.min(plotW, plotH) / maxRange) * 0.7;
  const rotationX = ((scopeBlock.params?.RotationX ?? 30) * Math.PI) / 180;
  const rotationY = ((scopeBlock.params?.RotationY ?? 45) * Math.PI) / 180;
  const project3D = (x: number, y: number, z: number) => {
    const nx = (x - xMin - xRange / 2) * scale;
    const ny = (y - yMin - yRange / 2) * scale;
    const nz = (z - zMin - zRange / 2) * scale;
    const cosX = Math.cos(rotationX);
    const sinX = Math.sin(rotationX);
    const cosY = Math.cos(rotationY);
    const sinY = Math.sin(rotationY);
    const y1 = ny * cosX - nz * sinX;
    const z1 = ny * sinX + nz * cosX;
    const x2 = nx * cosY + z1 * sinY;
    return {
      x: centerX + x2,
      y: centerY + y1,
    };
  };
  if (axes) {
    const origin = project3D(0, 0, 0);
    const xAxisEnd = project3D(xMax, 0, 0);
    const yAxisEnd = project3D(0, yMax, 0);
    const zAxisEnd = project3D(0, 0, zMax);

    const xAxisDir = { x: xAxisEnd.x - origin.x, y: xAxisEnd.y - origin.y };
    const yAxisDir = { x: yAxisEnd.x - origin.x, y: yAxisEnd.y - origin.y };
    const zAxisDir = { x: zAxisEnd.x - origin.x, y: zAxisEnd.y - origin.y };

    const xAxisLen = Math.sqrt(xAxisDir.x * xAxisDir.x + xAxisDir.y * xAxisDir.y);
    const yAxisLen = Math.sqrt(yAxisDir.x * yAxisDir.x + yAxisDir.y * yAxisDir.y);
    const zAxisLen = Math.sqrt(zAxisDir.x * zAxisDir.x + zAxisDir.y * zAxisDir.y);

    const xAxisNorm =
      xAxisLen > 0 ? { x: xAxisDir.x / xAxisLen, y: xAxisDir.y / xAxisLen } : { x: 1, y: 0 };
    const yAxisNorm =
      yAxisLen > 0 ? { x: yAxisDir.x / yAxisLen, y: yAxisDir.y / yAxisLen } : { x: 0, y: 1 };
    const zAxisNorm =
      zAxisLen > 0 ? { x: zAxisDir.x / zAxisLen, y: zAxisDir.y / zAxisLen } : { x: 0, y: 1 };

    const _xAxisPerp = { x: -xAxisNorm.y, y: xAxisNorm.x };
    const _yAxisPerp = { x: -yAxisNorm.y, y: yAxisNorm.x };
    const _zAxisPerp = { x: -zAxisNorm.y, y: zAxisNorm.x };
    void _xAxisPerp;
    void _yAxisPerp;
    void _zAxisPerp;

    axes.xAxis.setAttribute('x1', origin.x);
    axes.xAxis.setAttribute('y1', origin.y);
    axes.xAxis.setAttribute('x2', xAxisEnd.x);
    axes.xAxis.setAttribute('y2', xAxisEnd.y);
    axes.yAxis.setAttribute('x1', origin.x);
    axes.yAxis.setAttribute('y1', origin.y);
    axes.yAxis.setAttribute('x2', yAxisEnd.x);
    axes.yAxis.setAttribute('y2', yAxisEnd.y);
    axes.zAxis.setAttribute('x1', origin.x);
    axes.zAxis.setAttribute('y1', origin.y);
    axes.zAxis.setAttribute('x2', zAxisEnd.x);
    axes.zAxis.setAttribute('y2', zAxisEnd.y);

    axes.xTicks.forEach((tick: any) => tick.setAttribute('display', 'none'));
    axes.yTicks.forEach((tick: any) => tick.setAttribute('display', 'none'));
    axes.zTicks.forEach((tick: any) => tick.setAttribute('display', 'none'));
    axes.xTickLabels?.forEach((label: any) => label.setAttribute('display', 'none'));
    axes.yTickLabels?.forEach((label: any) => label.setAttribute('display', 'none'));
    axes.zTickLabels?.forEach((label: any) => label.setAttribute('display', 'none'));
  }
  // Bound the parametric trace by the plot resolution: an index
  // stride keeps first/last and the overall shape while capping segments.
  const strideIdx = stridePoints(points.length, Math.max(64, 4 * Math.ceil(plotW)));
  const drawnPoints = strideIdx ? strideIdx.map((k) => points[k]) : points;
  const path = drawnPoints
    .map((p, i) => {
      const proj = project3D(p.x, p.y, p.z);
      return `${i === 0 ? 'M' : 'L'} ${proj.x} ${proj.y}`;
    })
    .join(' ');
  const pathEl = scopeBlock.scopePaths[0];
  if (pathEl) pathEl.setAttribute('d', path);
  scopeBlock.computedLimits = {
    xMin,
    xMax,
    yMin,
    yMax,
    zMin,
    zMax,
  };
  refreshParamDisplay(scopeBlock);
}
//=============================================================================
