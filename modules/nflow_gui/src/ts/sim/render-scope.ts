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
import { createSvgElement } from '../render/svg-elements.js';
import { reduceMinMax } from './minmax-reduce.js';
import {
  firstConnectedScopeSeries,
  isScopeSeriesConnected,
  scopeConnectionKey,
} from './scope-connections.js';
import {
  parseLimit,
  niceStep,
  buildTicks,
  formatTick,
  roundToSignificant,
  refreshParamDisplay,
  buildLogTicks,
  logValueToPixelY,
  LOG_FLOOR,
} from './scope-utils.js';
//=============================================================================
/** Apply per-signal inline style overrides (color, strokeWidth, dash) to a path element. */
function applySignalStyle(pathEl: Element, sigParams: any, fallbackClass: string) {
  const color = sigParams?.color;
  const strokeWidth = sigParams?.strokeWidth;
  const dash = sigParams?.dash;

  let style = '';
  if (color) style += `stroke:${color};`;
  if (strokeWidth != null) style += `stroke-width:${strokeWidth};`;
  if (dash === 'dashed') style += 'stroke-dasharray:6 3;';
  else if (dash === 'dotted') style += 'stroke-dasharray:2 3;';
  else if (dash === 'solid') style += 'stroke-dasharray:none;';

  // Preserve the fallback palette class but strip any previous per-signal class
  const currentClass = pathEl.getAttribute('class') || '';
  if (color) {
    const stripped = currentClass.replace(/\bscope-path-\d\b/g, fallbackClass).trim();
    pathEl.setAttribute('class', stripped);
  }
  if (style) pathEl.setAttribute('style', style);
}
//=============================================================================
export function renderScope(scopeBlock: any) {
  if (scopeBlock.type === 'xyScope') {
    return;
  }
  if (scopeBlock.type === 'xyzScope') {
    return;
  }

  const theme = scopeBlock.params?.theme ?? 'dark';
  const group: Element = scopeBlock.group;
  if (group?.classList) {
    if (theme === 'light') group.classList.add('scope-theme-light');
    else group.classList.remove('scope-theme-light');
  }

  if (!scopeBlock.scopePaths || !scopeBlock.scopePlot || !scopeBlock.scopeData) return;
  const formatTime = (t: number, t0: number, t1: number) => {
    const range = Math.abs((t1 ?? 0) - (t0 ?? 0));
    let scale = 1;
    let unit = 's';
    if (range < 1e-3) {
      scale = 1e6;
      unit = 'µs';
    } else if (range < 1) {
      scale = 1e3;
      unit = 'ms';
    }
    return { value: t * scale, unit };
  };
  const plot = scopeBlock.scopePlot;
  const plotX = Number(plot.getAttribute('x'));
  const plotY = Number(plot.getAttribute('y'));
  const plotW = Number(plot.getAttribute('width'));
  const plotH = Number(plot.getAttribute('height'));

  try {
    const bgColor = scopeBlock.params?.backgroundColor as string | undefined;
    if (bgColor && String(bgColor).trim()) {
      // Apply as inline style with high priority so it overrides CSS rules.
      try {
        (plot as unknown as HTMLElement).style.setProperty('fill', String(bgColor), 'important');
      } catch (e) {
        try {
          plot.setAttribute('fill', String(bgColor));
        } catch (e) {}
      }
    } else {
      try {
        (plot as unknown as HTMLElement).style.removeProperty('fill');
      } catch (e) {}
      try {
        plot.removeAttribute('fill');
      } catch (e) {}
    }
  } catch (e) {}

  // Block title: the type label ('Scope') sits top-left with the block name appended
  // when `showTitle` is on; the centered `scopeBlockTitle` stays hidden.
  try {
    const typeLabelEl = scopeBlock.scopeTitle;
    const centeredTitleEl = scopeBlock.scopeBlockTitle;
    const showTitle = scopeBlock.params?.showTitle !== false;
    const nameText = String(scopeBlock.params?.name || '').trim();
    if (typeLabelEl) {
      const combined = nameText && showTitle ? `Scope - ${nameText}` : 'Scope';
      try {
        typeLabelEl.setAttribute('display', 'block');
        typeLabelEl.textContent = combined;
      } catch (e) {}
    }
    if (centeredTitleEl) {
      try {
        centeredTitleEl.setAttribute('display', 'none');
      } catch (e) {}
    }
  } catch (e) {}

  const axes = scopeBlock.scopeAxes;
  const showTickLabels = scopeBlock.params?.ShowTickLabels === true;
  const showGrid = scopeBlock.params?.showGrid !== false;
  const { connected } = scopeBlock.scopeData;
  // Sliding time window: keep only samples within the last N seconds.
  const timeWindow = parseLimit(scopeBlock.params?.timeWindow);
  let { time, series } = scopeBlock.scopeData as { time: number[]; series: any[][] };
  if (timeWindow != null && timeWindow > 0 && time.length > 0) {
    const tEnd = Number(time[time.length - 1]);
    const tStart = tEnd - timeWindow;
    const firstIdx = time.findIndex((t: number) => Number(t) >= tStart);
    if (firstIdx > 0) {
      time = time.slice(firstIdx);
      series = series.map((s: any[]) => s.slice(firstIdx));
    }
  }

  const triggerParam = scopeBlock.params?.trigger as
    | { source?: number; level?: number; edge?: string }
    | undefined;
  if (triggerParam && triggerParam.level != null) {
    const srcIdx = triggerParam.source ?? 0;
    const level = Number(triggerParam.level);
    const edge = triggerParam.edge ?? 'rising';
    const srcSeries: number[] = series[srcIdx] ?? [];
    let triggerIdx = -1;
    for (let i = 1; i < srcSeries.length; i++) {
      const prev = Number(srcSeries[i - 1]);
      const curr = Number(srcSeries[i]);
      if (edge === 'rising' && prev < level && curr >= level) {
        triggerIdx = i;
        break;
      }
      if (edge === 'falling' && prev > level && curr <= level) {
        triggerIdx = i;
        break;
      }
    }
    if (triggerIdx >= 0) {
      time = time.slice(triggerIdx);
      series = series.map((s: any[]) => s.slice(triggerIdx));
    } else {
      scopeBlock.scopePaths.forEach((path: any) => path.setAttribute('d', ''));
      if (axes?.xTickLabels)
        axes.xTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
      if (axes?.yTickLabels)
        axes.yTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
      if (scopeBlock.scopeGridLines) {
        scopeBlock.scopeGridLines.xGridLines.forEach((l: any) => l.setAttribute('display', 'none'));
        scopeBlock.scopeGridLines.yGridLines.forEach((l: any) => l.setAttribute('display', 'none'));
      }
      refreshParamDisplay(scopeBlock);
      return;
    }
  }
  const t0Raw = Number(time[0] ?? 0);
  const t1Raw = Number(time[time.length - 1] ?? t0Raw);
  const tMinParam = parseLimit(scopeBlock.params?.TMin);
  const tMaxParam = parseLimit(scopeBlock.params?.TMax);
  const yMinParam = parseLimit(scopeBlock.params?.YMin);
  const yMaxParam = parseLimit(scopeBlock.params?.YMax);
  const t0 = tMinParam != null ? tMinParam : t0Raw;
  const t1 = tMaxParam != null ? tMaxParam : t1Raw;
  const tRange = t1 - t0;
  // Observed range of the connected series. On the unfiltered path the data
  // only ever APPENDS between two renders of the same run (delta frames and
  // cumulative snapshots alike), so the aggregates are maintained
  // incrementally: only the new tail of each series is scanned per frame
  // instead of the whole history.
  // Any active filter (time window, trigger) slices the arrays, so those
  // modes keep the exact full scan.
  const filtersActive =
    (timeWindow != null && timeWindow > 0) || (triggerParam && triggerParam.level != null);
  const aggKey = `${series.length}:${scopeConnectionKey(connected, series.length)}`;
  let observedMin = Infinity;
  let observedMax = -Infinity;
  let observedCount = 0;
  const prevAgg = scopeBlock._minmaxAggCache as
    | { key: string; scanned: number[]; min: number; max: number; count: number }
    | undefined;
  const canIncrement =
    !filtersActive &&
    prevAgg &&
    prevAgg.key === aggKey &&
    series.every((s: any[], idx: number) => (s?.length ?? 0) >= prevAgg.scanned[idx]);
  if (canIncrement && prevAgg) {
    observedMin = prevAgg.min;
    observedMax = prevAgg.max;
    observedCount = prevAgg.count;
  }
  series.forEach((seriesValues: any[], idx: number) => {
    if (!isScopeSeriesConnected(connected, idx)) return;
    const start = canIncrement && prevAgg ? prevAgg.scanned[idx] : 0;
    for (let k = start; k < seriesValues.length; k += 1) {
      const v = seriesValues[k];
      if (v == null || !Number.isFinite(v)) continue;
      observedCount += 1;
      if (v < observedMin) observedMin = v;
      if (v > observedMax) observedMax = v;
    }
  });
  if (!filtersActive) {
    scopeBlock._minmaxAggCache = {
      key: aggKey,
      scanned: series.map((s: any[]) => s?.length ?? 0),
      min: observedMin,
      max: observedMax,
      count: observedCount,
    };
  } else {
    scopeBlock._minmaxAggCache = undefined;
  }
  if (observedCount === 0) {
    scopeBlock.scopePaths.forEach((path: any) => path.setAttribute('d', ''));
    if (axes?.xTickLabels)
      axes.xTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
    if (axes?.yTickLabels)
      axes.yTickLabels.forEach((label: any) => label.setAttribute('display', 'none'));
    if (scopeBlock.scopeGridLines) {
      scopeBlock.scopeGridLines.xGridLines.forEach((l: any) => l.setAttribute('display', 'none'));
      scopeBlock.scopeGridLines.yGridLines.forEach((l: any) => l.setAttribute('display', 'none'));
    }
    if (scopeBlock.scopeLegendItems)
      scopeBlock.scopeLegendItems.forEach((el: any) => el.setAttribute('display', 'none'));
    if (scopeBlock.scopeAxisLabels) {
      scopeBlock.scopeAxisLabels.xLabel?.setAttribute('display', 'none');
      scopeBlock.scopeAxisLabels.yLabel?.setAttribute('display', 'none');
    }
    scopeBlock.computedLimits = {
      tMin: t0,
      tMax: t1,
      yMin: yMinParam != null ? yMinParam : -1.2,
      yMax: yMaxParam != null ? yMaxParam : 1.2,
    };
    refreshParamDisplay(scopeBlock);
    return;
  }

  let maxVal = Math.max(observedMax, 1);
  let minVal = Math.min(observedMin, -1);
  if (yMinParam != null) minVal = yMinParam;
  if (yMaxParam != null) maxVal = yMaxParam;
  if (maxVal === minVal) {
    maxVal += 1;
    minVal -= 1;
  }
  if (yMinParam == null && yMaxParam == null) {
    const maxAbs = Math.max(Math.abs(maxVal), Math.abs(minVal), 1e-6);
    maxVal = roundToSignificant(maxAbs * 1.2, 2);
    minVal = roundToSignificant(-maxAbs * 1.2, 2);
  }
  const range = maxVal - minVal;

  const yScale = scopeBlock.params?.yScale ?? 'linear';
  const isLogScale = yScale === 'log';
  // For log scale, floor negative/zero values
  const logMinVal = isLogScale ? Math.max(minVal, LOG_FLOOR) : minVal;
  const logMaxVal = isLogScale ? Math.max(maxVal, LOG_FLOOR) : maxVal;

  const valueToPixelY = (v: number) => {
    if (isLogScale) return logValueToPixelY(v, logMinVal, logMaxVal, plotY, plotH);
    return plotY + plotH - ((v - minVal) / range) * plotH;
  };

  // With multiYAxis each signal's Y range is computed independently.
  const multiYAxis = scopeBlock.params?.multiYAxis === true;
  /** Per-signal pixel Y mapping (defaults to primary mapping). */
  const signalToPixelY: Array<(v: number) => number> = [];
  if (multiYAxis) {
    (series as any[][]).forEach((vals: any[], sIdx: number) => {
      if (!isScopeSeriesConnected(connected, sIdx)) {
        signalToPixelY[sIdx] = valueToPixelY;
        return;
      }
      const sigParams = (scopeBlock.params?.signals as any[])?.[sIdx];
      const sYMinParam = parseLimit(sigParams?.yMin);
      const sYMaxParam = parseLimit(sigParams?.yMax);
      let sMin = Infinity;
      let sMax = -Infinity;
      vals.forEach((v) => {
        if (v == null || !Number.isFinite(v)) return;
        if (v < sMin) sMin = v;
        if (v > sMax) sMax = v;
      });
      if (!Number.isFinite(sMin)) {
        sMin = -1;
        sMax = 1;
      }
      if (sYMinParam != null) sMin = sYMinParam;
      if (sYMaxParam != null) sMax = sYMaxParam;
      if (sMax === sMin) {
        sMax += 1;
        sMin -= 1;
      }
      const sRange = sMax - sMin;
      signalToPixelY[sIdx] = (v: number) => plotY + plotH - ((v - sMin) / sRange) * plotH;
    });
  }

  const getSignalPixelY = (v: number, sIdx: number) =>
    multiYAxis && signalToPixelY[sIdx] ? signalToPixelY[sIdx](v) : valueToPixelY(v);

  if (axes) {
    // For log: x-axis always at bottom; for linear: clamp at y=0
    const xAxisY = isLogScale
      ? plotY + plotH
      : Math.max(plotY, Math.min(plotY + plotH, plotY + plotH - ((0 - minVal) / range) * plotH));
    axes.xAxis.setAttribute('x1', plotX);
    axes.xAxis.setAttribute('y1', xAxisY);
    axes.xAxis.setAttribute('x2', plotX + plotW);
    axes.xAxis.setAttribute('y2', xAxisY);
    axes.yAxis.setAttribute('x1', plotX);
    axes.yAxis.setAttribute('y1', plotY);
    axes.yAxis.setAttribute('x2', plotX);
    axes.yAxis.setAttribute('y2', plotY + plotH);
    const tickLen = 5;
    const yTickVals = isLogScale
      ? buildLogTicks(logMinVal, logMaxVal)
      : buildTicks(minVal, maxVal, niceStep(range, 5));
    axes.yTicks.forEach((tick: any, idx: number) => {
      if (idx >= yTickVals.length) {
        tick.setAttribute('display', 'none');
        axes.yTickLabels?.[idx]?.setAttribute('display', 'none');
        return;
      }
      tick.setAttribute('display', 'block');
      const v = yTickVals[idx];
      const y = valueToPixelY(v);
      tick.setAttribute('x1', plotX - tickLen);
      tick.setAttribute('y1', y);
      tick.setAttribute('x2', plotX + tickLen);
      tick.setAttribute('y2', y);
      const label = axes.yTickLabels?.[idx];
      if (label) {
        if (showTickLabels) {
          label.setAttribute('display', 'block');
          label.setAttribute('x', plotX + 8);
          label.setAttribute('y', y - 2);
          label.textContent = formatTick(v);
        } else {
          label.setAttribute('display', 'none');
        }
      }
    });
    const xStep = niceStep(Math.max(1e-6, tRange), 5);
    const xTickVals = tRange <= 0 ? [t0] : buildTicks(t0, t1, xStep);
    axes.xTicks.forEach((tick: any, idx: number) => {
      if (idx >= xTickVals.length) {
        tick.setAttribute('display', 'none');
        axes.xTickLabels?.[idx]?.setAttribute('display', 'none');
        return;
      }
      tick.setAttribute('display', 'block');
      const v = xTickVals[idx];
      const ratio = tRange <= 0 ? 0 : (v - t0) / tRange;
      const x = plotX + ratio * plotW;
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

    if (scopeBlock.scopeGridLines) {
      const { xGridLines, yGridLines } = scopeBlock.scopeGridLines;
      yTickVals.forEach((v: number, idx: number) => {
        const line = yGridLines[idx];
        if (!line) return;
        if (!showGrid) {
          line.setAttribute('display', 'none');
          return;
        }
        const y = valueToPixelY(v);
        line.setAttribute('display', 'block');
        line.setAttribute('x1', String(plotX));
        line.setAttribute('y1', String(y));
        line.setAttribute('x2', String(plotX + plotW));
        line.setAttribute('y2', String(y));
      });
      for (let i = yTickVals.length; i < yGridLines.length; i++) {
        yGridLines[i].setAttribute('display', 'none');
      }
      xTickVals.forEach((v: number, idx: number) => {
        const line = xGridLines[idx];
        if (!line) return;
        if (!showGrid) {
          line.setAttribute('display', 'none');
          return;
        }
        const ratio = tRange <= 0 ? 0 : (v - t0) / tRange;
        const x = plotX + ratio * plotW;
        line.setAttribute('display', 'block');
        line.setAttribute('x1', String(x));
        line.setAttribute('y1', String(plotY));
        line.setAttribute('x2', String(x));
        line.setAttribute('y2', String(plotY + plotH));
      });
      for (let i = xTickVals.length; i < xGridLines.length; i++) {
        xGridLines[i].setAttribute('display', 'none');
      }
    }
  }

  if (scopeBlock.scopeAxisLabels) {
    const xLabelText = scopeBlock.params?.xLabel || '';
    const yLabelText = scopeBlock.params?.yLabel || '';
    const xLabelEl = scopeBlock.scopeAxisLabels.xLabel;
    const yLabelEl = scopeBlock.scopeAxisLabels.yLabel;
    if (xLabelEl) {
      if (xLabelText) {
        xLabelEl.setAttribute('display', 'block');
        xLabelEl.setAttribute('x', String(plotX + plotW / 2));
        xLabelEl.setAttribute('y', String(plotY + plotH + 14));
        xLabelEl.textContent = xLabelText;
      } else {
        xLabelEl.setAttribute('display', 'none');
      }
    }
    if (yLabelEl) {
      if (yLabelText) {
        yLabelEl.setAttribute('display', 'block');
        yLabelEl.setAttribute('x', String(plotX - 6));
        yLabelEl.setAttribute('y', String(plotY + plotH / 2));
        yLabelEl.setAttribute('transform', `rotate(-90,${plotX - 6},${plotY + plotH / 2})`);
        yLabelEl.textContent = yLabelText;
      } else {
        yLabelEl.setAttribute('display', 'none');
      }
    }
  }

  const signalParams: any[] = Array.isArray(scopeBlock.params?.signals)
    ? scopeBlock.params.signals
    : [];

  // Display-only min/max reduction bounds each SVG path by the plot resolution. The
  // reduction is cached per series until the filtered data, plot width or time span
  // change. Exact data (limits, hover, export) is never reduced.
  const reduceCache: Map<number, { key: string; times: number[]; values: Array<number | null> }> =
    scopeBlock._minmaxReduceCache instanceof Map
      ? scopeBlock._minmaxReduceCache
      : (scopeBlock._minmaxReduceCache = new Map());
  const reduceBuckets = Math.max(16, Math.ceil(plotW));

  series.forEach((valuesForSeries: any[], seriesIdx: number) => {
    if (!isScopeSeriesConnected(connected, seriesIdx)) {
      const pathEl = scopeBlock.scopePaths[seriesIdx];
      if (pathEl) pathEl.setAttribute('d', '');
      return;
    }
    const cacheKey = `${time.length}:${valuesForSeries.length}:${t0Raw}:${t1Raw}:${reduceBuckets}`;
    let rTimes: number[];
    let rValues: Array<number | null>;
    const hit = reduceCache.get(seriesIdx);
    if (hit && hit.key === cacheKey) {
      rTimes = hit.times;
      rValues = hit.values;
    } else {
      const reduced = reduceMinMax(
        time as number[],
        valuesForSeries as Array<number | null>,
        reduceBuckets
      );
      rTimes = reduced.times;
      rValues = reduced.values;
      reduceCache.set(seriesIdx, { key: cacheKey, times: rTimes, values: rValues });
    }
    const path = rValues
      .map((v, i) => {
        if (v == null) return null;
        const t = Number(rTimes[i] ?? t0);
        const ratio =
          tRange <= 0 ? (rValues.length <= 1 ? 0 : i / (rValues.length - 1)) : (t - t0) / tRange;
        const clamped = Math.max(0, Math.min(1, ratio));
        const x = plotX + clamped * plotW;
        const y = getSignalPixelY(v, seriesIdx);
        return `${i === 0 ? 'M' : 'L'} ${x} ${y}`;
      })
      .filter(Boolean);
    const pathEl = scopeBlock.scopePaths[seriesIdx];
    if (pathEl) {
      pathEl.setAttribute('d', (path as any).join(' '));
      applySignalStyle(pathEl, signalParams[seriesIdx], `scope-path-${seriesIdx + 1}`);
    }
  });

  if (scopeBlock.scopeLegendItems) {
    const showLegend = scopeBlock.params?.showLegend !== false;
    const legendItems: any[] = scopeBlock.scopeLegendItems;
    const legendColors = ['#f5e642', '#ff49c9', '#39d7ff'];
    series.forEach((_: any, seriesIdx: number) => {
      const item = legendItems[seriesIdx];
      if (!item) return;
      const isConnected = isScopeSeriesConnected(connected, seriesIdx);
      if (!showLegend || !isConnected) {
        item.setAttribute('display', 'none');
        return;
      }
      const label =
        signalParams[seriesIdx]?.label || signalParams[seriesIdx]?.name || `y${seriesIdx + 1}`;
      const color = signalParams[seriesIdx]?.color || legendColors[seriesIdx] || legendColors[0];
      item.setAttribute('display', 'block');
      item.setAttribute('x', String(plotX + plotW - 4));
      item.setAttribute('y', String(plotY + 12 + seriesIdx * 13));
      item.setAttribute('fill', color);
      item.textContent = label;
    });
    for (let i = series.length; i < legendItems.length; i++) {
      legendItems[i].setAttribute('display', 'none');
    }
  }

  scopeBlock.computedLimits = {
    tMin: t0,
    tMax: t1,
    yMin: minVal,
    yMax: maxVal,
  };
  refreshParamDisplay(scopeBlock);

  if (scopeBlock.scopeHoverX == null) return;
  const clampedX = Math.min(plotX + plotW, Math.max(plotX, scopeBlock.scopeHoverX));
  const ratio = (clampedX - plotX) / Math.max(1, plotW);
  const primaryIndex = firstConnectedScopeSeries(connected, series.length);
  const primary = primaryIndex >= 0 ? series[primaryIndex] : series[0] || [];
  if (primary.length < 2) return;
  const idx = Math.min(primary.length - 1, Math.max(0, Math.round(ratio * (primary.length - 1))));
  const t = tRange <= 0 ? Number(time[idx] ?? t0) : t0 + ratio * tRange;
  const x = plotX + ratio * plotW;
  const timeLabel = formatTime(t, t0, t1);

  scopeBlock.scopeCursor?.remove();
  if (scopeBlock.scopeLabels) scopeBlock.scopeLabels.forEach((el: any) => el.remove());
  if (scopeBlock.scopeDots) scopeBlock.scopeDots.forEach((el: any) => el.remove());

  const cursor = createSvgElement('line', {
    x1: x,
    y1: plotY,
    x2: x,
    y2: plotY + plotH,
    class: 'scope-cursor',
  });

  scopeBlock.group.appendChild(cursor);

  scopeBlock.scopeCursor = cursor;
  scopeBlock.scopeLabels = [];
  scopeBlock.scopeDots = [];

  series.forEach((valuesForSeries: any[], seriesIdx: number) => {
    if (!isScopeSeriesConnected(connected, seriesIdx)) return;
    const v = valuesForSeries[idx] ?? 0;
    const y = getSignalPixelY(v, seriesIdx);
    const dot = createSvgElement('circle', {
      cx: x,
      cy: y,
      r: 3.5,
      class: `scope-dot scope-dot-${seriesIdx + 1}`,
    });
    const label = createSvgElement(
      'text',
      { x: x + 6, y: y - 6, class: `scope-label scope-label-${seriesIdx + 1}` },
      `t=${timeLabel.value.toFixed(2)} ${timeLabel.unit} y${seriesIdx + 1}=${v.toFixed(2)}`
    );
    // Click on crosshair label copies the value to the clipboard
    const copyText = `t=${timeLabel.value.toFixed(4)} ${timeLabel.unit}  y${
      seriesIdx + 1
    }=${v.toFixed(4)}`;
    label.addEventListener('click', (e: Event) => {
      e.stopPropagation();
      navigator.clipboard?.writeText(copyText).catch(() => {
        /* clipboard not available (e.g. non-secure context) - silently ignore */
      });
    });
    (label as unknown as HTMLElement).style?.setProperty('cursor', 'copy');
    scopeBlock.group.appendChild(dot);
    scopeBlock.group.appendChild(label);
    scopeBlock.scopeDots.push(dot);
    scopeBlock.scopeLabels.push(label);
  });
}
//=============================================================================
