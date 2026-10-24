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
import type { Block } from '../types.js';
import { onAppEvent } from '../../app/app-events.js';
//=============================================================================
export type DashboardUpdateSink = (value: any, series?: number[][], times?: number[]) => void;
// Feeds a rendered dashboard block from the simulation's `nf-dashboard-update`
// events. The listener lives as long as this rendering of the block: it
// removes itself once the group has left the canvas.
export function subscribeDashboardUpdates(block: Block, update: DashboardUpdateSink): void {
  // Progress frames delta-encode the dashboardScope stream ("delta": true carries
  // only the new samples), so accumulate them here. A full payload replaces the
  // accumulator. Trim to TimeSpan so a long real-time run does not grow memory.
  let accSeries: number[][] = [];
  let accTimes: number[] = [];
  const group = block.group;
  const off = onAppEvent('nf-dashboard-update', (payload) => {
    if (!group.isConnected) {
      off();
      return;
    }
    const detail = payload as any;
    if (!detail || String(detail.id) !== String(block.id ?? '')) return;
    const value = Array.isArray(detail.value) ? detail.value : Number(detail.value);
    let series = detail.series as number[][] | undefined;
    let times = detail.times as number[] | undefined;
    if (Array.isArray(series) || Array.isArray(times)) {
      if (detail.delta === true) {
        for (let s = 0; s < (series?.length ?? 0); s += 1) {
          if (!accSeries[s]) accSeries[s] = [];
          if (Array.isArray(series![s]) && series![s].length) accSeries[s].push(...series![s]);
        }
        if (Array.isArray(times) && times.length) accTimes.push(...times);
        const configuredSpan = Number(block.params?.TimeSpan);
        if (Number.isFinite(configuredSpan) && configuredSpan > 0 && accTimes.length) {
          const cutoff = accTimes[accTimes.length - 1] - configuredSpan;
          const first = accTimes.findIndex((t) => t >= cutoff);
          if (first > 0) {
            accTimes = accTimes.slice(first);
            accSeries = accSeries.map((s) => s.slice(first));
          }
        }
      } else {
        accSeries = Array.isArray(series) ? series.map((s) => s.slice()) : [];
        accTimes = Array.isArray(times) ? times.slice() : [];
      }
      series = accSeries;
      times = accTimes;
    }
    if (Array.isArray(value) || Number.isFinite(value)) update(value, series, times);
    block.group.classList.toggle('dashboard-unbound', detail.connected === false);
    block.group.classList.toggle('dashboard-invalid', detail.valid === false);
  });
}
//=============================================================================
