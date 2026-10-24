//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Helpers to process partial and final simulation results and draw scopes
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { renderScope } from './render-scope.js';
import { writeStatus } from '../app/status-service.js';
import { t } from '../utils/i18n.js';
import { renderXYScope } from './render-xy-scope.js';
import { renderXYZScope } from './render-xyz-scope.js';
import { renderDisplay } from './render-display.js';
import { emitAppEvent } from '../app/app-events.js';
//=============================================================================
export type ScopeBlock = { id: string; [key: string]: any };
export type ScopeData =
  | {
      id: string;
      type: 'scope';
      times?: number[];
      series?: number[][];
      connected?: boolean | boolean[];
    }
  | { id: string; type: 'xyScope'; x?: number[]; y?: number[]; connected?: boolean | boolean[] }
  | {
      id: string;
      type: 'xyzScope';
      x?: number[];
      y?: number[];
      z?: number[];
      connected?: boolean | boolean[];
    }
  | { id: string; type: 'display'; value?: number | number[] };
export type DashboardUpdate = {
  id: string;
  time?: number;
  type?: string;
  value?: number | string | Array<number | string>;
  valueImag?: number | string | Array<number | string>;
  series?: Array<Array<number | string>>;
  seriesImag?: number[][];
  times?: number[];
  connected?: boolean;
  valueType?: string;
  valid?: boolean;
  reason?: string;
};
//=============================================================================
export type PartialResult = {
  /** Protocol version: absent/undefined = legacy v1 cumulative, 2 = delta. */
  version?: number;
  /** v2: strictly increasing per run (duplicate/stale/gap detection). */
  sequence?: number;
  /** v2: sample window [fromSample, toSample) carried by this frame. */
  fromSample?: number;
  toSample?: number;
  progress?: number;
  currentTime?: number;
  scopes?: ScopeData[];
  dashboardUpdates?: DashboardUpdate[];
};
//=============================================================================
/** Bus member layout as reported by the engine (recursive for nested buses). */
export type BusMemberInfo = {
  name: string;
  offset: number;
  width: number;
  type?: string;
  complex?: boolean;
  dims?: number[];
  bus?: BusInfo;
};
//=============================================================================
export type BusInfo = { name: string; nonvirtual: boolean; members: BusMemberInfo[] };
/** Per-port final value: scalar wire = number, vector/matrix wire = number[]. */
export type SignalValues = Record<string, Array<number | number[]>>;
/** Per-port dims [rows, cols], parallel to `signals`. */
export type SignalDims = Record<string, Array<[number, number]>>;
export type FinalResult = {
  scopes?: ScopeData[];
  status?: string;
  reason?: string;
  session?: unknown;
  /** Final value of each output port, keyed by block id (wire badges). */
  signals?: SignalValues;
  /** Dims of each output port ([rows, cols]), keyed by block id. */
  signalDims?: SignalDims;
  /** Signal type name of each output port ("double", "int8", ...), keyed by block id. */
  signalTypes?: Record<string, string[]>;
  /** Imaginary parts, parallel to `signals` (present iff the diagram carries
   *  complex signals; 0 entries for real ports). */
  signalsImag?: SignalValues;
  /** Per-port complexity flag, parallel to `signalTypes`. */
  signalComplex?: Record<string, boolean[]>;
  /** Per-port bus descriptor (null for non-bus ports), present iff the
   *  diagram carries bus signals. Member offsets are relative to the port's
   *  packed value in `signals`. */
  signalBus?: Record<string, Array<BusInfo | null>>;
  errorBlocks?: string[];
  dashboardUpdates?: DashboardUpdate[];
};
//=============================================================================
export function applyDashboardUpdates(updates?: DashboardUpdate[]): void {
  for (const update of updates || []) {
    emitAppEvent('nf-dashboard-update', update);
  }
}
//=============================================================================
export function drawDisplay(scopeBlock: ScopeBlock, value?: number | number[]) {
  (scopeBlock as ScopeBlock).displayData = { value };
  renderDisplay(scopeBlock);
}
//=============================================================================
export function drawScope(
  scopeBlock: ScopeBlock,
  time?: number[],
  series?: number[][],
  connected?: boolean | boolean[]
) {
  (scopeBlock as ScopeBlock).scopeData = { time, series, connected };
  renderScope(scopeBlock);
}
//=============================================================================
export function drawXYScope(
  scopeBlock: ScopeBlock,
  series?: { x?: number[]; y?: number[] },
  connected?: boolean | boolean[]
) {
  (scopeBlock as ScopeBlock).xyScopeData = { series, connected };
  renderXYScope(scopeBlock);
}
//=============================================================================
export function drawXYZScope(
  scopeBlock: ScopeBlock,
  series?: { x?: number[]; y?: number[]; z?: number[] },
  connected?: boolean | boolean[]
) {
  (scopeBlock as ScopeBlock).xyzScopeData = { series, connected };
  renderXYZScope(scopeBlock);
}
//=============================================================================
export function applyScopeUpdates(
  scopes: ScopeData[] | undefined,
  scopeBlockMap: Map<string, ScopeBlock>
) {
  for (const scopeData of scopes || []) {
    const scopeBlock = scopeBlockMap.get(scopeData.id);
    if (!scopeBlock) continue;
    if (scopeData.type === 'scope') {
      drawScope(scopeBlock, scopeData.times, scopeData.series, scopeData.connected);
    } else if (scopeData.type === 'xyScope') {
      drawXYScope(scopeBlock, { x: scopeData.x, y: scopeData.y }, scopeData.connected);
    } else if (scopeData.type === 'xyzScope') {
      drawXYZScope(
        scopeBlock,
        { x: scopeData.x, y: scopeData.y, z: scopeData.z },
        scopeData.connected
      );
    } else if (scopeData.type === 'display') {
      drawDisplay(scopeBlock, scopeData.value);
    }
  }
}
//=============================================================================
export function processPartialScopes(
  partialResult: PartialResult,
  scopeBlockMap: Map<string, ScopeBlock>,
  statusEl?: HTMLElement | null,
  onStatusUpdate?: ((msg: string, time: number) => void) | null
) {
  if (!partialResult) return;
  const pct = Math.round(((partialResult.progress ?? 0) as number) * 100);
  const curTime = partialResult.currentTime ?? 0;
  writeStatus(statusEl, t('Running... {pct}%', { pct }));
  if (onStatusUpdate) onStatusUpdate(t('Running... {pct}%', { pct }), curTime);
  emitAppEvent('nf-sim-time', { time: curTime, progress: pct, done: false });
  applyScopeUpdates(partialResult.scopes, scopeBlockMap);
  applyDashboardUpdates(partialResult.dashboardUpdates);
}
/**
 * Draws the preview accumulated in the partial store. Deltas accumulate on
 * arrival; only this draw is coalesced through requestAnimationFrame.
 */
export function renderPartialStore(
  store: import('./partial-store.js').PartialStore,
  scopeBlockMap: Map<string, ScopeBlock>,
  statusEl?: HTMLElement | null,
  onStatusUpdate?: ((msg: string, time: number) => void) | null
) {
  const pct = Math.round((store.progress ?? 0) * 100);
  const curTime = store.currentTime ?? 0;
  writeStatus(statusEl, t('Running... {pct}%', { pct }));
  if (onStatusUpdate) onStatusUpdate(t('Running... {pct}%', { pct }), curTime);
  emitAppEvent('nf-sim-time', { time: curTime, progress: pct, done: false });
  for (const [id, entry] of store.scopes) {
    const scopeBlock = scopeBlockMap.get(id);
    if (!scopeBlock) continue;
    if (entry.type === 'scope') {
      drawScope(scopeBlock, entry.times, entry.series as number[][], entry.connected);
    } else if (entry.type === 'xyScope') {
      drawXYScope(scopeBlock, { x: entry.x, y: entry.y }, entry.connected);
    } else if (entry.type === 'xyzScope') {
      drawXYZScope(scopeBlock, { x: entry.x, y: entry.y, z: entry.z }, entry.connected);
    } else if (entry.type === 'display') {
      drawDisplay(scopeBlock, entry.value);
    }
  }
}
//=============================================================================
export function processFinalScopes(
  result: FinalResult,
  scopeBlockMap: Map<string, ScopeBlock>,
  statusEl?: HTMLElement | null,
  onStatusUpdate?: ((msg: string, time: number) => void) | null
) {
  void statusEl;
  void onStatusUpdate;
  applyScopeUpdates(result.scopes, scopeBlockMap);
  applyDashboardUpdates(result.dashboardUpdates);
}
//=============================================================================
