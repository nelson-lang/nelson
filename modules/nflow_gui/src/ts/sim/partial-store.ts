//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Incremental partial-result store.
//
// The engine publishes protocol-v2 frames carrying only the samples recorded
// since the previous frame. Deltas are accumulated on arrival; only the render
// is coalesced (requestAnimationFrame in sim.ts), since coalescing the data
// would lose samples. The legacy cumulative v1 payload is still accepted
// (replace semantics).
//
// Error policy:
//   - duplicate or stale frame (sequence <= last seen): ignored;
//   - sequence gap (a frame was dropped): the preview is marked incomplete and
//     the delta is still appended;
//   - the final result replaces the preview unconditionally (sim.ts).
//=============================================================================
import type { DashboardUpdate } from './parse-results.js';
//=============================================================================
export type ScopeDelta = {
  id: string;
  type: 'scope' | 'xyScope' | 'xyzScope' | 'display';
  timesDelta?: number[];
  seriesDelta?: Array<Array<number | null>>;
  xDelta?: number[];
  yDelta?: number[];
  zDelta?: number[];
  connected?: boolean | boolean[];
  value?: number | number[];
};
//=============================================================================
export type PartialV2 = {
  version: number;
  sequence: number;
  status?: string;
  progress?: number;
  currentTime?: number;
  fromSample?: number;
  toSample?: number;
  scopes?: ScopeDelta[];
  dashboardUpdates?: DashboardUpdate[];
};
//=============================================================================
export type ScopeAccum = {
  type: 'scope' | 'xyScope' | 'xyzScope' | 'display';
  times: number[];
  series: Array<Array<number | null>>;
  x: number[];
  y: number[];
  z: number[];
  value?: number | number[];
  connected?: boolean | boolean[];
};
//=============================================================================
export type PartialStore = {
  scopes: Map<string, ScopeAccum>;
  progress: number;
  currentTime: number;
  /** Sequence of the last frame applied (-1 before the first one). */
  lastSequence: number;
  /** True once a sequence gap was observed: the preview is incomplete. */
  sequenceGap: boolean;
  /** For tests/diagnostics. */
  framesApplied: number;
};
//=============================================================================
export function createPartialStore(): PartialStore {
  return {
    scopes: new Map(),
    progress: 0,
    currentTime: 0,
    lastSequence: -1,
    sequenceGap: false,
    framesApplied: 0,
  };
}
//=============================================================================
function accumOf(store: PartialStore, id: string, type: ScopeAccum['type']): ScopeAccum {
  let entry = store.scopes.get(id);
  if (!entry || entry.type !== type) {
    entry = { type, times: [], series: [], x: [], y: [], z: [] };
    store.scopes.set(id, entry);
  }
  return entry;
}
//=============================================================================
export function appendScopeDelta(
  entry: ScopeAccum,
  timesDelta?: number[],
  seriesDelta?: Array<Array<number | null>>
): void {
  if (Array.isArray(timesDelta) && timesDelta.length > 0) {
    entry.times.push(...timesDelta);
  }
  if (Array.isArray(seriesDelta)) {
    for (let s = 0; s < seriesDelta.length; s += 1) {
      if (!entry.series[s]) entry.series[s] = [];
      const d = seriesDelta[s];
      if (Array.isArray(d) && d.length > 0) entry.series[s].push(...d);
    }
  }
}
//=============================================================================
export function appendXYScopeDelta(entry: ScopeAccum, xDelta?: number[], yDelta?: number[]): void {
  if (Array.isArray(xDelta) && xDelta.length > 0) entry.x.push(...xDelta);
  if (Array.isArray(yDelta) && yDelta.length > 0) entry.y.push(...yDelta);
}
//=============================================================================
export function appendXYZScopeDelta(
  entry: ScopeAccum,
  xDelta?: number[],
  yDelta?: number[],
  zDelta?: number[]
): void {
  appendXYScopeDelta(entry, xDelta, yDelta);
  if (Array.isArray(zDelta) && zDelta.length > 0) entry.z.push(...zDelta);
}
//=============================================================================
/** A display carries only its latest value. */
export function applyDisplayDelta(entry: ScopeAccum, value?: number | number[]): void {
  if (value !== undefined) entry.value = value;
}
//=============================================================================
function isV2(payload: unknown): payload is PartialV2 {
  return (
    !!payload &&
    typeof payload === 'object' &&
    (payload as PartialV2).version === 2 &&
    typeof (payload as PartialV2).sequence === 'number'
  );
}
/** Returns true when the store changed (a render should be scheduled). */
export function applyPartial(store: PartialStore, payload: unknown): boolean {
  if (!payload || typeof payload !== 'object') return false;

  if (isV2(payload)) {
    // Transport re-delivery.
    if (payload.sequence <= store.lastSequence) return false;
    // A frame was lost: keep appending, the final result repairs the data.
    if (store.lastSequence >= 0 && payload.sequence !== store.lastSequence + 1) {
      store.sequenceGap = true;
    }
    store.lastSequence = payload.sequence;
    if (typeof payload.progress === 'number') store.progress = payload.progress;
    if (typeof payload.currentTime === 'number') store.currentTime = payload.currentTime;
    for (const sc of payload.scopes || []) {
      if (!sc || typeof sc.id !== 'string') continue;
      const entry = accumOf(store, sc.id, sc.type);
      if (sc.connected !== undefined) entry.connected = sc.connected;
      if (sc.type === 'scope') {
        appendScopeDelta(entry, sc.timesDelta, sc.seriesDelta);
      } else if (sc.type === 'xyScope') {
        appendXYScopeDelta(entry, sc.xDelta, sc.yDelta);
      } else if (sc.type === 'xyzScope') {
        appendXYZScopeDelta(entry, sc.xDelta, sc.yDelta, sc.zDelta);
      } else if (sc.type === 'display') {
        applyDisplayDelta(entry, sc.value);
      }
    }
    store.framesApplied += 1;
    return true;
  }

  // Legacy v1 payload: replaces everything it carries.
  const v1 = payload as {
    progress?: number;
    currentTime?: number;
    scopes?: Array<{
      id: string;
      type: ScopeAccum['type'];
      times?: number[];
      series?: Array<Array<number | null>>;
      x?: number[];
      y?: number[];
      z?: number[];
      connected?: boolean | boolean[];
      value?: number | number[];
    }>;
  };
  if (typeof v1.progress === 'number') store.progress = v1.progress;
  if (typeof v1.currentTime === 'number') store.currentTime = v1.currentTime;
  for (const sc of v1.scopes || []) {
    if (!sc || typeof sc.id !== 'string') continue;
    const entry = accumOf(store, sc.id, sc.type);
    if (sc.connected !== undefined) entry.connected = sc.connected;
    if (sc.type === 'scope') {
      entry.times = sc.times ? sc.times.slice() : entry.times;
      entry.series = sc.series ? sc.series.map((s) => s.slice()) : entry.series;
    } else if (sc.type === 'xyScope' || sc.type === 'xyzScope') {
      entry.x = sc.x ? sc.x.slice() : entry.x;
      entry.y = sc.y ? sc.y.slice() : entry.y;
      entry.z = sc.z ? sc.z.slice() : entry.z;
    } else if (sc.type === 'display') {
      applyDisplayDelta(entry, sc.value);
    }
  }
  store.framesApplied += 1;
  return true;
}
//=============================================================================
