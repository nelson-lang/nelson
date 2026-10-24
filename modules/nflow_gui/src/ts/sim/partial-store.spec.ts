//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Incremental partial-result store: append, ordering, duplicate,
// gap, v1 compatibility, cleanup between runs (a fresh store per run).
//=============================================================================
import { describe, expect, test } from 'vitest';
import {
  applyDisplayDelta,
  applyPartial,
  appendScopeDelta,
  appendXYScopeDelta,
  appendXYZScopeDelta,
  createPartialStore,
  type ScopeAccum,
} from './partial-store.js';
//=============================================================================
function frame(sequence: number, extra: Record<string, unknown> = {}) {
  return { version: 2, sequence, status: 'running', ...extra };
}
//=============================================================================
describe('partial-store v2 deltas', () => {
  test('simple append accumulates times and series', () => {
    const store = createPartialStore();
    applyPartial(
      store,
      frame(0, {
        progress: 0.1,
        currentTime: 0.2,
        scopes: [{ id: 's1', type: 'scope', timesDelta: [0, 0.1], seriesDelta: [[1, 2]] }],
      })
    );
    applyPartial(
      store,
      frame(1, {
        progress: 0.5,
        currentTime: 0.4,
        scopes: [{ id: 's1', type: 'scope', timesDelta: [0.2, 0.3], seriesDelta: [[3, 4]] }],
      })
    );
    const entry = store.scopes.get('s1')!;
    expect(entry.times).toEqual([0, 0.1, 0.2, 0.3]);
    expect(entry.series[0]).toEqual([1, 2, 3, 4]);
    expect(store.progress).toBe(0.5);
    expect(store.currentTime).toBe(0.4);
    expect(store.sequenceGap).toBe(false);
  });

  test('multi-series append keeps series parallel', () => {
    const store = createPartialStore();
    applyPartial(
      store,
      frame(0, {
        scopes: [{ id: 's1', type: 'scope', timesDelta: [0], seriesDelta: [[1], [10]] }],
      })
    );
    applyPartial(
      store,
      frame(1, {
        scopes: [{ id: 's1', type: 'scope', timesDelta: [0.1], seriesDelta: [[2], [20]] }],
      })
    );
    const entry = store.scopes.get('s1')!;
    expect(entry.series).toEqual([
      [1, 2],
      [10, 20],
    ]);
  });

  test('duplicate frame is ignored', () => {
    const store = createPartialStore();
    const f = frame(0, {
      scopes: [{ id: 's1', type: 'scope', timesDelta: [0], seriesDelta: [[1]] }],
    });
    expect(applyPartial(store, f)).toBe(true);
    expect(applyPartial(store, f)).toBe(false); // same sequence: dropped
    expect(store.scopes.get('s1')!.times).toEqual([0]);
  });

  test('stale (older) frame is ignored', () => {
    const store = createPartialStore();
    applyPartial(store, frame(5, { scopes: [] }));
    expect(applyPartial(store, frame(3, { scopes: [] }))).toBe(false);
    expect(store.lastSequence).toBe(5);
  });

  test('sequence gap marks the preview incomplete but keeps appending', () => {
    const store = createPartialStore();
    applyPartial(
      store,
      frame(0, { scopes: [{ id: 's1', type: 'scope', timesDelta: [0], seriesDelta: [[1]] }] })
    );
    applyPartial(
      store,
      frame(4, { scopes: [{ id: 's1', type: 'scope', timesDelta: [0.4], seriesDelta: [[5]] }] })
    );
    expect(store.sequenceGap).toBe(true);
    expect(store.scopes.get('s1')!.times).toEqual([0, 0.4]);
  });

  test('xy and xyz deltas accumulate parallel arrays', () => {
    const store = createPartialStore();
    applyPartial(
      store,
      frame(0, {
        scopes: [
          { id: 'xy', type: 'xyScope', xDelta: [1], yDelta: [2] },
          { id: 'xyz', type: 'xyzScope', xDelta: [1], yDelta: [2], zDelta: [3] },
        ],
      })
    );
    applyPartial(
      store,
      frame(1, {
        scopes: [
          { id: 'xy', type: 'xyScope', xDelta: [10], yDelta: [20] },
          { id: 'xyz', type: 'xyzScope', xDelta: [10], yDelta: [20], zDelta: [30] },
        ],
      })
    );
    expect(store.scopes.get('xy')!.x).toEqual([1, 10]);
    expect(store.scopes.get('xy')!.y).toEqual([2, 20]);
    expect(store.scopes.get('xyz')!.z).toEqual([3, 30]);
  });

  test('display keeps only the latest value', () => {
    const store = createPartialStore();
    applyPartial(store, frame(0, { scopes: [{ id: 'd', type: 'display', value: 1 }] }));
    applyPartial(store, frame(1, { scopes: [{ id: 'd', type: 'display', value: 7 }] }));
    expect(store.scopes.get('d')!.value).toBe(7);
  });

  test('empty delta updates only progress and currentTime', () => {
    const store = createPartialStore();
    applyPartial(
      store,
      frame(0, { scopes: [{ id: 's1', type: 'scope', timesDelta: [0], seriesDelta: [[1]] }] })
    );
    applyPartial(store, frame(1, { progress: 0.9, currentTime: 9, scopes: [] }));
    expect(store.progress).toBe(0.9);
    expect(store.currentTime).toBe(9);
    expect(store.scopes.get('s1')!.times).toEqual([0]);
  });
});
//=============================================================================
describe('partial-store v1 compatibility', () => {
  test('cumulative v1 payload replaces scope data', () => {
    const store = createPartialStore();
    applyPartial(store, {
      progress: 0.3,
      currentTime: 3,
      scopes: [{ id: 's1', type: 'scope', times: [0, 1], series: [[1, 2]] }],
    });
    applyPartial(store, {
      progress: 0.6,
      currentTime: 6,
      scopes: [{ id: 's1', type: 'scope', times: [0, 1, 2], series: [[1, 2, 3]] }],
    });
    const entry = store.scopes.get('s1')!;
    expect(entry.times).toEqual([0, 1, 2]);
    expect(entry.series[0]).toEqual([1, 2, 3]);
    expect(store.progress).toBe(0.6);
  });

  test('null / non-object payloads are rejected', () => {
    const store = createPartialStore();
    expect(applyPartial(store, null)).toBe(false);
    expect(applyPartial(store, 'x')).toBe(false);
    expect(store.framesApplied).toBe(0);
  });
});
//=============================================================================
describe('pure append helpers', () => {
  const empty = (): ScopeAccum => ({
    type: 'scope',
    times: [],
    series: [],
    x: [],
    y: [],
    z: [],
  });

  test('appendScopeDelta grows series lanes on demand', () => {
    const entry = empty();
    appendScopeDelta(entry, [0.5], [[1], [2], [3]]);
    expect(entry.series.length).toBe(3);
    expect(entry.series[2]).toEqual([3]);
  });

  test('appendXYScopeDelta / appendXYZScopeDelta tolerate missing arrays', () => {
    const entry = empty();
    appendXYScopeDelta(entry, undefined, [1]);
    appendXYZScopeDelta(entry, [2], undefined, [3]);
    expect(entry.x).toEqual([2]);
    expect(entry.y).toEqual([1]);
    expect(entry.z).toEqual([3]);
  });

  test('applyDisplayDelta ignores undefined', () => {
    const entry = empty();
    applyDisplayDelta(entry, 4);
    applyDisplayDelta(entry, undefined);
    expect(entry.value).toBe(4);
  });
});
//=============================================================================
