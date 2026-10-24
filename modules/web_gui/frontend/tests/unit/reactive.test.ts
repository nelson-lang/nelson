//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect } from 'vitest';
import { analyzeCell, reactiveTargets, computeStale } from '../../src/notebook/reactive';
//=============================================================================
describe('Notebook reactive dependency analysis', () => {
  it('separates assigned variables from read variables', () => {
    const d = analyzeCell('y = a * x + b;\nplot(x, y)');
    expect([...d.assigns].sort()).toEqual(['y']);
    expect(d.reads.has('a')).toBe(true);
    expect(d.reads.has('x')).toBe(true);
    expect(d.reads.has('b')).toBe(true);
  });

  it('handles multiple-assignment and indexed targets', () => {
    expect([...analyzeCell('[q, r] = deal(1, 2)').assigns].sort()).toEqual(['q', 'r']);
    expect(analyzeCell('M(3) = 7').assigns.has('M')).toBe(true);
    expect(analyzeCell('s.field = 9').assigns.has('s')).toBe(true);
  });

  it('does not mistake comparisons for assignments', () => {
    expect(analyzeCell('ok = a == b').assigns.has('a')).toBe(false);
    expect(analyzeCell('if x >= 2\n  y = 1;\nend').assigns.has('x')).toBe(false);
    expect(analyzeCell('ok = a == b').assigns.has('ok')).toBe(true);
  });

  it('ignores identifiers inside comments and strings', () => {
    const d = analyzeCell("z = 1; % secret should not appear\nmsg = 'hidden too'");
    expect(d.reads.has('secret')).toBe(false);
    expect(d.reads.has('hidden')).toBe(false);
    expect([...d.assigns].sort()).toEqual(['msg', 'z']);
  });

  it('does not treat language keywords as variables', () => {
    const d = analyzeCell('for k = 1:3\n  s = s + k;\nend');
    expect(d.reads.has('for')).toBe(false);
    expect(d.reads.has('end')).toBe(false);
  });

  it('re-runs exactly the cells downstream of a changed variable', () => {
    const blocks = [
      { id: 'c1', type: 'codecell', props: { code: 'y = amp * sin(x);' } },
      { id: 'p1', type: 'paragraph' },
      { id: 'c2', type: 'codecell', props: { code: 'z = y + 1;' } },
      { id: 'c3', type: 'codecell', props: { code: 'w = unrelated * 2;' } },
      { id: 'c4', type: 'codecell', props: { code: 'plot(z)' } },
    ];
    const t = reactiveTargets(blocks, 'amp');
    // c1 reads amp -> dirties y; c2 reads y -> dirties z; c4 reads z.
    expect([...t].sort()).toEqual(['c1', 'c2', 'c4']);
    // c3 is independent of amp.
    expect(t.has('c3')).toBe(false);
  });

  it('returns nothing for an unknown or empty variable', () => {
    const blocks = [{ id: 'c1', type: 'codecell', props: { code: 'y = x;' } }];
    expect(reactiveTargets(blocks, 'notThere').size).toBe(0);
    expect(reactiveTargets(blocks, '').size).toBe(0);
  });
});
//=============================================================================
describe('Notebook staleness (out-of-date cells)', () => {
  const blocks = [
    { id: 'c1', type: 'codecell', props: { code: 'a = 2;' } },
    { id: 'c2', type: 'codecell', props: { code: 'b = a + 1;' } },
    { id: 'c3', type: 'codecell', props: { code: 'c = 10;' } },
  ];

  it('marks every cell stale when none has run yet', () => {
    expect([...computeStale(blocks, {}).values()].sort()).toEqual(['c1', 'c2', 'c3']);
  });

  it('marks nothing stale once all cells ran their current source', () => {
    const lastRun = { c1: 'a = 2;', c2: 'b = a + 1;', c3: 'c = 10;' };
    expect(computeStale(blocks, lastRun).size).toBe(0);
  });

  it('edits to one cell make it and its dependents stale, not independent cells', () => {
    // c1's source differs from what last ran -> c1 stale; c2 reads a -> stale;
    // c3 is independent -> not stale.
    const lastRun = { c1: 'a = 1;', c2: 'b = a + 1;', c3: 'c = 10;' };
    const stale = computeStale(blocks, lastRun);
    expect(stale.has('c1')).toBe(true);
    expect(stale.has('c2')).toBe(true);
    expect(stale.has('c3')).toBe(false);
  });

  it('measures the recompute reduction vs a full re-run', () => {
    // A 3-cell document where only the independent cell changed: 1 of 3 re-runs.
    const lastRun = { c1: 'a = 2;', c2: 'b = a + 1;', c3: 'c = 99;' };
    const stale = computeStale(blocks, lastRun);
    expect([...stale]).toEqual(['c3']);
    expect(stale.size).toBe(1); // vs 3 for Run all — a 2/3 reduction
  });
});
//=============================================================================
