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
import { filterHistory } from '../../src/console/history';
//=============================================================================
describe('filterHistory (prefix-aware history search)', () => {
  const history = ['plot(x)', 'surf(peaks)', 'plot(y)', 'clear', 'plotmatrix(A)'];

  it('returns all indices for an empty prefix', () => {
    expect(filterHistory(history, '')).toEqual([0, 1, 2, 3, 4]);
  });

  it('returns only entries starting with the prefix, in order', () => {
    expect(filterHistory(history, 'plot')).toEqual([0, 2, 4]);
  });

  it('is case-sensitive and exact on the prefix', () => {
    expect(filterHistory(history, 'surf')).toEqual([1]);
    expect(filterHistory(history, 'Plot')).toEqual([]);
  });

  it('returns nothing when no entry matches', () => {
    expect(filterHistory(history, 'zzz')).toEqual([]);
  });
});
//=============================================================================
