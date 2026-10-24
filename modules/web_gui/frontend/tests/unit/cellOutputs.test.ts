//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// @vitest-environment node
import { describe, it, expect } from 'vitest';
import {
  clearedOutputProps,
  isCellRunning,
  hasCellResult,
  errorCellIds,
} from '../../src/notebook/cellOutputs';
//=============================================================================
describe('Notebook cell output helpers', () => {
  it('clearedOutputProps empties text, figures, handles and error', () => {
    expect(clearedOutputProps()).toEqual({
      output: '',
      figures: '[]',
      figureHandles: '[]',
      errorMsg: '',
    });
    // figures must stay a parseable empty JSON array
    expect(JSON.parse(clearedOutputProps().figures)).toEqual([]);
  });

  it('isCellRunning matches only the active cell id', () => {
    expect(isCellRunning('c2', 'c2')).toBe(true);
    expect(isCellRunning('c1', 'c2')).toBe(false);
    expect(isCellRunning(null, 'c2')).toBe(false);
    expect(isCellRunning(undefined, 'c2')).toBe(false);
    expect(isCellRunning('', 'c2')).toBe(false);
  });

  it('hasCellResult is true when there is text, an error, or a figure', () => {
    expect(hasCellResult({})).toBe(false);
    expect(hasCellResult({ output: '', figures: '[]', errorMsg: '' })).toBe(false);
    expect(hasCellResult({ output: 'ans = 4' })).toBe(true);
    expect(hasCellResult({ errorMsg: 'Undefined variable' })).toBe(true);
    expect(hasCellResult({ figures: '["data:image/png;base64,QUJD"]' })).toBe(true);
    // malformed figures JSON must not throw and counts as no result
    expect(hasCellResult({ figures: 'not json' })).toBe(false);
  });

  it('errorCellIds lists code cells with a non-empty error, in order', () => {
    const blocks = [
      { id: 'c1', type: 'codecell', props: { errorMsg: '' } },
      { id: 'h1', type: 'heading', props: {} },
      { id: 'c2', type: 'codecell', props: { errorMsg: 'Undefined variable x' } },
      { id: 'c3', type: 'codecell', props: {} },
      { id: 'c4', type: 'codecell', props: { errorMsg: '   ' } }, // whitespace = no error
      { id: 'c5', type: 'codecell', props: { errorMsg: 'parse error' } },
    ];
    expect(errorCellIds(blocks)).toEqual(['c2', 'c5']);
  });
});
//=============================================================================
