//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import { NELSON_KEYWORDS, tokenizeNelson } from '../../src/editor/nelsonSyntax';
//=============================================================================
describe('Nelson command syntax highlighting', () => {
  it('shares the editor keyword set and classifies prompt tokens', () => {
    expect(NELSON_KEYWORDS).toContain('function');
    expect(tokenizeNelson('for k = 1:3\nend % loop')).toEqual([
      { kind: 'keyword', text: 'for' },
      { kind: 'plain', text: ' k ' },
      { kind: 'operator', text: '=' },
      { kind: 'plain', text: ' ' },
      { kind: 'number', text: '1' },
      { kind: 'operator', text: ':' },
      { kind: 'number', text: '3' },
      { kind: 'plain', text: '\n' },
      { kind: 'keyword', text: 'end' },
      { kind: 'plain', text: ' ' },
      { kind: 'comment', text: '% loop' },
    ]);
  });

  it('keeps comment markers inside strings as string content', () => {
    expect(tokenizeNelson('label = "100% // done" // comment')).toEqual([
      { kind: 'plain', text: 'label ' },
      { kind: 'operator', text: '=' },
      { kind: 'plain', text: ' ' },
      { kind: 'string', text: '"100% // done"' },
      { kind: 'plain', text: ' ' },
      { kind: 'comment', text: '// comment' },
    ]);
  });
});
//=============================================================================
