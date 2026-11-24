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
import { insertPromptText } from '../../src/console/promptBuffer';
//=============================================================================
describe('command prompt text insertion', () => {
  it('preserves multiline clipboard content and normalizes native newlines', () => {
    expect(insertPromptText('', 'a = 1\r\nb = 2\rc = 3')).toEqual({
      value: 'a = 1\nb = 2\nc = 3',
      caret: 17,
    });
  });

  it('replaces the selected range without flattening lines', () => {
    expect(insertPromptText('before after', 'one\ntwo', 7, 12)).toEqual({
      value: 'before one\ntwo',
      caret: 14,
    });
  });
});
//=============================================================================
