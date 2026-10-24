//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type PromptInsertion = {
  value: string;
  caret: number;
};
//=============================================================================
export function insertPromptText(
  value: string,
  text: string,
  selectionStart = value.length,
  selectionEnd = selectionStart
): PromptInsertion {
  const start = Math.max(0, Math.min(selectionStart, value.length));
  const end = Math.max(start, Math.min(selectionEnd, value.length));
  const normalized = text.replace(/\r\n?/g, '\n');
  return {
    value: value.slice(0, start) + normalized + value.slice(end),
    caret: start + normalized.length,
  };
}
//=============================================================================
