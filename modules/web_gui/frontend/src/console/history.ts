//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Prefix-aware history search helper (used by the Command Window). Given the
// command history (chronological) and the text already typed, return the
// indices of entries that start with that prefix; the set Up/Down cycle. Kept
// pure so it can be unit-tested without a terminal.
export function filterHistory(history: readonly string[], prefix: string): number[] {
  const out: number[] = [];
  for (let i = 0; i < history.length; i++) {
    if (history[i].startsWith(prefix)) out.push(i);
  }
  return out;
}
//=============================================================================
