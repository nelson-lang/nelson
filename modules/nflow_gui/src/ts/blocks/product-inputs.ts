//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export function parseProductInputs(
  value: unknown,
  allowSingle = false
): { count: number; operations: string } | null {
  const raw = String(value ?? '').trim();
  if (/^\d+$/.test(raw)) {
    const count = Number(raw);
    if (Number.isSafeInteger(count) && count >= (allowSingle ? 1 : 2))
      return { count, operations: '*'.repeat(count) };
  }
  if ((allowSingle ? /^[*/]+$/ : /^[*/]{2,}$/).test(raw))
    return { count: raw.length, operations: raw };
  return null;
}
//=============================================================================
