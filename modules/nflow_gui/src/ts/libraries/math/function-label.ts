//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export function functionLabel(type: string, value: unknown): string {
  const fn = String(value ?? (type === 'trigFunction' ? 'sin' : 'exp'));
  const symbols: Record<string, string> = {
    square: 'u²',
    sqrt: '√u',
    reciprocal: '1/u',
    '10^u': '10ᵘ',
  };
  return symbols[fn] ?? fn;
}
//=============================================================================
