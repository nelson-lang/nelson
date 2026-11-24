//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { CompletionItem, CompletionPopupState } from './CompletionPopup';
import { textareaCaretPosition } from './textareaCaret';
//=============================================================================
export type CompletionResponse = {
  prefix: string;
  builtin?: string[];
  macros?: string[];
  variables?: string[];
  files?: string[];
};
//=============================================================================
export function completionItems(response: CompletionResponse): CompletionItem[] {
  return [
    ...(response.variables ?? []).map((text) => ({ text, kind: 'var' })),
    ...(response.builtin ?? []).map((text) => ({ text, kind: 'fn' })),
    ...(response.macros ?? []).map((text) => ({ text, kind: 'macro' })),
    ...(response.files ?? []).map((text) => ({ text, kind: 'file' })),
  ];
}
//=============================================================================
export function completionAnchor(
  input: HTMLTextAreaElement | null,
  caret: number,
  prefix: string
): CompletionPopupState['anchor'] {
  const bounds = input?.getBoundingClientRect();
  const prefixStart = Math.max(0, caret - prefix.length);
  const position = input ? textareaCaretPosition(input, prefixStart) : null;
  const top = position?.top ?? bounds?.top ?? window.innerHeight - 8;
  return {
    left: Math.max(8, Math.min(position?.left ?? bounds?.left ?? 8, window.innerWidth - 328)),
    bottom: window.innerHeight - top + 2,
    maxHeight: Math.max(40, top - 12),
  };
}
//=============================================================================
