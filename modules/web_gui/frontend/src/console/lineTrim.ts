//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Line } from './ansi';
//=============================================================================
export function removeTrailingTextFromLine(line: Line, text: string): Line | null {
  if (!text) return null;
  const lineText = line.map((span) => span.text).join('');
  if (!lineText.endsWith(text)) return null;
  const next = line.map((span) => ({ ...span }));
  let remaining = text.length;
  for (let index = next.length - 1; index >= 0 && remaining > 0; index -= 1) {
    const span = next[index];
    if (span.text.length <= remaining) {
      remaining -= span.text.length;
      next.splice(index, 1);
    } else {
      span.text = span.text.slice(0, span.text.length - remaining);
      remaining = 0;
    }
  }
  return next;
}
//=============================================================================
export function linesWithoutTrailingText(lines: Line[], text: string): Line[] {
  if (!text || lines.length === 0) return lines;
  const lineIndex = lines.length - 1;
  const trimmed = removeTrailingTextFromLine(lines[lineIndex], text);
  if (!trimmed) return lines;
  const next = lines.slice();
  next[lineIndex] = trimmed;
  return next;
}
//=============================================================================
