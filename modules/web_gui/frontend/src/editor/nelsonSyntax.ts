//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export const NELSON_KEYWORDS = [
  'function',
  'endfunction',
  'end',
  'if',
  'elseif',
  'else',
  'while',
  'for',
  'switch',
  'case',
  'otherwise',
  'break',
  'continue',
  'return',
  'try',
  'catch',
  'end_try_catch',
  'do',
  'until',
  'global',
  'persistent',
] as const;
//=============================================================================
export type NelsonTokenKind = 'plain' | 'keyword' | 'string' | 'number' | 'comment' | 'operator';
export type NelsonSyntaxToken = { kind: NelsonTokenKind; text: string };
const keywords = new Set<string>(NELSON_KEYWORDS);
const numberPattern = /^(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?/;
const identifierPattern = /^[A-Za-z_]\w*/;
const operatorPattern = /^[+\-*/=<>~&|^:;,()[\]{}]+/;
function append(tokens: NelsonSyntaxToken[], kind: NelsonTokenKind, text: string): void {
  if (!text) return;
  const previous = tokens[tokens.length - 1];
  if (previous?.kind === kind) previous.text += text;
  else tokens.push({ kind, text });
}
//=============================================================================
export function tokenizeNelson(source: string): NelsonSyntaxToken[] {
  const tokens: NelsonSyntaxToken[] = [];
  let index = 0;
  while (index < source.length) {
    const rest = source.slice(index);
    if (rest[0] === '%' || rest.startsWith('//')) {
      const end = rest.indexOf('\n');
      const length = end < 0 ? rest.length : end;
      append(tokens, 'comment', rest.slice(0, length));
      index += length;
      continue;
    }
    const quote = rest[0];
    if (quote === '"' || quote === "'") {
      let length = 1;
      while (length < rest.length) {
        if (rest[length] === '\\') length += 2;
        else if (rest[length++] === quote) break;
      }
      append(tokens, 'string', rest.slice(0, length));
      index += length;
      continue;
    }
    const number = numberPattern.exec(rest)?.[0];
    if (number) {
      append(tokens, 'number', number);
      index += number.length;
      continue;
    }
    const identifier = identifierPattern.exec(rest)?.[0];
    if (identifier) {
      append(tokens, keywords.has(identifier) ? 'keyword' : 'plain', identifier);
      index += identifier.length;
      continue;
    }
    const operator = operatorPattern.exec(rest)?.[0];
    if (operator) {
      append(tokens, 'operator', operator);
      index += operator.length;
      continue;
    }
    append(tokens, 'plain', rest[0]);
    index++;
  }
  return tokens;
}
//=============================================================================
