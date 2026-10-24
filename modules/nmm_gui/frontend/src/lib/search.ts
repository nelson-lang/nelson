//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Approximate package search. Every word of the query must match somewhere:
// in the name (best), in the title, summary, description or keywords, as the
// letters of the name in order ("cmeng" finds com_engine), or with one typo
// in a word of the name or keywords. The score orders the results.
export interface Searchable {
  name?: string;
  title?: string;
  summary?: string;
  description?: string;
}
//=============================================================================
function words(query: string): string[] {
  return String(query || '').toLowerCase().split(/\s+/).filter(Boolean);
}
//=============================================================================
function isSubsequence(word: string, text: string): boolean {
  let i = 0;
  for (const c of text) if (c === word[i] && ++i === word.length) return true;
  return false;
}
//=============================================================================
function withinOneEdit(a: string, b: string): boolean {
  if (Math.abs(a.length - b.length) > 1) return false;
  let i = 0, j = 0, edits = 0;
  while (i < a.length && j < b.length) {
    if (a[i] === b[j]) { i++; j++; continue; }
    if (++edits > 1) return false;
    if (a.length > b.length) i++;
    else if (a.length < b.length) j++;
    else { i++; j++; }
  }
  return edits + (a.length - i) + (b.length - j) <= 1;
}
//=============================================================================
function wordScore(word: string, name: string, text: string, tokens: string[]): number {
  if (name.includes(word)) return name.startsWith(word) ? 120 : 100;
  if (text.includes(word)) return 40;
  if (word.length >= 3 && isSubsequence(word, name)) return 20;
  if (word.length >= 4 && tokens.some(tok => withinOneEdit(word, tok))) return 10;
  return 0;
}
//=============================================================================
// 0 = no match; otherwise the higher the better.
export function searchScore(p: Searchable, query: string, keywords: string[]): number {
  const list = words(query);
  if (!list.length) return 1;
  const name = String(p.name || '').toLowerCase();
  const text = [p.title, p.summary, p.description, ...keywords].filter(Boolean).join(' ').toLowerCase();
  const tokens = [name, ...name.split(/[_\-.]+/), ...keywords.map(k => String(k).toLowerCase())];
  let total = 0;
  for (const word of list) {
    const s = wordScore(word, name, text, tokens);
    if (!s) return 0;
    total += s;
  }
  return total;
}
//=============================================================================
// Text split into plain and matched parts, for <mark> highlighting.
export function highlightParts(text: string, query: string): { text: string; mark: boolean }[] {
  const source = String(text || '');
  const list = words(query).filter(w => w.length > 1);
  if (!list.length) return source ? [{ text: source, mark: false }] : [];
  const lower = source.toLowerCase();
  const marks = new Array<boolean>(source.length).fill(false);
  for (const word of list) {
    for (let at = lower.indexOf(word); at >= 0; at = lower.indexOf(word, at + 1)) {
      for (let k = at; k < at + word.length; k++) marks[k] = true;
    }
  }
  const parts: { text: string; mark: boolean }[] = [];
  for (let k = 0; k < source.length; k++) {
    const last = parts[parts.length - 1];
    if (last && last.mark === marks[k]) last.text += source[k];
    else parts.push({ text: source[k], mark: marks[k] });
  }
  return parts;
}
//=============================================================================
