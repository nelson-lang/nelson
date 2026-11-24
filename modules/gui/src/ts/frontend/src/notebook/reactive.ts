//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Dependency analysis for reactive execution.
//
// A reactive Notebook re-runs only the code cells that depend on a changed
// variable (a control value, or a variable assigned by a cell that itself
// re-ran). The analysis over-approximates on purpose: it never re-runs fewer
// cells than it should, so a missed dependency cannot leave a stale result. At
// worst it re-runs a cell that did not strictly need it.
const KEYWORDS = new Set([
  'if',
  'elseif',
  'else',
  'end',
  'for',
  'parfor',
  'while',
  'switch',
  'case',
  'otherwise',
  'function',
  'endfunction',
  'return',
  'break',
  'continue',
  'try',
  'catch',
  'do',
  'until',
  'global',
  'persistent',
  'true',
  'false',
  'end_try_catch',
  'end_unwind_protect',
  'unwind_protect',
  'unwind_protect_cleanup',
]);
//=============================================================================
const IDENT = /[A-Za-z_][A-Za-z0-9_]*/g;
/** Remove comments and string literals so they do not pollute identifier scans. */
function stripNonCode(code: string): string {
  let s = code.replace(/%\{[\s\S]*?%\}/g, ' ');
  s = s.replace(/%[^\n]*/g, ' ');
  s = s.replace(/"(?:[^"\\]|\\.)*"/g, ' "" ');
  s = s.replace(/'(?:[^'])*'/g, " '' "); // single-quoted strings (char arrays)
  return s;
}
//=============================================================================
/** Identifiers the statement reads: all identifiers, minus keywords and `.field` names. */
function readsOf(stmt: string): string[] {
  const noFields = stmt.replace(/\.\s*[A-Za-z_]\w*/g, ' ');
  const out: string[] = [];
  for (const m of noFields.matchAll(IDENT)) {
    const id = m[0];
    if (!KEYWORDS.has(id)) out.push(id);
  }
  return out;
}
//=============================================================================
/** Variables a statement assigns (LHS targets), handling `[a,b] = ...` and indexing. */
function assignsOf(stmt: string): string[] {
  const t = stmt.trim();
  // [a, b, c] = f(...)
  const multi = /^\[([^\]]*)\]\s*=(?!=)/.exec(t);
  if (multi) {
    return multi[1]
      .split(',')
      .map((p) => /^\s*([A-Za-z_]\w*)/.exec(p)?.[1] ?? '')
      .filter(Boolean);
  }
  // name, name(idx), name{idx}, name.field ... = (but not ==, <=, >=, ~=)
  const one = /^([A-Za-z_]\w*)\s*(?:\([^)]*\)|\{[^}]*\}|\.\s*[A-Za-z_]\w*)*\s*=(?!=)/.exec(t);
  if (one && !/[<>~!=+\-*/^]$/.test(t.slice(0, one[0].length - 1).trimEnd())) {
    return [one[1]];
  }
  return [];
}
//=============================================================================
export interface CellDeps {
  assigns: Set<string>;
  reads: Set<string>;
}
//=============================================================================
/** Static read/assign sets for one code cell. */
export function analyzeCell(code: string): CellDeps {
  const assigns = new Set<string>();
  const reads = new Set<string>();
  const stmts = stripNonCode(code ?? '').split(/[;\n]+/);
  for (const stmt of stmts) {
    for (const a of assignsOf(stmt)) assigns.add(a);
    for (const r of readsOf(stmt)) reads.add(r);
  }
  return { assigns, reads };
}
//=============================================================================
type Block = { id?: string; type: string; props?: Record<string, unknown> };
/**
 * Ids of the code cells that are out of date (stale) given the code each cell
 * had when it last ran (`lastRunCode`, keyed by block id). A cell is stale if
 * its source changed or it never ran, and, transitively, if it reads a variable
 * produced by a stale cell above it. One pass in document order.
 */
export function computeStale(blocks: Block[], lastRunCode: Record<string, string>): Set<string> {
  const stale = new Set<string>();
  const dirty = new Set<string>();
  for (const b of blocks) {
    if (b.type !== 'codecell' || !b.id) continue;
    const code = String(b.props?.code ?? '');
    const { assigns, reads } = analyzeCell(code);
    const last = lastRunCode[b.id];
    const sourceStale = last === undefined || last !== code;
    let depStale = false;
    for (const r of reads)
      if (dirty.has(r)) {
        depStale = true;
        break;
      }
    if (sourceStale || depStale) {
      stale.add(b.id);
      for (const a of assigns) dirty.add(a);
    }
  }
  return stale;
}
/**
 * Ordered ids of the code cells to re-run reactively after `changedVar` changes.
 * Forward pass in document order: a cell re-runs if it reads a "dirty" variable,
 * and then its own assignments become dirty for the cells below it.
 */
export function reactiveTargets(blocks: Block[], changedVar: string): Set<string> {
  const targets = new Set<string>();
  if (!changedVar) return targets;
  const dirty = new Set<string>([changedVar]);
  for (const b of blocks) {
    if (b.type !== 'codecell' || !b.id) continue;
    const { assigns, reads } = analyzeCell(String(b.props?.code ?? ''));
    let hit = false;
    for (const r of reads)
      if (dirty.has(r)) {
        hit = true;
        break;
      }
    if (hit) {
      targets.add(b.id);
      for (const a of assigns) dirty.add(a);
    }
  }
  return targets;
}
//=============================================================================
