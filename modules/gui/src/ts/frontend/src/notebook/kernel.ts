//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Notebook kernel adapter. One RPC per cell: `notebook.run` evaluates the
// cell against the shared base workspace and returns the captured output, the
// error identifier/message, and any figures it produced (as data-URL images).
// The same method is served by the mock engine and by the C++ backend
// (WebGuiRpc), so the panel never knows which one answers.
import { rpc } from '../rpc/client';
import type { FigureImageResult } from '../rpc/types';
//=============================================================================
export interface CellResult {
  ok: boolean;
  output: string;
  errorId: string;
  errorMessage: string;
  figures: FigureImageResult[];
}
export interface RunCellOptions {
  resetLiveFigures?: boolean;
}
//=============================================================================
// A browser reload can leave the shared Wasm worker with figures owned by the
// previous Notebook frontend. Request one cleanup on the first cell run of
// this frontend session; subsequent runs use the normal per-cell handles.
let liveFiguresReset = false;
//=============================================================================
export async function runCell(
  code: string,
  reuseHandles: number[] = [],
  options: RunCellOptions = {}
): Promise<CellResult> {
  try {
    // Ask the engine to keep the cell's figures alive so the interactive figure
    // panel can render them inline; the mock engine ignores this and returns a
    // static image, which the panel shows as a PNG fallback. `reuseHandles` are the
    // figures this cell owned on its previous run: the engine closes them before
    // its baseline snapshot, so the cell's figure() calls re-grab the same numbers
    // and the figures update in place instead of climbing a new number each run.
    const r = await rpc<CellResult>('notebook.run', {
      code,
      interactiveFigures: true,
      reuseHandles,
      resetLiveFigures: options.resetLiveFigures ?? !liveFiguresReset,
    });
    // Keep the cleanup request armed when the first RPC reports an error. This
    // lets the next run recover from a transient worker/interpreter failure.
    if (r.ok ?? !r.errorMessage) liveFiguresReset = true;
    return {
      ok: r.ok ?? !r.errorMessage,
      output: r.output ?? '',
      errorId: r.errorId ?? '',
      errorMessage: r.errorMessage ?? '',
      figures: Array.isArray(r.figures) ? r.figures : [],
    };
  } catch (e) {
    return {
      ok: false,
      output: '',
      errorId: 'Nelson:Notebook:transport',
      errorMessage: (e as Error).message,
      figures: [],
    };
  }
}
//=============================================================================
/** Assign control variables into the base workspace in one round trip. */
export async function applyWorkspace(
  assignments: string[],
  options: RunCellOptions = {}
): Promise<CellResult | undefined> {
  const nonEmpty = assignments.filter((a) => a.trim().length > 0);
  if (nonEmpty.length === 0) return undefined;
  return runCell(nonEmpty.map((a) => (a.endsWith(';') ? a : a + ';')).join('\n'), [], options);
}
//=============================================================================
export async function resetWorkspace(options: RunCellOptions = {}): Promise<CellResult> {
  return runCell('clear', [], options);
}
//=============================================================================
