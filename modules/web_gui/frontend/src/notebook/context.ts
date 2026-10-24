//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createContext, useContext } from 'react';
//=============================================================================
export interface NotebookCtx {
  /** Reset the workspace, apply all control values, run every code cell in order. */
  runAll: () => Promise<void>;
  /** Run a single code cell against the current (shared) workspace. */
  runBlock: (blockId: string) => Promise<void>;
  /** Reset, apply controls, run every code cell down to and including this one. */
  runToHere: (blockId: string) => Promise<void>;
  /** Run the code cells in the same section (between surrounding headings). */
  runSection: (blockId: string) => Promise<void>;
  /**
   * Reactively re-run only the code cells that depend on the variable the given
   * control block drives (and, transitively, the cells downstream of those).
   */
  runReactive: (controlBlockId: string) => Promise<void>;
  running: boolean;
  /** The id of the code cell currently being evaluated, or null when idle. */
  runningBlockId: string | null;
  /** "Hide code" view mode: only text, controls and results. */
  hideCode: boolean;
  /** Where a code cell's output sits: below the code, or to its right. */
  outputLayout: 'inline' | 'right';
  /** Dark theme active (themes the embedded Monaco editors). */
  dark: boolean;
}
//=============================================================================
export const NotebookContext = createContext<NotebookCtx | null>(null);
export function useNotebook(): NotebookCtx {
  const ctx = useContext(NotebookContext);
  if (!ctx) throw new Error('NotebookContext missing');
  return ctx;
}
//=============================================================================
