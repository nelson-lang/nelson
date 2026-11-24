//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Small pure helpers for a code cell's output, kept out of the React component
// so they can be unit-tested directly.
/** The props that clear a single code cell's inline output (text + figures). */
export function clearedOutputProps(): {
  output: string;
  figures: string;
  figureHandles: string;
  errorMsg: string;
} {
  return { output: '', figures: '[]', figureHandles: '[]', errorMsg: '' };
}
//=============================================================================
export function isCellRunning(runningBlockId: string | null | undefined, blockId: string): boolean {
  return !!runningBlockId && runningBlockId === blockId;
}
//=============================================================================
/** Ids of the code cells whose last run reported an error, in document order. */
export function errorCellIds(
  blocks: { id?: string; type: string; props?: { errorMsg?: string } }[]
): string[] {
  return blocks
    .filter((b) => b.type === 'codecell' && !!(b.props?.errorMsg ?? '').trim() && !!b.id)
    .map((b) => b.id as string);
}
//=============================================================================
export function hasCellResult(props: {
  output?: string;
  errorMsg?: string;
  figures?: string;
}): boolean {
  if ((props.output ?? '').length > 0) return true;
  if ((props.errorMsg ?? '').length > 0) return true;
  try {
    return (
      Array.isArray(JSON.parse(props.figures || '[]')) && JSON.parse(props.figures!).length > 0
    );
  } catch {
    return false;
  }
}
//=============================================================================
