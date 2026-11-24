//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tracks the figures a Notebook run kept alive so they can be rendered with
// the interactive figure panel inline. A handle is "live" only in the current
// engine session and only until its cell re-runs or the panel unmounts; a
// reopened notebook has no live handles, so its figures fall back to the stored
// PNG until it is run again.
import { rpc } from '../rpc/client';
//=============================================================================
const liveHandles = new Set<number>();
export function markFiguresLive(handles: number[]): void {
  for (const h of handles) if (Number.isFinite(h) && h >= 0) liveHandles.add(h);
}
//=============================================================================
/** Whether a handle is a live, interactive figure (vs a restored PNG). */
export function isFigureLive(handle: number): boolean {
  return Number.isFinite(handle) && handle >= 0 && liveHandles.has(handle);
}
//=============================================================================
/** Parse a cell's stored parallel handle array (−1 means "no live handle"). */
export function parseFigureHandles(raw: string | undefined): number[] {
  try {
    const arr = JSON.parse(raw || '[]');
    return Array.isArray(arr) ? arr.map((h) => Number(h)) : [];
  } catch {
    return [];
  }
}
/**
 * Hand one live figure back to the desktop without closing it, so it can be
 * opened as a normal figure window ("open in figure window"). Forgets it locally
 * so the cell renders its PNG fallback for that slot.
 */
export async function releaseFigure(handle: number): Promise<void> {
  liveHandles.delete(handle);
  try {
    await rpc('notebook.releaseFigure', { handle });
  } catch {
    /* engine gone, nothing to do */
  }
}
/**
 * Release the live figures with these handles: forget them locally and ask the
 * engine to close them. Called before a cell re-runs and when the panel closes.
 */
export async function closeLiveFigures(handles: number[]): Promise<void> {
  const live = handles.filter((h) => isFigureLive(h));
  if (live.length === 0) return;
  try {
    await rpc('notebook.closeFigures', { handles: live });
  } catch {
    /* engine gone or already closed, nothing to do */
  } finally {
    // Keep the handles marked live until the close RPC has completed. A late
    // frame can arrive while the worker is closing the figure; forgetting the
    // handle first would let the desktop lifecycle open a stray Figure tab.
    for (const h of live) liveHandles.delete(h);
  }
}
//=============================================================================
