//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export function isFigureFormTarget(target: EventTarget | null): boolean {
  return (
    target instanceof Element &&
    !!target.closest(
      'input, select, textarea, [contenteditable]:not([contenteditable="false"]), [role="textbox"]'
    )
  );
}
//=============================================================================
export function isFigureDrawingTarget(target: EventTarget | null): boolean {
  return (
    target instanceof Element &&
    (target.tagName === 'CANVAS' ||
      target.classList.contains('figure-panel__pan') ||
      target.classList.contains('figure-panel__rotate') ||
      target.classList.contains('figure-panel__zoom'))
  );
}
//=============================================================================
export function figureShortcut(
  event: Pick<
    KeyboardEvent,
    'key' | 'ctrlKey' | 'altKey' | 'metaKey' | 'repeat' | 'isComposing'
  > & { shiftKey?: boolean },
  is3d: boolean
):
  | 'pan'
  | 'rotate'
  | 'zoom'
  | 'inspect'
  | 'fit'
  | 'undo'
  | 'redo'
  | 'arrowLeft'
  | 'arrowRight'
  | 'arrowUp'
  | 'arrowDown'
  | null {
  if (event.altKey || event.isComposing) return null;
  if (event.ctrlKey || event.metaKey) {
    if (event.repeat) return null;
    if (event.key.toLowerCase() === 'z') return event.shiftKey ? 'redo' : 'undo';
    if (event.key.toLowerCase() === 'y') return 'redo';
    return null;
  }
  if (event.key === 'ArrowLeft') return 'arrowLeft';
  if (event.key === 'ArrowRight') return 'arrowRight';
  if (event.key === 'ArrowUp') return 'arrowUp';
  if (event.key === 'ArrowDown') return 'arrowDown';
  if (event.repeat) return null;
  switch (event.key.toLowerCase()) {
    case 'p':
      return 'pan';
    case 'r':
      return is3d ? 'rotate' : null;
    case 'z':
      return 'zoom';
    case 'escape':
      return 'inspect';
    case 'f':
      return 'fit';
    default:
      return null;
  }
}
//=============================================================================
