//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { UIControl } from '../rpc/types';
//=============================================================================
export function positionFigureControl(control: UIControl, heightOffset: number): UIControl {
  if (control.units !== 'pixels' || heightOffset === 0) return control;
  // Pixel controls retain their size and bottom-left anchor, just like pixel axes.
  // Their serialized y coordinate uses the authored figure height.
  return {
    ...control,
    y: control.y + heightOffset,
  };
}
//=============================================================================
export function figureWindowNeedsResize(
  requestedWidth: number,
  requestedHeight: number,
  canvasWidth: number,
  canvasHeight: number,
  tolerance = 1
): boolean {
  return (
    Math.abs(requestedWidth - canvasWidth) > tolerance ||
    Math.abs(requestedHeight - canvasHeight) > tolerance
  );
}
//=============================================================================
