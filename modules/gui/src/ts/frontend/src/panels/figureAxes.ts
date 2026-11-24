//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { RotatableAxes } from '../rpc/types';
//=============================================================================
export function pickRotatableAxes(
  axes: RotatableAxes[] | undefined,
  overlay: { left: number; top: number; width: number; height: number },
  clientX: number,
  clientY: number
): RotatableAxes | null {
  return hitFigureAxes(axes, overlay, clientX, clientY) ?? axes?.[0] ?? null;
}
//=============================================================================
export function hitFigureAxes(
  axes: RotatableAxes[] | undefined,
  overlay: { left: number; top: number; width: number; height: number },
  clientX: number,
  clientY: number
): RotatableAxes | null {
  if (!axes || axes.length === 0) {
    return null;
  }
  if (overlay.width <= 0 || overlay.height <= 0) {
    return null;
  }
  const fx = (clientX - overlay.left) / overlay.width;
  const fy = (clientY - overlay.top) / overlay.height;
  for (const candidate of axes) {
    const [left, top, w, h] = candidate.rect;
    if (fx >= left && fx <= left + w && fy >= top && fy <= top + h) {
      return candidate;
    }
  }
  return null;
}
//=============================================================================
