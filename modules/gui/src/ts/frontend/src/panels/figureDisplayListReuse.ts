//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { FigureDisplayListResult } from '../rpc/types';
//=============================================================================
export function reuseStableDisplayListParts(
  current: FigureDisplayListResult | null,
  next: FigureDisplayListResult
): FigureDisplayListResult {
  if (current && next.controls === undefined) {
    return {
      ...next,
      controls: current.controls,
      menus: next.menus ?? current.menus,
      contextMenu: next.contextMenu ?? current.contextMenu,
      controlLayoutWidth: next.controlLayoutWidth ?? current.controlLayoutWidth,
      controlLayoutHeight: next.controlLayoutHeight ?? current.controlLayoutHeight,
    };
  }
  if (
    !current?.layoutSignature ||
    !next.layoutSignature ||
    current.layoutSignature !== next.layoutSignature ||
    current.width !== next.width ||
    current.height !== next.height
  ) {
    return next;
  }
  return {
    ...next,
    controls: current.controls,
    menus: current.menus,
    contextMenu: current.contextMenu,
    controlLayoutWidth: current.controlLayoutWidth,
    controlLayoutHeight: current.controlLayoutHeight,
  };
}
//=============================================================================
