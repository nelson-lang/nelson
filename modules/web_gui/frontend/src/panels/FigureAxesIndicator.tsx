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
export function FigureAxesIndicator({ axis, label }: { axis: RotatableAxes; label: string }) {
  return (
    <div
      className="figure-axes-indicator"
      aria-hidden="true"
      data-axes-handle={axis.handle}
      style={{
        left: `${axis.rect[0] * 100}%`,
        top: `${axis.rect[1] * 100}%`,
        width: `${axis.rect[2] * 100}%`,
        height: `${axis.rect[3] * 100}%`,
      }}
    >
      <span>{label}</span>
    </div>
  );
}
//=============================================================================
