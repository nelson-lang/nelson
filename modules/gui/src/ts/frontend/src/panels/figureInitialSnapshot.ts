//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { FigureDisplayListResult } from '../rpc/types';
//=============================================================================
export async function loadInitialFigureSnapshot(
  load: () => Promise<FigureDisplayListResult>,
  accept: (frame: FigureDisplayListResult) => void,
  active: () => boolean
): Promise<void> {
  for (let attempt = 0; active() && attempt < 20; attempt++) {
    try {
      const frame = await load();
      if (active()) accept(frame);
      return;
    } catch {
      if (!active()) return;
    }
    await new Promise((resolve) => window.setTimeout(resolve, 200));
  }
}
//=============================================================================
