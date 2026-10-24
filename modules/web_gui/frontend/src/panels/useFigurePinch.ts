//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef } from 'react';
import type { RefObject } from 'react';
import type { FigureDisplayListResult } from '../rpc/types';
import { createFigurePinch } from './figurePinch';
//=============================================================================
export function useFigurePinch(
  handle: number,
  stageRef: RefObject<HTMLElement | null>,
  dl: FigureDisplayListResult | null,
  selectAxes: (handle: number) => void,
  action: (request: Record<string, unknown>) => Promise<void>
) {
  const options = useRef({ dl, selectAxes, action });
  options.current = { dl, selectAxes, action };
  const ready = !!dl;
  useEffect(() => {
    const stage = stageRef.current;
    if (!stage || !ready) return;
    const pinch = createFigurePinch({
      stage,
      axes: () => options.current.dl?.rotatableAxes,
      selectAxes: (h) => options.current.selectAxes(h),
      action: (request) => options.current.action(request),
    });
    stage.addEventListener('pointerdown', pinch.down);
    stage.addEventListener('pointermove', pinch.move, { passive: false });
    stage.addEventListener('pointerup', pinch.up);
    stage.addEventListener('pointercancel', pinch.up);
    return () => {
      pinch.dispose();
      stage.removeEventListener('pointerdown', pinch.down);
      stage.removeEventListener('pointermove', pinch.move);
      stage.removeEventListener('pointerup', pinch.up);
      stage.removeEventListener('pointercancel', pinch.up);
    };
  }, [handle, stageRef, ready]);
}
//=============================================================================
