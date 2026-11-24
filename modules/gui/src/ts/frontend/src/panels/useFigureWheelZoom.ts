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
import { createFigureWheelZoom } from './figureWheelZoom';
//=============================================================================
export function useFigureWheelZoom(
  handle: number,
  stageRef: RefObject<HTMLElement | null>,
  dl: FigureDisplayListResult | null,
  selectAxes: (handle: number) => void,
  action: (request: Record<string, unknown>) => Promise<void>,
  refresh?: () => void
) {
  const options = useRef({ dl, selectAxes, action, refresh });
  options.current = { dl, selectAxes, action, refresh };
  const controller = useRef<ReturnType<typeof createFigureWheelZoom> | null>(null);
  const ready = !!dl;
  useEffect(() => {
    const stage = stageRef.current;
    if (!stage || !ready) return;
    const zoom = createFigureWheelZoom({
      stage,
      axes: () => {
        const dl = options.current.dl;
        // A figure with a WindowScrollWheelFcn owns the wheel: no axes to zoom.
        return dl?.hasScrollWheelCallback ? undefined : dl?.rotatableAxes;
      },
      selectAxes: (handle) => options.current.selectAxes(handle),
      action: (request) => options.current.action(request),
      refresh: () => options.current.refresh?.(),
    });
    controller.current = zoom;
    stage.addEventListener('wheel', zoom.wheel, { passive: false });
    return () => {
      zoom.dispose();
      stage.removeEventListener('wheel', zoom.wheel);
      if (controller.current === zoom) controller.current = null;
    };
  }, [handle, stageRef, ready]);
  return () => controller.current?.flush();
}
//=============================================================================
