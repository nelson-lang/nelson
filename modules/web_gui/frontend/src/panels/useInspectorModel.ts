//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useEffect, useRef, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import { isEngineBusy, onEngineReady } from '../desktop/engineSession';
import { withFigureSlot } from '../desktop/figureQueue';
import type { FigureActionResult, InspectorPayload, InspectorTreeNode } from '../rpc/figureTypes';
//=============================================================================
// Fetches the property-inspector model ({model, tree}) of the selected object.
// The Qt figure window uses the same payload. It is refetched only when the
// selection changes or the figure updates while the panel is open.
export function useInspectorModel(
  handle: number,
  selectedObject: number,
  observe: boolean
): InspectorPayload | null {
  const [payload, setPayload] = useState<InspectorPayload | null>(null);
  const observing = useRef(observe);
  observing.current = observe;
  const selection = useRef(selectedObject);
  selection.current = selectedObject;
  const alive = useRef(false);

  const fetchModel = useCallback(async () => {
    if (!observing.current || isEngineBusy() || !alive.current) return;
    try {
      const result = await withFigureSlot(() =>
        alive.current && observing.current && !isEngineBusy()
          ? rpc<FigureActionResult>('figure.action', {
              handle,
              request: { action: 'inspect', objectHandle: selection.current || 0 },
            })
          : Promise.resolve(null)
      );
      if (result && alive.current && result.model) {
        setPayload({ model: result.model, tree: result.tree as InspectorTreeNode });
      }
    } catch {
      /* transient; the next refresh retries */
    }
  }, [handle]);

  useEffect(() => {
    alive.current = true;
    let timer: ReturnType<typeof setTimeout> | undefined;
    const schedule = () => {
      if (timer !== undefined || !observing.current || isEngineBusy()) return;
      timer = setTimeout(() => {
        timer = undefined;
        void fetchModel();
      }, 200);
    };
    const offReady = onEngineReady(() => void fetchModel());
    const offUpdated = onPush('figure.updated', (p) => {
      if ((p as { handle?: number }).handle === handle) schedule();
    });
    const offFrame = onPush('figure.frame', (p) => {
      if ((p as { handle?: number }).handle === handle) schedule();
    });
    return () => {
      alive.current = false;
      offReady();
      offUpdated();
      offFrame();
      if (timer !== undefined) clearTimeout(timer);
    };
  }, [handle, fetchModel]);

  useEffect(() => {
    if (observe) void fetchModel();
    else setPayload(null);
  }, [handle, selectedObject, observe, fetchModel]);

  return payload;
}
//=============================================================================
