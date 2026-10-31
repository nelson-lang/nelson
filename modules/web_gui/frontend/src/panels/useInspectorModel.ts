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
import { createResponseOrder, withFigureSlot } from '../desktop/figureQueue';
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
  // Server workers answer in any order: an inspect issued during a pan can come
  // back seconds after a later one, so responses are applied in issue order.
  const order = useRef(createResponseOrder());
  // One request per object at a time; a refresh asked meanwhile runs afterwards
  // with whatever is selected by then, instead of piling identical requests.
  const inFlight = useRef(new Set<number>());
  const refetch = useRef(false);

  const fetchModel = useCallback(async (): Promise<void> => {
    if (!observing.current || isEngineBusy() || !alive.current) return;
    const target = selection.current || 0;
    if (inFlight.current.has(target)) {
      refetch.current = true;
      return;
    }
    inFlight.current.add(target);
    const apply = order.current((result: FigureActionResult) => {
      // the browser moved on to another object while this one was answered
      if (target !== (selection.current || 0) || !result.model) return;
      setPayload({ model: result.model, tree: result.tree as InspectorTreeNode });
    });
    try {
      const result = await withFigureSlot(() =>
        alive.current && observing.current && !isEngineBusy()
          ? rpc<FigureActionResult>('figure.action', {
              handle,
              request: { action: 'inspect', objectHandle: target },
            })
          : Promise.resolve(null)
      );
      if (result && alive.current) apply(result);
    } catch {
      /* transient; the next refresh retries */
    } finally {
      inFlight.current.delete(target);
      if (refetch.current && alive.current) {
        refetch.current = false;
        void fetchModel();
      }
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
