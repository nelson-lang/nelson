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
import { figureStateArrays } from '../rpc/figureTypes';
import type { FigureActionResult, FigureInspectorState, FigureDataPoint } from '../rpc/figureTypes';
import type { FigureDisplayListResult } from '../rpc/types';
//=============================================================================
export function useFigureInspector(
  handle: number,
  applyDisplayList: (dl: FigureDisplayListResult) => void,
  observe = true
) {
  const observing = useRef(observe);
  observing.current = observe;
  const [state, setState] = useState<FigureInspectorState | null>(null);
  const [axesHandle, setAxesHandle] = useState(0);
  const [point, setPoint] = useState<FigureDataPoint | null>(null);
  const [pending, setPending] = useState(false);
  const [activeActions, setActiveActions] = useState(0);
  const [error, setError] = useState('');
  const apply = useRef(applyDisplayList);
  apply.current = applyDisplayList;
  const generation = useRef(0);
  const alive = useRef(false);
  const mutation = useRef(0);
  const chain = useRef(Promise.resolve());
  const delayedPick = useRef<Record<string, unknown> | null>(null);
  const repeatAction = useRef<(request: Record<string, unknown>) => Promise<void>>();
  const accept = useCallback((result: FigureActionResult) => {
    if (result.state) {
      const next = figureStateArrays(result.state);
      setState((current) => (JSON.stringify(current) === JSON.stringify(next) ? current : next));
      setAxesHandle((selected) =>
        next.axes.some((axis) => axis.handle === selected) ? selected : next.axes[0]?.handle ?? 0
      );
    }
    if (result.displayList) apply.current(result.displayList);
    if (result.point) {
      setPoint(result.point);
      if (result.point.axesHandle) setAxesHandle(result.point.axesHandle);
    }
    setPending(!!result.queued);
  }, []);

  const refresh = useCallback(async () => {
    if (!observing.current || isEngineBusy() || !alive.current) return;
    const ticket = generation.current;
    const version = mutation.current;
    try {
      const result = await withFigureSlot(() =>
        ticket === generation.current && alive.current && observing.current && !isEngineBusy()
          ? rpc<FigureActionResult>('figure.action', { handle, request: { action: 'state' } })
          : Promise.resolve(null)
      );
      if (
        result &&
        observing.current &&
        ticket === generation.current &&
        version === mutation.current
      )
        accept(result);
    } catch (err) {
      if (ticket === generation.current) setError(err instanceof Error ? err.message : String(err));
    }
  }, [handle, accept]);
  useEffect(() => {
    generation.current++;
    alive.current = true;
    chain.current = Promise.resolve();
    setState(null);
    setPoint(null);
    setError('');
    setActiveActions(0);
    let timer: ReturnType<typeof setTimeout> | undefined;
    const schedule = () => {
      if (timer !== undefined || !observing.current || isEngineBusy()) return;
      timer = setTimeout(() => {
        timer = undefined;
        void refresh();
      }, 200);
    };
    const offReady = onEngineReady(() => {
      setPoint(null);
      void refresh();
      const request = delayedPick.current;
      delayedPick.current = null;
      if (request) void repeatAction.current?.(request);
    });
    const offFrame = onPush('figure.frame', (payload) => {
      if ((payload as { handle?: number }).handle === handle) {
        schedule();
      }
    });
    const offUpdated = onPush('figure.updated', (payload) => {
      if ((payload as { handle?: number }).handle === handle) schedule();
    });
    return () => {
      alive.current = false;
      generation.current++;
      delayedPick.current = null;
      offReady();
      offFrame();
      offUpdated();
      if (timer !== undefined) clearTimeout(timer);
    };
  }, [handle, refresh]);
  useEffect(() => {
    if (observe) void refresh();
  }, [handle, observe, refresh]);

  const action = useCallback(
    (request: Record<string, unknown>) => {
      if (!alive.current) return Promise.resolve();
      if (request.action === 'pick' && isEngineBusy()) {
        delayedPick.current = { axesHandle, ...request };
        setPending(true);
        return Promise.resolve();
      }
      if (request.action !== 'pick' && request.action !== 'step') setPoint(null);
      mutation.current++;
      setActiveActions((count) => count + 1);
      setPending(true);
      setError('');
      const ticket = generation.current;
      const run = async () => {
        if (ticket !== generation.current || !alive.current) return;
        try {
          const result = await withFigureSlot(() =>
            ticket === generation.current && alive.current
              ? rpc<FigureActionResult>('figure.action', {
                  handle,
                  request: { axesHandle, ...request },
                })
              : Promise.resolve(null)
          );
          if (result && ticket === generation.current) accept(result);
        } catch (err) {
          if (ticket === generation.current) {
            setError(err instanceof Error ? err.message : String(err));
            setPending(false);
            await refresh();
          }
        } finally {
          if (ticket === generation.current) setActiveActions((count) => count - 1);
        }
      };
      chain.current = chain.current.then(run, run);
      return chain.current;
    },
    [handle, axesHandle, accept, refresh]
  );
  repeatAction.current = action;
  const clearPoint = useCallback(() => setPoint(null), []);
  const axes = state?.axes.find((axis) => axis.handle === axesHandle) ?? null;
  return {
    state,
    axes,
    axesHandle,
    setAxesHandle,
    point,
    pending: pending || activeActions > 0,
    error,
    action,
    refresh,
    clearPoint,
  };
}
//=============================================================================
