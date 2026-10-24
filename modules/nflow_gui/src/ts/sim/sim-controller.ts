//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Run / pause / resume / stop lifecycle of one simulation, as an explicit
// state machine:
//
//   idle --start--> running --pause--> paused --resume--> running
//   running|paused --stop--> stopping --(run settles)--> idle
//   running|paused --(run settles)--> idle
//
// A single run is in flight at a time; stop() aborts the run's signal and
// resolves once the run has settled, which `simulate` guarantees within its
// stop grace period.
export type SimPhase = 'idle' | 'running' | 'paused' | 'stopping';
export interface SimControllerDeps {
  run(signal: AbortSignal): Promise<unknown>;
  rpc(method: string, params?: Record<string, unknown>): Promise<unknown>;
}
//=============================================================================
export interface SimController {
  readonly phase: SimPhase;
  start(): Promise<unknown> | null;
  togglePause(): Promise<void>;
  stop(): Promise<boolean>;
  subscribe(listener: (phase: SimPhase) => void): () => void;
}
//=============================================================================
// Engine control requests must not block the UI when the engine is gone.
export const SIM_CONTROL_TIMEOUT_MS = 5000;
export function createSimController(deps: SimControllerDeps): SimController {
  let phase: SimPhase = 'idle';
  let abort: AbortController | null = null;
  let runPromise: Promise<unknown> | null = null;
  const listeners = new Set<(phase: SimPhase) => void>();

  const setPhase = (next: SimPhase) => {
    if (next === phase) return;
    phase = next;
    for (const listener of listeners) listener(next);
  };

  const control = async (method: string, params?: Record<string, unknown>) => {
    try {
      await deps.rpc(method, params);
    } catch (_err) {
      // The engine may already have finished the run: nothing to control.
    }
  };

  const start = (): Promise<unknown> | null => {
    if (phase !== 'idle') return null;
    const controller = new AbortController();
    abort = controller;
    setPhase('running');
    // The async wrapper calls `run` synchronously and turns a throw into a rejection.
    const promise = (async () => deps.run(controller.signal))().finally(() => {
      abort = null;
      runPromise = null;
      setPhase('idle');
    });
    runPromise = promise;
    return promise;
  };

  const togglePause = async (): Promise<void> => {
    if (phase === 'running') {
      setPhase('paused');
      await control('nflow.sim.pause', { pause: true });
    } else if (phase === 'paused') {
      setPhase('running');
      await control('nflow.sim.pause', { pause: false });
    }
  };

  const stop = async (): Promise<boolean> => {
    if (phase === 'idle') return false;
    const pending = runPromise;
    if (phase !== 'stopping') {
      setPhase('stopping');
      abort?.abort();
      await control('nflow.sim.stop');
    }
    await pending?.catch(() => undefined);
    return true;
  };

  return {
    get phase() {
      return phase;
    },
    start,
    togglePause,
    stop,
    subscribe(listener) {
      listeners.add(listener);
      return () => listeners.delete(listener);
    },
  };
}
//=============================================================================
