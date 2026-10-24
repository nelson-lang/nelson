//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { writeStatus } from '../app/status-service.js';
import { setSignalValues, clearSignalValues, applyVectorWireStyles } from '../app/signal-badges.js';
import { buildDiagramAndScopeMap, processFinalScopes } from './index.js';
import { t } from '../utils/i18n.js';
import type { PartialResult, FinalResult } from './parse-results.js';
import { applyDashboardUpdates, renderPartialStore } from './parse-results.js';
import { applyPartial, createPartialStore } from './partial-store.js';
import { nflowRpc, onNFlowPush } from '../app/native-rpc.js';
import { getCurrentFilePath } from '../app/state.js';
import { emitAppEvent } from '../app/app-events.js';
//=============================================================================
// Directory of the current .nflow file; empty for an unsaved diagram.
function diagramBaseDir(): string {
  const p = getCurrentFilePath();
  if (!p) return '';
  const idx = Math.max(p.lastIndexOf('/'), p.lastIndexOf('\\'));
  return idx >= 0 ? p.slice(0, idx) : '';
}
//=============================================================================
export interface SimulationConfig {
  state: unknown;
  runtimeInput?: HTMLInputElement | null;
  statusEl?: HTMLElement | null;
  onStatusUpdate?: ((msg: string, time: number) => void) | null;
  // Aborted when the user stops the run (the engine is told separately).
  signal?: AbortSignal;
  // How long a stopped run still waits for the engine's final result.
  stopGraceMs?: number;
}
//=============================================================================
export const DEFAULT_STOP_GRACE_MS = 3000;
const STOPPED = Symbol('stopped');
export function simulate(config: SimulationConfig): Promise<unknown> {
  return runSimulationCpp(config);
}
//=============================================================================
// After a stop request the engine still delivers its final (partial) result:
// wait for it at most `graceMs`, so a lost reply can never hang the caller.
function awaitResultOrStop(
  result: Promise<unknown>,
  signal: AbortSignal | undefined,
  graceMs: number
): Promise<unknown> {
  if (!signal) return result;
  return new Promise((resolve, reject) => {
    let timer: ReturnType<typeof setTimeout> | undefined;
    const arm = () => {
      timer = setTimeout(() => resolve(STOPPED), graceMs);
    };
    const settle = () => {
      if (timer !== undefined) clearTimeout(timer);
      signal.removeEventListener('abort', arm);
    };
    if (signal.aborted) arm();
    else signal.addEventListener('abort', arm, { once: true });
    result.then(
      (value) => {
        settle();
        resolve(value);
      },
      (error) => {
        settle();
        reject(error);
      }
    );
  });
}
//=============================================================================
function parseSimulationPayload(payload: unknown): unknown {
  if (typeof payload !== 'string') return payload;
  try {
    return JSON.parse(payload || '{}');
  } catch (error) {
    const tail = payload.slice(-80).replace(/[\r\n]/g, ' ');
    throw new SyntaxError(
      `${String(error)} (${payload.length} chars, tail: ${JSON.stringify(tail)})`
    );
  }
}
//=============================================================================
function isJsonParseError(error: unknown): boolean {
  if (error instanceof SyntaxError) return true;
  const message = error instanceof Error ? `${error.name}: ${error.message}` : String(error);
  return /json.*parse|parse.*json|expected\s+['"]?[}\]]/i.test(message);
}
//=============================================================================
async function pollSimulationResult(
  runId: string,
  onPartial: (partial: PartialResult) => void,
  signal: AbortSignal | undefined,
  graceMs: number
): Promise<unknown> {
  let malformedResponseCount = 0;
  let stopDeadline: number | null = null;
  for (;;) {
    if (signal?.aborted) {
      stopDeadline ??= Date.now() + graceMs;
      if (Date.now() > stopDeadline) return STOPPED;
    }
    let response: {
      status?: unknown;
      reason?: unknown;
      result?: unknown;
      resultJson?: unknown;
      partials?: unknown;
    };
    try {
      const payload = await nflowRpc<unknown>('nflow.sim.poll', { runId });
      response = parseSimulationPayload(payload) as typeof response;
      malformedResponseCount = 0;
    } catch (error) {
      malformedResponseCount += 1;
      if (!isJsonParseError(error) || malformedResponseCount > 3) throw error;
      await new Promise((resolve) => setTimeout(resolve, 100));
      continue;
    }
    if (Array.isArray(response?.partials)) {
      for (const partial of response.partials) {
        const parsedPartial = parseSimulationPayload(partial) as PartialResult;
        onPartial(parsedPartial);
      }
    }
    if (response?.status === 'complete') {
      return response.result !== undefined ? response.result : response.resultJson;
    }
    if (response?.status === 'error') {
      throw new Error(String(response.reason || 'simulation polling failed'));
    }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
}
//=============================================================================
async function runSimulationCpp({
  state,
  runtimeInput,
  statusEl,
  onStatusUpdate = null,
  signal,
  stopGraceMs = DEFAULT_STOP_GRACE_MS,
}: SimulationConfig): Promise<unknown> {
  writeStatus(statusEl, t('Running...'));
  if (onStatusUpdate) onStatusUpdate(t('Running...'), 0);
  clearSignalValues();
  emitAppEvent('nf-error-blocks', { ids: null });

  const { diagram, scopeBlockMap } = buildDiagramAndScopeMap(state, runtimeInput, diagramBaseDir());
  updateClockIconDisplays(state, 0, true);

  // Incremental preview store: deltas are ACCUMULATED immediately on
  // arrival (replacing a pending frame would lose samples), and only the
  // render is coalesced to one draw per animation frame.
  const partialStore = createPartialStore();
  let _rafId: number | null = null;
  const _flushPartial = () => {
    _rafId = null;
    updateClockIconDisplays(state, partialStore.currentTime ?? 0);
    renderPartialStore(partialStore, scopeBlockMap, statusEl, onStatusUpdate);
  };

  const handlePartialScopes = (partialResult: PartialResult) => {
    if (!applyPartial(partialStore, partialResult)) return; // dup/stale frame
    // Dashboards are event-driven widgets: forward every frame's updates on
    // arrival (dashboardScope entries are delta-encoded and appended by the
    // widget), so no update is lost to render coalescing.
    applyDashboardUpdates(partialResult?.dashboardUpdates);
    if (_rafId === null) _rafId = requestAnimationFrame(_flushPartial) as unknown as number;
  };
  const unsubscribePartial = onNFlowPush(
    'nflow.sim.partial',
    handlePartialScopes as (payload: unknown) => void
  );

  let runId: string | null = null;
  let resolveFinal: ((payload: unknown) => void) | null = null;
  const pendingFinalPayloads: unknown[] = [];
  const finalPromise = new Promise<unknown>((resolve) => {
    resolveFinal = resolve;
  });
  const tryResolveFinal = (payload: unknown): boolean => {
    const obj = payload as { runId?: unknown; result?: unknown; resultJson?: unknown };
    if (!runId) {
      pendingFinalPayloads.push(payload);
      return false;
    }
    if (String(obj?.runId || '') !== runId) return false;
    resolveFinal?.(obj.result !== undefined ? obj.result : obj.resultJson);
    return true;
  };
  const unsubscribeFinal = onNFlowPush('nflow.sim.final', tryResolveFinal);

  let resultPayload: unknown;
  try {
    const sendDiagram = diagram;
    try {
      if (
        typeof localStorage !== 'undefined' &&
        localStorage.getItem &&
        localStorage.getItem('nflow.debugSim') === '1'
      ) {
        // eslint-disable-next-line no-console
        console.debug('[nflow] Simulation payload:', sendDiagram);
      }
    } catch (e) {
      // ignore localStorage access errors in constrained environments
    }
    resultPayload = await awaitResultOrStop(
      nflowRpc<unknown>('nflow.sim.run', {
        diagramJson: JSON.stringify(sendDiagram),
        async: true,
      }),
      signal,
      stopGraceMs
    );
    let startOrResult: { status?: unknown; runId?: unknown; delivery?: unknown } | unknown =
      resultPayload;
    try {
      startOrResult = parseSimulationPayload(resultPayload);
    } catch {
      startOrResult = resultPayload;
    }
    if (
      startOrResult &&
      typeof startOrResult === 'object' &&
      (startOrResult as { status?: unknown }).status === 'started' &&
      typeof (startOrResult as { runId?: unknown }).runId === 'string'
    ) {
      runId = String((startOrResult as { runId: string }).runId);
      if ((startOrResult as { delivery?: unknown }).delivery === 'poll') {
        resultPayload = await pollSimulationResult(runId, handlePartialScopes, signal, stopGraceMs);
      } else {
        for (const payload of pendingFinalPayloads.splice(0)) {
          if (tryResolveFinal(payload)) break;
        }
        resultPayload = await awaitResultOrStop(finalPromise, signal, stopGraceMs);
      }
    } else {
      resultPayload = startOrResult;
    }
  } catch (err) {
    if (typeof _rafId === 'number') cancelAnimationFrame(_rafId);
    unsubscribePartial();
    unsubscribeFinal();
    if (signal?.aborted) return { status: 'stopped', session: null };
    writeStatus(statusEl, t('Error: {message}', { message: String(err) }));
    return { status: 'error', reason: String(err), session: null };
  }

  if (typeof _rafId === 'number') cancelAnimationFrame(_rafId);
  unsubscribePartial();
  unsubscribeFinal();
  if (resultPayload === STOPPED) return { status: 'stopped', session: null };

  let result: unknown = resultPayload;
  try {
    result = parseSimulationPayload(resultPayload);
  } catch (err) {
    writeStatus(statusEl, t('Error: invalid response from C++ engine'));
    return { status: 'error', reason: 'parse_error', session: null };
  }

  const resObj = result as FinalResult;
  if (resObj.status === 'error') {
    const reason = typeof resObj.reason === 'string' ? resObj.reason : 'unknown';
    let errorBlockId: string | null = null;
    if (Array.isArray(resObj.errorBlocks) && resObj.errorBlocks.length > 0) {
      errorBlockId = String(resObj.errorBlocks[0]);
      emitAppEvent('nf-error-blocks', { ids: resObj.errorBlocks.map(String) });
    } else if (reason.startsWith('unsupported_block:')) {
      const badType = reason.slice('unsupported_block:'.length).trim();
      const blocks = (state as { blocks?: unknown } | null)?.blocks;
      const values =
        blocks instanceof Map ? Array.from(blocks.values()) : Array.isArray(blocks) ? blocks : [];
      const hit = (values as Array<{ type?: string; id?: string }>).find(
        (b) => b?.type === badType
      );
      if (hit?.id) errorBlockId = String(hit.id);
    }
    writeStatus(statusEl, t('Error: {reason}', { reason }), 'error', null, errorBlockId);
    return { status: 'error', reason, session: null };
  }

  setSignalValues(
    resObj.signals,
    resObj.signalTypes,
    resObj.signalDims,
    resObj.signalComplex,
    resObj.signalBus
  );
  applyVectorWireStyles();
  processFinalScopes(resObj, scopeBlockMap, statusEl, onStatusUpdate);
  updateClockIconDisplays(state, readRuntimeSeconds(runtimeInput), true);

  writeStatus(statusEl, t('Done'));
  if (onStatusUpdate) onStatusUpdate(t('Done'), 0);
  emitAppEvent('nf-sim-time', {
    time: readRuntimeSeconds(runtimeInput),
    progress: 100,
    done: true,
  });
  return { status: 'done', session: null };
}
//=============================================================================
function readRuntimeSeconds(runtimeInput?: HTMLInputElement | null): number {
  const runtime = Number(runtimeInput?.value ?? 0);
  return Number.isFinite(runtime) ? runtime : 0;
}
//=============================================================================
function formatClockIconTime(value: unknown): string {
  const n = Number(value);
  if (!Number.isFinite(n)) return '0';
  if (Math.abs(n) >= 1000 || (Math.abs(n) > 0 && Math.abs(n) < 0.001)) {
    return n.toExponential(2);
  }
  const fixed = n.toFixed(3).replace(/\.?0+$/, '');
  return fixed === '-0' || fixed === '' ? '0' : fixed;
}
//=============================================================================
function isClockDisplayEnabled(params: Record<string, unknown> | undefined): boolean {
  return (
    params?.DisplayTime === true || params?.DisplayTime === 1 || params?.DisplayTime === 'true'
  );
}
//=============================================================================
function readClockDecimation(params: Record<string, unknown> | undefined): number {
  const decimation = Number(params?.decimation ?? 10);
  return Number.isFinite(decimation) && decimation > 0 ? Math.max(1, Math.trunc(decimation)) : 10;
}
//=============================================================================
function updateClockIconDisplays(state: unknown, time: number, force = false) {
  const blocks = (state as { blocks?: unknown } | null)?.blocks;
  const values =
    blocks instanceof Map ? Array.from(blocks.values()) : Array.isArray(blocks) ? blocks : [];

  for (const block of values as Array<Record<string, any>>) {
    if (!block || block.type !== 'clock') continue;
    const params = block.params as Record<string, unknown> | undefined;
    block.clockDisplayTime = time;

    if (!isClockDisplayEnabled(params)) {
      block.clockDisplayUpdateCount = 0;
      const textEl = block.clockDisplayEl as Element | undefined;
      if (textEl) textEl.textContent = 'Clock';
      continue;
    }

    const decimation = readClockDecimation(params);
    block.clockDisplayUpdateCount = (Number(block.clockDisplayUpdateCount) || 0) + 1;
    if (!force && block.clockDisplayUpdateCount % decimation !== 0) continue;

    const textEl = block.clockDisplayEl as Element | undefined;
    if (textEl) textEl.textContent = formatClockIconTime(time);
  }
}
//=============================================================================
export * from './index.js';
//=============================================================================
