//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// RPC client for the nelson-webview backend.
//
// Transport = WebUI (webui_bind + civetweb). The backend binds a single
// `rpc` function; every request is one JSON envelope:
//     { id, method, params }  ->  { id, result }  |  { id, error }
//
// Server -> client push (e.g. terminal output) arrives through a global
// `window.__nelsonPush(channel, payloadJson)` that the backend invokes via
// webui_script. Panels subscribe with `onPush(channel, cb)`.
//
// When `webui.js` is absent (standalone `npm run dev`), a mock backend is
// used so the UI is runnable without the C++ engine.
import { abortableSleep, withDeadline } from './deadline';
import type { DeadlineOptions } from './deadline';
import type { FigureDisplayListResult } from './types';
import { FigureImagePayloadCache } from '../panels/figureImagePayloadCache';
//=============================================================================
export type RpcParams = Record<string, unknown> | undefined;
// `signal` / `timeoutMs` bound how long the caller waits (retries included);
// the engine is not told to cancel.
export type RpcOptions = DeadlineOptions;
interface WebUiGlobal {
  call(name: string, payload: string): Promise<string>;
}
//=============================================================================
declare global {
  interface Window {
    webui?: WebUiGlobal;
    __nelsonPush?: (channel: string, payloadJson: string) => void;
  }
}
//=============================================================================
type PushHandler = (payload: unknown) => void;
const pushHandlers = new Map<string, Set<PushHandler>>();
const anyPushHandlers = new Set<(channel: string, payload: unknown) => void>();
const figureImagePayloads = new FigureImagePayloadCache();
function dispatchPush(channel: string, payload: unknown): void {
  const handlers = pushHandlers.get(channel);
  if (handlers) {
    for (const handler of handlers) handler(payload);
  }
  for (const handler of anyPushHandlers) handler(channel, payload);
}
//=============================================================================
export interface LocalRpcBackend {
  handle(method: string, params: Record<string, unknown>, signal: AbortSignal): Promise<unknown>;
  dispose?(): void;
}
//=============================================================================
let localBackend: LocalRpcBackend | null = null;
// Route backend push into subscribers. Installed once, unconditionally, so the
// real backend can call it as soon as the page loads.
window.__nelsonPush = (channel: string, payloadJson: string) => {
  let payload: unknown = payloadJson;
  try {
    payload = JSON.parse(payloadJson);
  } catch {
    /* leave as raw string */
  }
  if (channel === 'figure.frame') {
    const frame = payload as { displayList?: FigureDisplayListResult };
    if (frame?.displayList) {
      payload = { ...frame, displayList: figureImagePayloads.hydrate(frame.displayList) };
    }
  }
  dispatchPush(channel, payload);
};
//=============================================================================
export function onPush(channel: string, cb: PushHandler): () => void {
  let set = pushHandlers.get(channel);
  if (!set) {
    set = new Set();
    pushHandlers.set(channel, set);
  }
  set.add(cb);
  return () => set!.delete(cb);
}
//=============================================================================
export function onAnyPush(cb: (channel: string, payload: unknown) => void): () => void {
  anyPushHandlers.add(cb);
  return () => anyPushHandlers.delete(cb);
}
//=============================================================================
// Emit a push locally (used by the mock backend).
export function emitLocalPush(channel: string, payload: unknown): void {
  window.__nelsonPush?.(channel, JSON.stringify(payload));
}
//=============================================================================
// Local backends already own structured values. High-frequency payloads such
// as animation display lists must not take an avoidable stringify/parse trip.
export function emitLocalPushValue(channel: string, payload: unknown): void {
  dispatchPush(channel, payload);
}
//=============================================================================
export function installLocalRpcBackend(backend: LocalRpcBackend): () => void {
  localBackend?.dispose?.();
  localBackend = backend;
  return () => {
    if (localBackend !== backend) return;
    localBackend = null;
    backend.dispose?.();
  };
}
//=============================================================================
let nextId = 1;
let mockEngine: import('./mockEngine').MockEngine | null = null;
async function getMockEngine(): Promise<import('./mockEngine').MockEngine> {
  if (!mockEngine) {
    const { MockEngine } = await import('./mockEngine');
    mockEngine = new MockEngine(emitLocalPush);
  }
  return mockEngine;
}
//=============================================================================
export const isBackendConnected = (): boolean =>
  typeof window.webui?.call === 'function' || localBackend !== null;
// WebUI opens its WebSocket a beat after the page loads, so early calls fail
// until it is up. The failure is not always the tidy "WebSocket is not
// connected": webui.js can also throw a TypeError while its internal state is
// half-built. Any transport-level throw is retried (up to ~10s) for calls the
// caller marked as safe to resend. Engine-side failures never come through here
// (they are returned as an {error} envelope, not thrown).
async function callWithRetry(
  envelope: string,
  allowRetry: boolean,
  signal: AbortSignal
): Promise<string> {
  const maxAttempts = 50;
  for (let attempt = 0; ; attempt++) {
    try {
      return await window.webui!.call('rpc', envelope);
    } catch (e) {
      if (allowRetry && attempt < maxAttempts && !signal.aborted) {
        await abortableSleep(200, signal);
        continue;
      }
      throw e;
    }
  }
}
//=============================================================================
// Retrying resends the SAME envelope, so it must be limited to idempotent reads.
// Re-sending a side-effectful call (a callback, a command, a cell edit) would
// execute it again, e.g. a uicontrol click firing its Callback two or three times
// over a flaky cold connection. These methods are sent exactly once.
const NON_IDEMPOTENT_METHODS = new Set<string>([
  'figure.close',
  'uicontrol.action',
  'term.input',
  'shell.run',
  'var.setCell',
  'editor.save',
  'examples.open',
  'examples.run',
  'debugger.toggleBreakpoint',
  'history.clear',
  'fs.chdir',
  'fs.dialog.native',
  'fs.dialog.accept',
  'fs.dialog.start',
  'profile.control',
  'profile.export',
]);
//=============================================================================
export function rpc<T = unknown>(
  method: string,
  params?: RpcParams,
  options?: RpcOptions
): Promise<T> {
  return withDeadline(`RPC ${method}`, (signal) => callRpc<T>(method, params, signal), options);
}
//=============================================================================
async function callRpc<T>(method: string, params: RpcParams, signal: AbortSignal): Promise<T> {
  const id = nextId++;
  const envelope = JSON.stringify({ id, method, params: params ?? {} });

  if (typeof window.webui?.call === 'function') {
    const raw = await callWithRetry(envelope, !NON_IDEMPOTENT_METHODS.has(method), signal);
    let parsed: { id?: number; result?: T; error?: { message: string } };
    try {
      parsed = JSON.parse(raw);
    } catch {
      throw new Error(`nelson-webview: malformed RPC response for "${method}"`);
    }
    if (parsed.error) throw new Error(`nelson-webview RPC "${method}": ${parsed.error.message}`);
    if (method.startsWith('figure.')) {
      const figure = parsed.result as FigureDisplayListResult | undefined;
      if (Array.isArray(figure?.primitives)) {
        return figureImagePayloads.hydrate(figure) as T;
      }
    }
    return parsed.result as T;
  }

  if (localBackend) {
    return (await localBackend.handle(
      method,
      (params ?? {}) as Record<string, unknown>,
      signal
    )) as T;
  }

  // Standalone dev: route to the in-process mock engine.
  const engine = await getMockEngine();
  return (await engine.handle(method, (params ?? {}) as Record<string, unknown>)) as T;
}
//=============================================================================
