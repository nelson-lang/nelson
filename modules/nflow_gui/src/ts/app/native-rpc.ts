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
import { abortableSleep, withDeadline } from '../utils/deadline.js';
import type { DeadlineOptions } from '../utils/deadline.js';
import { NativeWebUiHost, WasmWorkerHost, NFlowWasmModelOpener } from './nflow-host.js';
import type {
  NativeWebUiGlobal,
  NFlowHost,
  NFlowWasmHelpOpener,
  NFlowWasmExamplesProvider,
  NFlowWasmWorkerClient,
  NFlowWasmFmuProvider,
} from './nflow-host.js';
//=============================================================================
export type NFlowRpcParams = Record<string, unknown> | undefined;
// `signal` / `timeoutMs` bound how long the caller waits (transport wait,
// retries and the call itself); the engine is not told to cancel.
export type NFlowRpcOptions = DeadlineOptions;
export interface NFlowRpcEnvelope {
  id: number;
  method: string;
  params: Record<string, unknown>;
}
//=============================================================================
type WebUiRpcFunction = (payload: string) => Promise<string> | string;
type PushHandler = (payload: unknown) => void;
type PushHandlerMap = Map<string, Set<PushHandler>>;
declare global {
  interface Window {
    webui?: NativeWebUiGlobal['webui'];
    nflow_rpc?: WebUiRpcFunction;
    rpc?: WebUiRpcFunction;
    __nelsonPush?: (channel: string, payloadJson: string) => void;
    __nflowHasPushHandlers?: (channel: string) => boolean;
    __nflowPushHandlers?: PushHandlerMap;
  }
}
//=============================================================================
let nextId = 1;
let configuredHost: NFlowHost | undefined;
// A signal that never aborts, for the window.nflow_rpc bridge: the calling
// library already wraps its request in its own deadline.
const NEVER_ABORT_SIGNAL = new AbortController().signal;
const NON_IDEMPOTENT_METHODS = new Set<string>([
  'nflow.file.writeText',
  'nflow.file.readText',
  'nflow.file.loadInitial',
  'nflow.sim.run',
  'nflow.sim.pause',
  'nflow.sim.stop',
  'nflow.sim.event',
  'nflow.callback.invoke',
  'nflow.help.open',
  'nflow.library.load',
  'nflow.library.unload',
  'nflow.library.getSearchPaths',
  'nflow.library.setSearchPaths',
  'nflow.codegen.generateToDir',
  'nflow.codegen.generateBlockToDir',
  'nflow.fmi3.inspect',
  'nflow.fmu.pick',
  'nflow.ssp.import',
  'nflow.ssp.export',
  'nflow.fmu.export',
  'nflow.clipboard.set',
  'nflow.blob.release',
  'nflow.fs.dialogAccept',
  'nflow.fs.dialogStart',
]);
//=============================================================================
function host(): (typeof globalThis & Window) | any {
  return typeof window !== 'undefined' ? window : globalThis;
}
//=============================================================================
const nativeHost = new NativeWebUiHost(() => host());
function activeHost(): NFlowHost {
  return configuredHost ?? nativeHost;
}
//=============================================================================
function pushHandlerMap(): PushHandlerMap {
  const h = host();
  if (!h.__nflowPushHandlers) h.__nflowPushHandlers = new Map<string, Set<PushHandler>>();
  return h.__nflowPushHandlers;
}
//=============================================================================
function hasRpcTransport(): boolean {
  return activeHost().isConnected();
}
//=============================================================================
async function callBackendRpc(envelope: string, signal: AbortSignal): Promise<string> {
  return activeHost().call(envelope, signal);
}
//=============================================================================
async function callWithRetry(
  envelope: string,
  allowRetry: boolean,
  signal: AbortSignal
): Promise<string> {
  const maxAttempts = 50;
  for (let attempt = 0; ; attempt++) {
    try {
      if (!isNFlowRpcConnected()) {
        if (attempt < maxAttempts) {
          await abortableSleep(200, signal);
          continue;
        }
        throw new Error('WebUI RPC transport is not connected');
      }
      return await callBackendRpc(envelope, signal);
    } catch (err) {
      if (allowRetry && attempt < maxAttempts && !signal.aborted) {
        await abortableSleep(200, signal);
        continue;
      }
      throw err;
    }
  }
}
//=============================================================================
async function waitForWebUiTransport(signal: AbortSignal): Promise<boolean> {
  const maxAttempts = 50;
  for (let attempt = 0; attempt < maxAttempts; attempt += 1) {
    if (hasRpcTransport()) return true;
    await abortableSleep(200, signal);
  }
  return hasRpcTransport();
}
//=============================================================================
async function readBlobIfNeeded(value: unknown): Promise<unknown> {
  if (!value || typeof value !== 'object') return value;
  const blob = value as { __nflowBlob?: boolean; blobId?: string; size?: number };
  if (blob.__nflowBlob !== true || typeof blob.blobId !== 'string') return value;

  let offset = 0;
  let data = '';
  const size = Number(blob.size) || 0;
  while (offset < size) {
    const chunk = await nflowRpc<{ data: string; nextOffset: number }>('nflow.blob.readChunk', {
      blobId: blob.blobId,
      offset,
    });
    data += chunk.data || '';
    offset = Number(chunk.nextOffset);
    if (!Number.isFinite(offset) || offset <= data.length - (chunk.data || '').length) break;
  }
  await nflowRpc('nflow.blob.release', { blobId: blob.blobId }).catch(() => undefined);
  return data;
}
//=============================================================================
export function isNFlowRpcConnected(): boolean {
  return activeHost().isConnected();
}
//=============================================================================
export function canUseNFlowRpc(method: string): boolean {
  return activeHost().canCall(method);
}
//=============================================================================
export function setNFlowHost(value: NFlowHost | undefined): () => void {
  const previous = configuredHost;
  configuredHost = value;
  return () => {
    configuredHost = previous;
  };
}
//=============================================================================
export function installWasmWorkerHost(
  client: NFlowWasmWorkerClient,
  examples?: NFlowWasmExamplesProvider,
  openHelp?: NFlowWasmHelpOpener,
  fmus?: NFlowWasmFmuProvider,
  openModel?: NFlowWasmModelOpener
): () => void {
  const wasmHost = new WasmWorkerHost(client, examples, openHelp, fmus, openModel);
  const removePartialListener = client.onNFlowPartial?.((partial) => {
    emitNFlowPush('nflow.sim.partial', partial);
  });
  const restore = setNFlowHost(wasmHost);
  // Dynamically imported block libraries (e.g. libraries/dashboard/blocks.js) are
  // separate bundles with their own native-rpc instance and no configured host.
  // They reach the engine through window.nflow_rpc, so expose this host there.
  const g = host() as NativeWebUiGlobal;
  const previousBridge = g.nflow_rpc;
  const bridge = (envelope: string): Promise<string> => wasmHost.call(envelope, NEVER_ABORT_SIGNAL);
  g.nflow_rpc = bridge;
  return () => {
    removePartialListener?.();
    wasmHost.disconnect();
    if (g.nflow_rpc === bridge) g.nflow_rpc = previousBridge;
    restore();
  };
}
//=============================================================================
export function nflowRpc<T = unknown>(
  method: string,
  params?: NFlowRpcParams,
  options?: NFlowRpcOptions
): Promise<T> {
  return withDeadline(
    `RPC ${method}`,
    (signal) => callNFlowRpc<T>(method, params, signal),
    options
  );
}
//=============================================================================
async function callNFlowRpc<T>(
  method: string,
  params: NFlowRpcParams,
  signal: AbortSignal
): Promise<T> {
  const rpcParams = params ?? {};

  if (!hasRpcTransport()) {
    await waitForWebUiTransport(signal);
  }

  if (hasRpcTransport()) {
    const envelope = JSON.stringify({
      id: nextId++,
      method,
      params: rpcParams,
    } satisfies NFlowRpcEnvelope);

    const raw = await callWithRetry(envelope, !NON_IDEMPOTENT_METHODS.has(method), signal);
    let parsed: { result?: unknown; error?: { message?: string } };
    try {
      parsed = JSON.parse(raw) as typeof parsed;
    } catch (error) {
      const tail = raw.slice(-80).replace(/[\r\n]/g, ' ');
      throw new SyntaxError(
        `${String(error)} (RPC ${method}, ${raw.length} bytes, tail: ${JSON.stringify(tail)})`
      );
    }
    if (parsed.error) throw new Error(parsed.error.message || 'native RPC error');
    return (await readBlobIfNeeded(parsed.result)) as T;
  }

  throw new Error(`nflow RPC backend is not available for "${method}"`);
}
//=============================================================================
export function onNFlowPush(channel: string, cb: PushHandler): () => void {
  const pushHandlers = pushHandlerMap();
  let handlers = pushHandlers.get(channel);
  if (!handlers) {
    handlers = new Set();
    pushHandlers.set(channel, handlers);
  }
  handlers.add(cb);
  return () => handlers!.delete(cb);
}
//=============================================================================
export function emitNFlowPush(channel: string, payload: unknown): void {
  host().__nelsonPush?.(channel, JSON.stringify(payload));
}
//=============================================================================
export function installNFlowPushBridge(): void {
  host().__nflowHasPushHandlers = (channel: string) => {
    const handlers = pushHandlerMap().get(channel);
    return !!handlers && handlers.size > 0;
  };
  host().__nelsonPush = (channel: string, payloadJson: string) => {
    const handlers = pushHandlerMap().get(channel);
    if (!handlers || handlers.size === 0) return;
    let payload: unknown = payloadJson;
    try {
      payload = JSON.parse(payloadJson);
    } catch {
      // Keep raw string payload.
    }
    for (const handler of handlers) handler(payload);
  };
}
//=============================================================================
installNFlowPushBridge();
//=============================================================================
