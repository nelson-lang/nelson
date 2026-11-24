//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  emitLocalPushValue,
  installLocalRpcBackend,
  onAnyPush,
  rpc,
  type LocalRpcBackend,
} from '../rpc/client';
//=============================================================================
const REQUEST = 'nelson.rpc.proxy.request';
const RESPONSE = 'nelson.rpc.proxy.response';
const READY = 'nelson.rpc.proxy.ready';
const PUSH = 'nelson.rpc.proxy.push';
type RpcRequestMessage = {
  type: typeof REQUEST;
  id: number;
  method: string;
  params: Record<string, unknown>;
};
//=============================================================================
type RpcResponseMessage = {
  type: typeof RESPONSE;
  id: number;
  result?: unknown;
  error?: string;
};
//=============================================================================
type RpcPushMessage = {
  type: typeof PUSH;
  channel: string;
  payload: unknown;
};
//=============================================================================
function sameOrigin(event: MessageEvent): boolean {
  return event.origin === window.location.origin;
}
//=============================================================================
function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null;
}
//=============================================================================
function post(target: Window, message: unknown): void {
  target.postMessage(message, window.location.origin);
}
//=============================================================================
export function shouldUseOpenerRpcProxy(): boolean {
  return (
    new URLSearchParams(window.location.search).has('figure') &&
    window.opener !== null &&
    window.opener !== window
  );
}
//=============================================================================
export function installOpenerRpcProxyClient(): () => void {
  const opener = window.opener;
  if (!opener) return () => undefined;
  let nextId = 1;
  const pending = new Map<
    number,
    { resolve: (value: unknown) => void; reject: (error: Error) => void }
  >();
  const onMessage = (event: MessageEvent) => {
    if (!sameOrigin(event) || !isObject(event.data)) return;
    if (event.data.type === RESPONSE && typeof event.data.id === 'number') {
      const response = event.data as RpcResponseMessage;
      const entry = pending.get(response.id);
      if (!entry) return;
      pending.delete(response.id);
      if (typeof response.error === 'string') {
        entry.reject(new Error(response.error));
      } else {
        entry.resolve(response.result);
      }
    } else if (event.data.type === PUSH && typeof event.data.channel === 'string') {
      const push = event.data as RpcPushMessage;
      emitLocalPushValue(push.channel, push.payload);
    }
  };
  window.addEventListener('message', onMessage);
  const backend: LocalRpcBackend = {
    handle(method, params, signal) {
      const id = nextId++;
      return new Promise((resolve, reject) => {
        pending.set(id, { resolve, reject });
        const abort = () => {
          pending.delete(id);
          reject(new Error(`RPC ${method} aborted`));
        };
        if (signal.aborted) {
          abort();
          return;
        }
        signal.addEventListener('abort', abort, { once: true });
        post(opener, { type: REQUEST, id, method, params });
      });
    },
  };
  const removeBackend = installLocalRpcBackend(backend);
  post(opener, { type: READY });
  return () => {
    window.removeEventListener('message', onMessage);
    removeBackend();
    for (const [id, entry] of pending) {
      pending.delete(id);
      entry.reject(new Error('RPC proxy window closed'));
    }
  };
}
//=============================================================================
export function installOpenerRpcProxyHost(): () => void {
  const clients = new Set<Window>();
  const onMessage = (event: MessageEvent) => {
    if (!sameOrigin(event) || !event.source || !isObject(event.data)) return;
    const source = event.source as Window;
    if (event.data.type === READY) {
      clients.add(source);
      return;
    }
    if (
      event.data.type !== REQUEST ||
      typeof event.data.id !== 'number' ||
      typeof event.data.method !== 'string'
    ) {
      return;
    }
    const request = event.data as RpcRequestMessage;
    clients.add(source);
    rpc(request.method, request.params)
      .then((result) => {
        post(source, { type: RESPONSE, id: request.id, result });
      })
      .catch((error: unknown) => {
        const message = error instanceof Error ? error.message : String(error);
        post(source, { type: RESPONSE, id: request.id, error: message });
      });
  };
  const offPush = onAnyPush((channel, payload) => {
    for (const client of clients) {
      post(client, { type: PUSH, channel, payload });
    }
  });
  window.addEventListener('message', onMessage);
  return () => {
    window.removeEventListener('message', onMessage);
    offPush();
    clients.clear();
  };
}
//=============================================================================
