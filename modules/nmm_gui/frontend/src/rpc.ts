//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Package manager window transport. The backend binds "nmmGuiRpc" (WebUI)
// and answers one JSON envelope per call: { id, method, params } ->
// { id, result } | { id, error }.
declare global {
  interface Window {
    nmmGuiRpc?: (payload: string) => Promise<string>;
    nmmGuiPush?: (channel: string, payload: string) => void;
    nmmGuiReload?: () => void;
  }
}
//=============================================================================
// The WebUI bridge of the window page. Read through a local view of window:
// the web desktop that embeds this page declares the global with its own type.
interface WebUiBridge {
  call(name: string, payload: string): Promise<string>;
  isConnected?: () => boolean;
}
//=============================================================================
function webuiBridge(): WebUiBridge | undefined {
  return (window as unknown as { webui?: WebUiBridge }).webui;
}
//=============================================================================
export type Json = any;
let rpcId = 0;
// The page normally talks to its WebUI window. Embedded in the web desktop
// (see embed.tsx) the host injects a transport instead and no window exists.
export type RpcTransport = (method: string, params: Record<string, unknown>) => Promise<Json>;
let transport: RpcTransport | null = null;
let embedded = false;
export function setRpcTransport(next: RpcTransport | null): void {
  transport = next;
}
//=============================================================================
export function setEmbedded(value: boolean): void {
  embedded = value;
}
//=============================================================================
export function isEmbedded(): boolean {
  return embedded;
}
//=============================================================================
function delay(ms: number): Promise<void> {
  return new Promise(resolve => window.setTimeout(resolve, ms));
}
//=============================================================================
function transportReady(): boolean {
  const webui = webuiBridge();
  if (typeof webui?.call === 'function') {
    return typeof webui.isConnected !== 'function' || webui.isConnected();
  }
  return typeof window.nmmGuiRpc === 'function';
}
//=============================================================================
async function waitForTransport(): Promise<void> {
  for (let attempt = 0; attempt < 50; attempt += 1) {
    if (transportReady()) return;
    await delay(100);
  }
  throw new Error('Native bridge unavailable');
}
//=============================================================================
async function callTransport(payload: string): Promise<string> {
  const webui = webuiBridge();
  if (typeof webui?.call === 'function') return webui.call('nmmGuiRpc', payload);
  return window.nmmGuiRpc!(payload);
}
//=============================================================================
// Results are loosely typed JSON from the Nelson facade.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export async function rpc(method: string, params: Record<string, unknown> = {}): Promise<Json> {
  if (transport) return transport(method, params);
  await waitForTransport();
  const raw = await callTransport(JSON.stringify({ id: ++rpcId, method, params }));
  const response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  if (response.error) throw new Error(response.error.message || 'Request failed');
  return response.result;
}
//=============================================================================
