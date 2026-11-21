//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Live backend for a notebook figure output. It carries the figure RPC envelope
// over the host's renderer messaging channel: an outgoing {id,method,params} is
// wrapped as {channel,outputId,handle,body:{rpc}} and posted; an incoming
// envelope the renderer owns is unwrapped and its body.rpc delivered to the
// shared RPC client (responses matched by id, pushes re-emitted by handle).
//
// One module instance serves every figure output of a notebook, so the backend
// is installed once and all outputs share it. Responses are correlated by the
// request id; pushes are re-emitted locally and each figure panel keeps only the
// frames for its own handle. When no live kernel backs an output the renderer
// never reaches this module and stays static.
//=============================================================================
import { emitLocalPush, installLocalRpcBackend, type LocalRpcBackend } from '../rpc/client';
//=============================================================================
export const BRIDGE_CHANNEL = 'nelson.figure';
export const BRIDGE_PROTOCOL = 1;
//=============================================================================
// The subset of the host renderer messaging surface this bridge needs. A VS Code
// NotebookRenderer hands these on the activate() context when messaging is on.
export interface RendererMessaging {
  postMessage?(message: unknown): void;
  onDidReceiveMessage?(listener: (message: unknown) => void): { dispose(): void } | void;
}
//=============================================================================
// The extension's reply to a renderer going live.
export interface InitResult {
  live: boolean;
  capabilities: Record<string, unknown>;
}
//=============================================================================
interface OutputSession {
  handle: number;
  onInit: (result: InitResult) => void;
  onClosed?: () => void;
}
//=============================================================================
interface Envelope {
  outputId: string | null;
  handle: number;
  body: unknown;
}
//=============================================================================
// Kernel pushes the renderer applies. figure.frame carries a fresh display list;
// the others ask the panel to re-fetch, retitle or go static.
const PUSH_CHANNELS = new Set(['figure.frame', 'figure.updated', 'figure.title', 'figure.closed']);
//=============================================================================
export class NotebookFigureBridge {
  private readonly pending = new Map<
    number,
    { resolve(v: unknown): void; reject(e: Error): void }
  >();
  private readonly sessions = new Map<string, OutputSession>();
  private nextRpcId = 1;
  private uninstallBackend: (() => void) | null = null;
  private subscription: { dispose(): void } | null = null;

  constructor(private readonly messaging: RendererMessaging) {
    const disposable = messaging.onDidReceiveMessage?.((message) => this.onMessage(message));
    this.subscription = disposable ?? null;
    const backend: LocalRpcBackend = {
      handle: (method, params) => this.sendRpc(method, params),
    };
    this.uninstallBackend = installLocalRpcBackend(backend);
  }

  // True when the host actually gave a two-way channel. Without one the renderer
  // can only stay static.
  canGoLive(): boolean {
    return typeof this.messaging.postMessage === 'function';
  }

  // Announce a live-capable output and resolve with the extension's init reply.
  // The caller mounts the interactive panel only when the reply says live.
  requestLive(outputId: string, handle: number, onClosed: () => void): Promise<InitResult> {
    return new Promise<InitResult>((resolve) => {
      this.sessions.set(outputId, { handle, onInit: resolve, onClosed });
      this.post(outputId, handle, { control: 'ready', protocol: BRIDGE_PROTOCOL });
    });
  }

  // Release an output: tell the extension and drop its session. If a live request
  // was still in flight (the output was replaced mid-handshake, e.g. a slider
  // re-run), settle it as not-live so the awaiting caller never hangs. Resolving
  // an already-resolved request is a no-op.
  bye(outputId: string): void {
    const session = this.sessions.get(outputId);
    if (!session) return;
    this.sessions.delete(outputId);
    session.onInit({ live: false, capabilities: {} });
    this.post(outputId, session.handle, { control: 'bye' });
  }

  dispose(): void {
    for (const outputId of [...this.sessions.keys()]) this.bye(outputId);
    for (const entry of this.pending.values()) {
      entry.reject(new Error('Nelson figure: the notebook renderer was disposed.'));
    }
    this.pending.clear();
    this.subscription?.dispose();
    this.subscription = null;
    this.uninstallBackend?.();
    this.uninstallBackend = null;
  }

  // Forward one figure RPC to the kernel and await its response. The id is the
  // bridge's own; the extension echoes it so the response is matched here. The
  // handle rides in params, as the kernel addresses figures by handle.
  private sendRpc(method: string, params: Record<string, unknown>): Promise<unknown> {
    const id = this.nextRpcId++;
    return new Promise<unknown>((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.post(null, handleOf(params), { rpc: { id, method, params } });
    });
  }

  private onMessage(message: unknown): void {
    const envelope = asEnvelope(message);
    if (!envelope) return;
    const body = envelope.body as Record<string, unknown> | null;
    if (!body || typeof body !== 'object') return;

    if (body.control === 'init') {
      this.onInit(envelope, body);
      return;
    }

    const rpc = body.rpc as Record<string, unknown> | undefined;
    if (!rpc || typeof rpc !== 'object') return;

    if (typeof rpc.channel === 'string') {
      this.onPush(rpc.channel, rpc.payload, envelope.handle);
      return;
    }
    if (typeof rpc.id === 'number') {
      this.onResponse(rpc.id, rpc);
    }
  }

  // init is targeted at the output that announced itself.
  private onInit(envelope: Envelope, body: Record<string, unknown>): void {
    const session = envelope.outputId != null ? this.sessions.get(envelope.outputId) : undefined;
    if (!session) return;
    const capabilities =
      body.capabilities && typeof body.capabilities === 'object'
        ? (body.capabilities as Record<string, unknown>)
        : {};
    session.onInit({ live: body.live === true, capabilities });
  }

  private onResponse(id: number, rpc: Record<string, unknown>): void {
    const entry = this.pending.get(id);
    if (!entry) return;
    this.pending.delete(id);
    const error = rpc.error as { message?: string } | undefined;
    if (error) entry.reject(new Error(error.message ?? 'Nelson figure RPC failed.'));
    else entry.resolve(rpc.result);
  }

  // Pushes are broadcast by handle. They are re-emitted on the shared push bus so
  // the figure panel with the matching handle applies them; a figure.closed also
  // drops the output back to static.
  private onPush(channel: string, payload: unknown, handle: number): void {
    if (!PUSH_CHANNELS.has(channel)) return;
    emitLocalPush(channel, payload);
    if (channel === 'figure.closed') {
      const closed = handleOf(payload) || handle;
      for (const session of this.sessions.values()) {
        if (session.handle === closed) session.onClosed?.();
      }
    }
  }

  private post(outputId: string | null, handle: number, body: unknown): void {
    this.messaging.postMessage?.({ channel: BRIDGE_CHANNEL, outputId, handle, body });
  }
}
//=============================================================================
function asEnvelope(message: unknown): Envelope | null {
  if (!message || typeof message !== 'object') return null;
  const m = message as Record<string, unknown>;
  if (m.channel !== BRIDGE_CHANNEL) return null;
  return {
    outputId: typeof m.outputId === 'string' ? m.outputId : null,
    handle: typeof m.handle === 'number' ? m.handle : 0,
    body: m.body,
  };
}
//=============================================================================
function handleOf(value: unknown): number {
  if (value && typeof value === 'object') {
    const handle = (value as { handle?: unknown }).handle;
    if (typeof handle === 'number') return handle;
  }
  return 0;
}
//=============================================================================
