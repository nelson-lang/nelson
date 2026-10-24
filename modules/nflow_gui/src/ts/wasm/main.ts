//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createNelsonWorkerClient, type NelsonLoadingProgress } from '@nelson/wasm-client';
import { installNFlowPushBridge, installWasmWorkerHost } from '../app/native-rpc.js';
import { OPEN_MODEL_PATH_EVENT } from '../app/components/modal-events.js';
import type { NFlowWasmExamplesProvider } from '../app/nflow-host.js';
import { createFmuProvider } from './fmu.js';
//=============================================================================
// The packaged bundle versions every runtime asset with `?build=<id>`. Derive it
// like the desktop does so both pages request identical URLs: the SharedWorker
// relay is identified by its script URL, and a different query string would
// start a second relay and a second engine.
function buildId(): string {
  const candidates = [
    new URL(import.meta.url).searchParams.get('build'),
    new URL(window.location.href).searchParams.get('build'),
    ...Array.from(document.scripts, (script) =>
      script.src ? new URL(script.src, window.location.href).searchParams.get('build') : null
    ),
  ];
  return candidates.find((value): value is string => Boolean(value)) ?? '';
}
//=============================================================================
function runtimeUrl(name: string): string {
  const url = new URL(`../${name}`, window.location.href);
  const build = buildId();
  if (build && !url.searchParams.has('build')) url.searchParams.set('build', build);
  return url.href;
}
//=============================================================================
async function loadRuntimeCapabilities(): Promise<unknown> {
  const response = await fetch(runtimeUrl('capabilities.json'));
  if (!response.ok) {
    throw new Error(`capability manifest failed: HTTP ${response.status}`);
  }
  return response.json();
}
//=============================================================================
function createExamplesProvider(): NFlowWasmExamplesProvider {
  const prefix = 'nflow-example:/';
  return {
    async listExamples(): Promise<unknown> {
      const response = await fetch(new URL('./examples/catalog.json', window.location.href));
      if (!response.ok) throw new Error(`NFlow examples catalog failed: HTTP ${response.status}`);
      return response.json();
    },
    async readText(path: string): Promise<string> {
      if (!path.startsWith(prefix)) throw new Error('NFlow example path is not available');
      const relativePath = path.slice(prefix.length);
      const segments = relativePath.split('/');
      if (
        !(relativePath.endsWith('.nflow') || relativePath.endsWith('.m')) ||
        segments.some((segment) => !segment || segment === '.' || segment === '..')
      ) {
        throw new Error('Invalid NFlow example path');
      }
      const encodedPath = segments.map(encodeURIComponent).join('/');
      const response = await fetch(
        new URL(`./examples/files/${encodedPath}`, window.location.href)
      );
      if (!response.ok) throw new Error(`NFlow example failed: HTTP ${response.status}`);
      return response.text();
    },
  };
}
//=============================================================================
// When this page owns the engine other pages depend on, closing it would
// drop their base workspace: ask for confirmation first. A relayed page that
// loses its owner gets a fresh engine; say so because variables are gone.
function installSharedEngineGuards(client: {
  readonly role: string;
  readonly peers: number;
  onEngineRestart(handler: (event: { reason?: string }) => void): () => void;
}): void {
  window.addEventListener('beforeunload', (event) => {
    if (client.role === 'owner' && client.peers > 0) {
      event.preventDefault();
      event.returnValue = '';
    }
  });
  client.onEngineRestart((event) => {
    if (event.reason === 'owner-lost' || event.reason === 'owner-changed') {
      console.warn(
        'Nelson engine restarted: the page that owned it was closed, the base workspace is empty.'
      );
    }
  });
}
//=============================================================================
async function start(): Promise<void> {
  const showProgress = (progress: NelsonLoadingProgress) => {
    const status = document.getElementById('nf-loading-status');
    const bar = document.querySelector<HTMLProgressElement>('#nf-loading-progress');
    const ratio =
      progress.total && progress.loaded !== undefined
        ? Math.max(0, Math.min(1, progress.loaded / progress.total))
        : 0;
    const value =
      progress.phase === 'wasm'
        ? 10 + ratio * 60
        : progress.phase === 'resources'
        ? 70 + ratio * 24
        : progress.phase === 'ready'
        ? 100
        : 95;
    if (bar) bar.value = value;
    if (status) status.textContent = progress.label;
  };
  const runtimeCapabilities = await loadRuntimeCapabilities();
  // Share the desktop's engine (same base workspace, same virtual filesystem)
  // through the origin-wide SharedWorker; the client falls back to a dedicated
  // Worker when the browser or the page (`?sharedEngine=0`) rules it out.
  const sharedEngine = new URL(window.location.href).searchParams.get('sharedEngine');
  const useSharedEngine =
    sharedEngine === null ||
    !['0', 'false', 'off', 'no', 'dedicated'].includes(sharedEngine.toLowerCase());
  const client = await createNelsonWorkerClient({
    workerUrl: runtimeUrl('nelson-worker.mjs'),
    ...(useSharedEngine ? { sharedWorkerUrl: runtimeUrl('nelson-shared-worker.mjs') } : {}),
    moduleUrl: runtimeUrl('nelson-portable.mjs'),
    wasmUrl: runtimeUrl('nelson-portable.wasm'),
    runtimeCapabilities,
    onProgress: showProgress,
  });
  installNFlowPushBridge();
  const removeHost = installWasmWorkerHost(
    client,
    createExamplesProvider(),
    (url) => {
      // Some embedded browsers return null even after opening the tab; that is
      // not an error.
      window.open(url, '_blank', 'noopener,noreferrer');
    },
    createFmuProvider(client),
    (path) => {
      document.dispatchEvent(
        new CustomEvent(OPEN_MODEL_PATH_EVENT, { cancelable: true, detail: { path } })
      );
    }
  );
  installSharedEngineGuards(client);
  window.addEventListener(
    'pagehide',
    () => {
      removeHost();
      client.terminate();
    },
    { once: true }
  );
  await import('../app/app.js');
  installDesktopModelRequests();
}
//=============================================================================
// `open_system(path)` evaluated in the desktop Command Window reaches this
// page either through the origin-wide channel (page already open) or through
// the `?model=` query of a freshly opened page. Paths are engine paths; the
// host maps packaged example paths when it reads them.
const NFLOW_MODEL_CHANNEL = 'nelson-wasm-nflow';
function openModelInEditor(path: string): void {
  document.dispatchEvent(
    new CustomEvent(OPEN_MODEL_PATH_EVENT, { cancelable: true, detail: { path } })
  );
}
//=============================================================================
function installDesktopModelRequests(): void {
  if (typeof BroadcastChannel === 'function') {
    const channel = new BroadcastChannel(NFLOW_MODEL_CHANNEL);
    channel.onmessage = (event: MessageEvent) => {
      const data = event.data as { type?: string; path?: string; token?: string } | null;
      if (data?.type !== 'open-model' || typeof data.path !== 'string') return;
      channel.postMessage({ type: 'open-model-ack', token: data.token });
      openModelInEditor(data.path);
      window.focus?.();
    };
    window.addEventListener('pagehide', () => channel.close(), { once: true });
  }
  const requested = new URL(window.location.href).searchParams.get('model');
  if (requested) openModelInEditor(requested);
}
//=============================================================================
void start().catch((error: unknown) => {
  const message = error instanceof Error ? error.message : String(error);
  const status = document.getElementById('nf-loading-status');
  if (status) status.textContent = `WebAssembly startup failed: ${message}`;
  document.documentElement.classList.add('nf-startup-failed');
});
//=============================================================================
