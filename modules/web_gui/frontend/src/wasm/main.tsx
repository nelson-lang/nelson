//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import React from 'react';
import ReactDOM from 'react-dom/client';
import { createNelsonWorkerClient, type NelsonLoadingProgress } from '@nelson/wasm-client';
import { App } from '../App';
import { installBrowserZoomLock } from '../desktop/browserZoom';
import { initializePersistentStorage } from '../desktop/persistentStorage';
import { installLocalRpcBackend } from '../rpc/client';
import { WasmDesktopBackend } from './WasmDesktopBackend';
import { ensureCrossOriginIsolation } from './crossOriginIsolation';
import {
  installOpenerRpcProxyClient,
  installOpenerRpcProxyHost,
  shouldUseOpenerRpcProxy,
} from './windowRpcProxy';
import {
  browserManifestFileNames,
  loadBrowserManifest,
  loadRuntimeCapabilities,
  startupDownloadPlan,
  type StartupDownloadPlan,
} from './runtimeManifest';
import { resolveVersionedAssetUrl, resolveWasmRuntimeUrls } from './runtimeUrls';
import { scheduleStartupUrlActions } from '../desktop/startupUrlActions';
import '../styles.css';
import '../console.css';
import '../panels.css';
import '../dialogs.css';
import '../shell.css';
import '@xterm/xterm/css/xterm.css';
import 'dockview/dist/styles/dockview.css';
//=============================================================================
const root = ReactDOM.createRoot(document.getElementById('root') as HTMLElement);
let downloadPlan: StartupDownloadPlan = {};
const bundleUrl = import.meta.url;
function browserAssetUrl(path: string): string {
  return resolveVersionedAssetUrl(path, window.location.href, bundleUrl);
}
//=============================================================================
function loadingPercentage(progress: NelsonLoadingProgress): number {
  const ratio =
    progress.total && progress.loaded !== undefined
      ? Math.max(0, Math.min(1, progress.loaded / progress.total))
      : 0;
  const wasmBytes = downloadPlan.wasmBytes ?? 0;
  const resourceBytes = downloadPlan.resourceBytes ?? 0;
  const transferBytes = wasmBytes + resourceBytes;
  if (transferBytes > 0) {
    const wasmWeight = (90 * wasmBytes) / transferBytes;
    const resourceWeight = (90 * resourceBytes) / transferBytes;
    if (progress.phase === 'wasm') return 5 + ratio * wasmWeight;
    if (progress.phase === 'resources') return 5 + wasmWeight + ratio * resourceWeight;
  }
  if (progress.phase === 'wasm') return 10 + ratio * 60;
  if (progress.phase === 'resources') return 70 + ratio * 24;
  if (progress.phase === 'ready') return 100;
  return 95;
}
//=============================================================================
function hasRuntimeResourcesGroup(deploymentManifest: {
  delivery?: { onDemand?: unknown };
}): boolean {
  const groups = deploymentManifest.delivery?.onDemand;
  if (!Array.isArray(groups)) return false;
  return groups.some((group) => {
    if (!group || typeof group !== 'object') return false;
    const candidate = group as { id?: unknown; trigger?: unknown };
    return candidate.id === 'runtime-resources' || candidate.trigger === 'runtime-resources';
  });
}
//=============================================================================
function showLoading(progress: NelsonLoadingProgress): void {
  const percentage = loadingPercentage(progress);
  root.render(
    <main className="wasm-loading" aria-busy="true">
      <div className="wasm-loading__card">
        <div className="wasm-loading__title">Nelson WebAssembly</div>
        <progress aria-label="Runtime download" max="100" value={percentage} />
        <div className="wasm-loading__status" aria-live="polite">
          {progress.label}
          {progress.total && progress.loaded !== undefined
            ? ` — ${Math.round((progress.loaded / progress.total) * 100)}%`
            : '…'}
        </div>
      </div>
    </main>
  );
}
//=============================================================================
async function start(): Promise<void> {
  installBrowserZoomLock();
  if (shouldUseOpenerRpcProxy()) {
    showLoading({
      phase: 'ready',
      label: 'Connecting to Nelson desktop window',
      loaded: 1,
      total: 1,
    });
    const removeProxy = installOpenerRpcProxyClient();
    window.addEventListener('pagehide', removeProxy, { once: true });
    await initializePersistentStorage();
    root.render(
      <React.StrictMode>
        <App />
      </React.StrictMode>
    );
    return;
  }
  showLoading({ phase: 'wasm', label: 'Preparing browser isolation', loaded: 0 });
  await ensureCrossOriginIsolation(browserAssetUrl('./nelson-coi-serviceworker.js'), (label) =>
    showLoading({ phase: 'wasm', label, loaded: 0 })
  );
  showLoading({ phase: 'wasm', label: 'Starting WebAssembly Worker', loaded: 0 });
  const manifestUrl = browserAssetUrl('./manifest.json');
  const capabilityManifestUrl = browserAssetUrl('./capabilities.json');
  const [deploymentManifest, runtimeCapabilities] = await Promise.all([
    loadBrowserManifest(manifestUrl),
    loadRuntimeCapabilities(capabilityManifestUrl),
  ]);
  const runtimeUrls = resolveWasmRuntimeUrls(
    window.location.href,
    bundleUrl,
    browserManifestFileNames(deploymentManifest)
  );
  downloadPlan = startupDownloadPlan(deploymentManifest, manifestUrl, runtimeUrls.wasmUrl);
  const client = await createNelsonWorkerClient({
    ...runtimeUrls,
    wasmBytes: downloadPlan.wasmBytes,
    runtimeCapabilities,
    onProgress: showLoading,
  });
  const nflowUrl = __NELSON_WASM_NFLOW__ ? browserAssetUrl('./nflow/index.html') : undefined;
  const examplesUrl = browserAssetUrl('./examples/index.html');
  const packageManagerUrl = __NELSON_WASM_NMM__ ? browserAssetUrl('./nmm/index.html') : undefined;
  const runtimeResourcesUrl = hasRuntimeResourcesGroup(deploymentManifest)
    ? browserAssetUrl('./runtime-resources/index.json')
    : undefined;
  const removeBackend = installLocalRpcBackend(
    new WasmDesktopBackend(client, {
      nflowUrl,
      packageManagerUrl,
      examplesUrl,
      runtimeResourcesUrl,
      runtimeCapabilities,
      deploymentManifest,
      downloadPlan,
      openExternal: (url) => window.open(url, '_blank', 'noopener'),
    })
  );
  const removeProxyHost = installOpenerRpcProxyHost();
  // Other pages (NFlow editor, satellite windows) may run on this page's
  // engine: closing it would drop their base workspace, so ask first.
  window.addEventListener('beforeunload', (event) => {
    if (client.role === 'owner' && client.peers > 0) {
      event.preventDefault();
      event.returnValue = '';
    }
  });
  window.addEventListener('pagehide', removeBackend, { once: true });
  window.addEventListener('pagehide', removeProxyHost, { once: true });
  await initializePersistentStorage();
  root.render(
    <React.StrictMode>
      <App />
    </React.StrictMode>
  );
  scheduleStartupUrlActions(window.location.href);
}
//=============================================================================
showLoading({ phase: 'wasm', label: 'Loading browser interface', loaded: 0 });
void start().catch((error: unknown) => {
  const message =
    error instanceof Error
      ? error.message
      : typeof error === 'object'
      ? JSON.stringify(error)
      : String(error);
  root.render(
    <main className="wasm-start-error">
      <h1>Nelson WebAssembly could not start</h1>
      <pre>{message}</pre>
    </main>
  );
});
//=============================================================================
