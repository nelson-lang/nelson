//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
const RELOAD_MARKER = 'nelson-wasm-coi-reload-attempted';
function canInstallServiceWorker(): boolean {
  return window.isSecureContext && typeof navigator !== 'undefined' && 'serviceWorker' in navigator;
}
//=============================================================================
async function reloadOnce(): Promise<void> {
  if (sessionStorage.getItem(RELOAD_MARKER) === '1') {
    sessionStorage.removeItem(RELOAD_MARKER);
    return;
  }
  sessionStorage.setItem(RELOAD_MARKER, '1');
  window.location.reload();
  await new Promise<never>(() => undefined);
}
//=============================================================================
export async function ensureCrossOriginIsolation(
  serviceWorkerUrl: string,
  onStatus?: (message: string) => void
): Promise<void> {
  if (window.crossOriginIsolated) {
    sessionStorage.removeItem(RELOAD_MARKER);
    return;
  }
  if (new URLSearchParams(window.location.search).has('no-coi-sw')) return;
  if (!canInstallServiceWorker()) return;

  onStatus?.('Preparing isolated browser session');
  try {
    await navigator.serviceWorker.register(serviceWorkerUrl, { scope: './' });
    await navigator.serviceWorker.ready;
    if (!window.crossOriginIsolated) await reloadOnce();
  } catch (error) {
    console.warn('Nelson WebAssembly could not install the isolation service worker', error);
  }
}
//=============================================================================
