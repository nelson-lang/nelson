//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export interface BrowserRuntimeDiagnostics {
  userAgent?: string;
  platform?: string;
  features: Record<string, boolean>;
  unsupported: string[];
  notes: string[];
}
//=============================================================================
interface BrowserCapabilityEnvironment {
  Atomics?: unknown;
  AudioContext?: unknown;
  FileReader?: unknown;
  SharedArrayBuffer?: unknown;
  WebAssembly?: unknown;
  caches?: unknown;
  crossOriginIsolated?: boolean;
  document?: {
    createElement?: (tagName: string) => unknown;
  };
  isSecureContext?: boolean;
  navigator?: {
    userAgent?: string;
    userAgentData?: { platform?: string };
    platform?: string;
    serviceWorker?: unknown;
    mediaDevices?: { getUserMedia?: unknown };
    gpu?: unknown;
  };
  showDirectoryPicker?: unknown;
  showOpenFilePicker?: unknown;
  showSaveFilePicker?: unknown;
  webkitAudioContext?: unknown;
}
//=============================================================================
function hasFunction(value: unknown): boolean {
  return typeof value === 'function';
}
//=============================================================================
function wasmApi(env: BrowserCapabilityEnvironment): Record<string, unknown> {
  return env.WebAssembly && typeof env.WebAssembly === 'object'
    ? (env.WebAssembly as Record<string, unknown>)
    : {};
}
//=============================================================================
function hasWebGlContext(env: BrowserCapabilityEnvironment, name: 'webgl' | 'webgl2'): boolean {
  try {
    const canvas = env.document?.createElement?.('canvas') as
      | { getContext?: (contextId: string) => unknown }
      | undefined;
    return !!canvas?.getContext?.(name);
  } catch {
    return false;
  }
}
//=============================================================================
export function detectBrowserRuntimeDiagnostics(
  env: BrowserCapabilityEnvironment = globalThis as BrowserCapabilityEnvironment
): BrowserRuntimeDiagnostics {
  const wasm = wasmApi(env);
  const fileOpen = hasFunction(env.showOpenFilePicker);
  const fileSave = hasFunction(env.showSaveFilePicker);
  const directoryOpen = hasFunction(env.showDirectoryPicker);
  const secureContext = env.isSecureContext !== false;
  const crossOriginIsolated = env.crossOriginIsolated === true;
  const sharedArrayBuffer = hasFunction(env.SharedArrayBuffer);
  const atomics = !!env.Atomics;
  const wasmCore = hasFunction(wasm.instantiate) && hasFunction(wasm.validate);
  const wasmExceptions = hasFunction(wasm.Exception) && hasFunction(wasm.Tag);
  const jspi = hasFunction(wasm.promising) && hasFunction(wasm.Suspending);
  const serviceWorker = !!env.navigator?.serviceWorker;
  const cacheStorage = !!env.caches;
  const webAudio = hasFunction(env.AudioContext) || hasFunction(env.webkitAudioContext);
  const microphone = hasFunction(env.navigator?.mediaDevices?.getUserMedia);
  const fileReader = hasFunction(env.FileReader);
  const webGpu = !!env.navigator?.gpu;
  const features: Record<string, boolean> = {
    cacheStorage,
    crossOriginIsolated,
    dragAndDropFiles: fileReader,
    fileSystemAccess: fileOpen && fileSave,
    localDirectoryAccess: directoryOpen,
    microphone,
    pwaInstallSupport: serviceWorker && cacheStorage,
    secureContext,
    serviceWorker,
    sharedArrayBuffer,
    wasmCore,
    wasmExceptions,
    wasmJspi: jspi,
    wasmThreads: crossOriginIsolated && sharedArrayBuffer && atomics,
    webAudio,
    webGl: hasWebGlContext(env, 'webgl'),
    webGl2: hasWebGlContext(env, 'webgl2'),
    webGpu,
  };
  const unsupported = Object.entries(features)
    .filter(([, value]) => !value)
    .map(([name]) => name)
    .sort();
  const notes: string[] = [];
  if (!features.wasmThreads) {
    notes.push('Wasm threads require SharedArrayBuffer, Atomics and cross-origin isolation.');
  }
  if (!features.wasmJspi) {
    notes.push('JSPI-dependent blocking input must stay disabled or use a fallback.');
  }
  if (!features.fileSystemAccess) {
    notes.push('Use drag-and-drop or file input fallback for local files in this browser.');
  }
  return {
    userAgent: env.navigator?.userAgent,
    platform: env.navigator?.userAgentData?.platform ?? env.navigator?.platform,
    features,
    unsupported,
    notes,
  };
}
//=============================================================================
