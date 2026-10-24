//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export interface WasmRuntimeUrls {
  moduleUrl: string;
  wasmUrl: string;
  workerUrl: string;
  /** SharedWorker broker; undefined when `?sharedEngine=0` opts out. */
  sharedWorkerUrl?: string;
}
//=============================================================================
type WebAssemblyWithJspi = typeof WebAssembly & {
  promising?: unknown;
  Suspending?: unknown;
};
//=============================================================================
export function supportsWasmJspi(): boolean {
  const wasm = WebAssembly as WebAssemblyWithJspi;
  return typeof wasm.Suspending === 'function' && typeof wasm.promising === 'function';
}
//=============================================================================
function normalizeEngineName(value: string | null): 'jspi' | 'classic' | null {
  if (value === 'jspi' || value === 'classic') return value;
  return null;
}
//=============================================================================
function hasPackagedDualEngine(files: readonly string[] | undefined): boolean {
  if (!files) return false;
  const packaged = new Set(files);
  return (
    packaged.has('engine/jspi/nelson-portable.mjs') &&
    packaged.has('engine/jspi/nelson-portable.wasm') &&
    packaged.has('engine/classic/nelson-portable.mjs') &&
    packaged.has('engine/classic/nelson-portable.wasm')
  );
}
//=============================================================================
function defaultEnginePath(page: URL, packagedFiles: readonly string[] | undefined): string | null {
  const requestedEngine = normalizeEngineName(
    page.searchParams.get('engine') ?? page.searchParams.get('runtimeEngine')
  );
  if (!requestedEngine && !hasPackagedDualEngine(packagedFiles)) return null;
  const engine = requestedEngine ?? (supportsWasmJspi() ? 'jspi' : 'classic');
  return `./engine/${engine}/`;
}
//=============================================================================
export function resolveVersionedAssetUrl(
  path: string,
  baseHref: string,
  versionSourceHref = baseHref
): string {
  const base = new URL(baseHref);
  const buildId = new URL(versionSourceHref, base).searchParams.get('build') ?? '';
  const url = new URL(path, base);
  if (buildId && url.origin === base.origin && !url.searchParams.has('build')) {
    url.searchParams.set('build', buildId);
  }
  return url.href;
}
//=============================================================================
export function resolveWasmRuntimeUrls(
  locationHref: string,
  bundleHref = locationHref,
  packagedFiles?: readonly string[]
): WasmRuntimeUrls {
  const page = new URL(locationHref);
  const buildId =
    new URL(bundleHref, page).searchParams.get('build') ?? page.searchParams.get('build') ?? '';
  const enginePath = defaultEnginePath(page, packagedFiles);
  const modulePath = page.searchParams.get('module') || `${enginePath ?? './'}nelson-portable.mjs`;
  const wasmPath = page.searchParams.get('wasm') || `${enginePath ?? './'}nelson-portable.wasm`;
  const workerPath = page.searchParams.get('worker') || './nelson-worker.mjs';
  const sharedWorkerPath = page.searchParams.get('sharedWorker') || './nelson-shared-worker.mjs';
  const sharedEngine = sharedEngineRequested(page);
  const versionSource = new URL(page);
  if (buildId) versionSource.searchParams.set('build', buildId);
  return {
    moduleUrl: resolveVersionedAssetUrl(modulePath, page.href, versionSource.href),
    wasmUrl: resolveVersionedAssetUrl(wasmPath, page.href, versionSource.href),
    workerUrl: resolveVersionedAssetUrl(workerPath, page.href, versionSource.href),
    ...(sharedEngine
      ? {
          sharedWorkerUrl: resolveVersionedAssetUrl(
            sharedWorkerPath,
            page.href,
            versionSource.href
          ),
        }
      : {}),
  };
}
/**
 * The origin-wide shared engine is the default. `?sharedEngine=0` (or
 * `dedicated`) pins the page to its own dedicated Worker, which tests and
 * troubleshooting use to isolate a page from the other windows.
 */
export function sharedEngineRequested(page: URL): boolean {
  const value = page.searchParams.get('sharedEngine');
  if (value === null) return true;
  return !['0', 'false', 'off', 'no', 'dedicated'].includes(value.toLowerCase());
}
//=============================================================================
