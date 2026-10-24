//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
interface ManifestFile {
  file?: unknown;
  bytes?: unknown;
}
//=============================================================================
export interface DeliveryGroup {
  id: string;
  mode: 'startup' | 'on-demand';
  trigger?: string;
  files: string[];
  bytes: number;
}
//=============================================================================
export interface BrowserManifest {
  formatVersion?: unknown;
  buildId?: unknown;
  totalBytes?: unknown;
  files?: unknown;
  delivery?: {
    formatVersion?: unknown;
    startup?: unknown;
    onDemand?: unknown;
  };
}
//=============================================================================
export interface StartupDownloadPlan {
  wasmBytes?: number;
  resourceBytes?: number;
  startupBytes?: number;
  onDemandBytes?: number;
}
//=============================================================================
export function browserManifestFileNames(manifest: BrowserManifest): string[] {
  if (!Array.isArray(manifest.files)) return [];
  return manifest.files.flatMap((entry): string[] => {
    if (!entry || typeof entry !== 'object') return [];
    const file = (entry as ManifestFile).file;
    return typeof file === 'string' ? [file] : [];
  });
}
//=============================================================================
export interface RuntimeCapabilityManifest {
  formatVersion: 1;
  runtime: 'webassembly';
  profile: string;
  modules: string[];
  capabilities: string[];
  features: {
    blas: boolean;
    controlSystem: boolean;
    lapack: boolean;
    nativeWasmExceptions: boolean;
    nflow: boolean;
    odeSolvers: boolean;
    packageManager: boolean;
    plots: boolean;
    slicot: boolean;
    threadedWasm: boolean;
    virtualFilesystem: boolean;
    worker: boolean;
  };
  limits: {
    maxNflowJsonBytes: number;
    maxSourceBytes: number;
    maxVariableBlockCells: number;
    maxVirtualFileBytes: number;
    maxWasmBytes: number;
  };
  unavailable: string[];
}
//=============================================================================
function stringArray(value: unknown, label: string): string[] {
  if (!Array.isArray(value) || value.some((entry) => typeof entry !== 'string')) {
    throw new Error(`Invalid WebAssembly capability manifest field: ${label}`);
  }
  return value as string[];
}
//=============================================================================
export function parseRuntimeCapabilities(value: unknown): RuntimeCapabilityManifest {
  if (!value || typeof value !== 'object') {
    throw new Error('Invalid WebAssembly capability manifest');
  }
  const manifest = value as Record<string, unknown>;
  const features = manifest.features as Record<string, unknown> | undefined;
  if (
    manifest.formatVersion !== 1 ||
    manifest.runtime !== 'webassembly' ||
    typeof manifest.profile !== 'string' ||
    !features
  ) {
    throw new Error('Unsupported WebAssembly capability manifest');
  }
  const booleanFeatures = [
    'blas',
    'controlSystem',
    'lapack',
    'nflow',
    'odeSolvers',
    'packageManager',
    'plots',
    'slicot',
    'virtualFilesystem',
    'worker',
  ];
  if (booleanFeatures.some((name) => typeof features[name] !== 'boolean')) {
    throw new Error('Invalid WebAssembly capability feature flags');
  }
  if (
    features.nativeWasmExceptions !== undefined &&
    typeof features.nativeWasmExceptions !== 'boolean'
  ) {
    throw new Error('Invalid WebAssembly capability feature flags');
  }
  if (features.threadedWasm !== undefined && typeof features.threadedWasm !== 'boolean') {
    throw new Error('Invalid WebAssembly capability feature flags');
  }
  const parsedFeatures: RuntimeCapabilityManifest['features'] = {
    ...(features as RuntimeCapabilityManifest['features']),
    nativeWasmExceptions: features.nativeWasmExceptions === true,
    threadedWasm: features.threadedWasm === true,
  };
  const limits = manifest.limits as Record<string, unknown> | undefined;
  const limitNames = [
    'maxNflowJsonBytes',
    'maxSourceBytes',
    'maxVariableBlockCells',
    'maxVirtualFileBytes',
    'maxWasmBytes',
  ];
  if (
    !limits ||
    limitNames.some((name) => !Number.isSafeInteger(limits[name]) || Number(limits[name]) <= 0)
  ) {
    throw new Error('Invalid WebAssembly capability resource limits');
  }
  return {
    formatVersion: 1,
    runtime: 'webassembly',
    profile: manifest.profile,
    modules: stringArray(manifest.modules, 'modules'),
    capabilities: stringArray(manifest.capabilities, 'capabilities'),
    features: parsedFeatures,
    limits: limits as RuntimeCapabilityManifest['limits'],
    unavailable: stringArray(manifest.unavailable, 'unavailable'),
  };
}
//=============================================================================
export async function loadRuntimeCapabilities(
  manifestUrl: string,
  fetchImpl: typeof fetch = fetch
): Promise<RuntimeCapabilityManifest> {
  const response = await fetchImpl(manifestUrl);
  if (!response.ok) {
    throw new Error(`WebAssembly capability manifest failed: HTTP ${response.status}`);
  }
  return parseRuntimeCapabilities(await response.json());
}
//=============================================================================
function parseDeliveryGroups(value: unknown, mode: DeliveryGroup['mode']): DeliveryGroup[] {
  if (!Array.isArray(value)) throw new Error('Invalid WebAssembly delivery groups');
  return value.map((entry) => {
    if (!entry || typeof entry !== 'object') {
      throw new Error('Invalid WebAssembly delivery group');
    }
    const group = entry as Record<string, unknown>;
    if (
      typeof group.id !== 'string' ||
      group.mode !== mode ||
      !Array.isArray(group.files) ||
      group.files.some((file) => typeof file !== 'string') ||
      !Number.isSafeInteger(group.bytes) ||
      Number(group.bytes) < 0 ||
      (mode === 'on-demand' && typeof group.trigger !== 'string')
    ) {
      throw new Error('Invalid WebAssembly delivery group');
    }
    return group as unknown as DeliveryGroup;
  });
}
//=============================================================================
export function parseBrowserManifest(value: unknown): BrowserManifest {
  if (!value || typeof value !== 'object') {
    throw new Error('Invalid WebAssembly deployment manifest');
  }
  const manifest = value as BrowserManifest;
  if (manifest.formatVersion !== 1 || !Array.isArray(manifest.files)) {
    throw new Error('Unsupported WebAssembly deployment manifest');
  }
  const delivery = manifest.delivery;
  if (!delivery || delivery.formatVersion !== 1) {
    throw new Error('WebAssembly delivery plan is missing');
  }
  const startup = parseDeliveryGroups(delivery.startup, 'startup');
  const onDemand = parseDeliveryGroups(delivery.onDemand, 'on-demand');
  const delivered = [...startup, ...onDemand].flatMap(({ files }) => files).sort();
  const packaged = manifest.files
    .map((entry) => (entry as ManifestFile).file)
    .filter((file): file is string => typeof file === 'string')
    .sort();
  if (
    delivered.length !== packaged.length ||
    delivered.some((file, index) => file !== packaged[index])
  ) {
    throw new Error('WebAssembly delivery plan does not cover every packaged file');
  }
  return { ...manifest, delivery: { formatVersion: 1, startup, onDemand } };
}
//=============================================================================
export async function loadBrowserManifest(
  manifestUrl: string,
  fetchImpl: typeof fetch = fetch
): Promise<BrowserManifest> {
  const response = await fetchImpl(manifestUrl);
  if (!response.ok) {
    throw new Error(`WebAssembly deployment manifest failed: HTTP ${response.status}`);
  }
  return parseBrowserManifest(await response.json());
}
//=============================================================================
export function wasmBytesFromManifest(
  manifest: BrowserManifest,
  manifestUrl: string,
  wasmUrl: string
): number | undefined {
  if (!Array.isArray(manifest.files)) return undefined;
  const baseUrl = new URL('.', manifestUrl);
  for (const value of manifest.files) {
    const file = value as ManifestFile;
    if (typeof file.file !== 'string') continue;
    if (new URL(file.file, baseUrl).href !== wasmUrl) continue;
    const bytes = Number(file.bytes);
    return Number.isSafeInteger(bytes) && bytes > 0 ? bytes : undefined;
  }
  return undefined;
}
//=============================================================================
export async function loadExpectedWasmBytes(
  manifestUrl: string,
  wasmUrl: string,
  fetchImpl: typeof fetch = fetch
): Promise<number | undefined> {
  try {
    const response = await fetchImpl(manifestUrl);
    if (!response.ok) return undefined;
    return wasmBytesFromManifest((await response.json()) as BrowserManifest, manifestUrl, wasmUrl);
  } catch {
    return undefined;
  }
}
//=============================================================================
export function startupDownloadPlan(
  manifest: BrowserManifest,
  manifestUrl: string,
  wasmUrl: string
): StartupDownloadPlan {
  const startup = Array.isArray(manifest.delivery?.startup)
    ? (manifest.delivery.startup as DeliveryGroup[])
    : undefined;
  const onDemand = Array.isArray(manifest.delivery?.onDemand)
    ? (manifest.delivery.onDemand as DeliveryGroup[])
    : undefined;
  const resourceBytes = startup?.find(({ id }) => id === 'resources')?.bytes;
  return {
    wasmBytes: wasmBytesFromManifest(manifest, manifestUrl, wasmUrl),
    ...(Number.isSafeInteger(resourceBytes) && Number(resourceBytes) > 0
      ? { resourceBytes: Number(resourceBytes) }
      : {}),
    ...(startup ? { startupBytes: startup.reduce((sum, group) => sum + group.bytes, 0) } : {}),
    ...(onDemand ? { onDemandBytes: onDemand.reduce((sum, group) => sum + group.bytes, 0) } : {}),
  };
}
//=============================================================================
