//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Indirection: keeps `new URL(rel, base)` resolved at runtime instead of
// being rewritten by Vite's build-time asset analysis.
const MODULE_URL: string = import.meta.url;
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { BlockDefinitionsMap } from './block-registry.js';
import { getPreferencesPath } from './host-utils.js';
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
import { buildRegistryFromManifest } from '../libraries/generic-block-renderer.js';
import { loadBuiltinSprite } from '../libraries/icon-cache.js';
import { isEngineNativeBlockType } from '../libraries/engine-native-blocks.js';
//=============================================================================
// Start the builtin icon sprite load when this module is evaluated, before
// startApp() and before any palette or block preview renders, so every getIcon()
// waits for the one sprite request. Only for http(s) URLs: inert under test
// (file:// import URLs), where fetch is mocked.
try {
  if (typeof MODULE_URL === 'string' && /^https?:/i.test(MODULE_URL)) {
    void loadBuiltinSprite(new URL('../libraries/_builtin-icons.sprite.svg', MODULE_URL).href);
  }
} catch {
  // ignore; getIcon() falls back to per-file fetches
}
//=============================================================================
export type LoadedLibrary = {
  id: string;
  manifest: Record<string, unknown>;
  library: Record<string, unknown>;
  createTemplates?: (helpers: Record<string, unknown>) => Record<string, unknown> | null;
};
//=============================================================================
const loadedLibraries: Map<string, LoadedLibrary> = new Map();
const discoveredManifests: Map<string, Record<string, unknown>> = new Map();
const searchPaths: string[] = [];
export interface LibraryManifest extends Record<string, unknown> {
  id?: string;
  title?: string;
  version?: string;
  native?: boolean | string;
  blocks?: Array<{ type?: string }>;
  _dir?: string;
}
//=============================================================================
export interface LibraryLocation {
  path: string;
  available: boolean;
  libraryCount: number;
}
//=============================================================================
export interface SearchPathResult {
  success: boolean;
  paths: string[];
  manifests: LibraryManifest[];
  locations: LibraryLocation[];
  diagnostics: string[];
  message?: string;
}
//=============================================================================
interface LibraryConfig {
  enabledLibraries?: string[];
  searchPaths?: string[];
}
//=============================================================================
const BUILTIN_IDS = [
  'builtin.source',
  'builtin.math',
  'builtin.lookup',
  'builtin.logic',
  'builtin.continuous',
  'builtin.discrete',
  'builtin.nonlinear',
  'builtin.sink',
  'builtin.utility',
  'builtin.fmi',
  'builtin.userdefined',
  'builtin.dashboard',
  'builtin.acausal_electrical',
  'builtin.acausal_thermal',
  'builtin.acausal_translational',
  'builtin.acausal_rotational',
  'builtin.acausal_planar',
];
//=============================================================================
const BUILTIN_DIR_MAP: Record<string, string> = {
  'builtin.source': 'source',
  'builtin.math': 'math',
  'builtin.lookup': 'lookup',
  'builtin.logic': 'logic',
  'builtin.continuous': 'continuous',
  'builtin.discrete': 'discrete',
  'builtin.nonlinear': 'nonlinear',
  'builtin.sink': 'sink',
  'builtin.utility': 'utility',
  'builtin.fmi': 'fmi',
  'builtin.userdefined': 'userdefined',
  'builtin.dashboard': 'dashboard',
  'builtin.acausal_electrical': 'acausal_electrical',
  'builtin.acausal_thermal': 'acausal_thermal',
  'builtin.acausal_translational': 'acausal_translational',
  'builtin.acausal_rotational': 'acausal_rotational',
  'builtin.acausal_planar': 'acausal_planar',
};
//=============================================================================
function _libraryFromManifest(manifest: Record<string, unknown>) {
  return {
    id: manifest.id as string,
    title: (manifest.title as string) || (manifest.id as string),
    blocks: ((manifest.blocks as any[]) || []).map((b) => ({
      type: b.type,
      label: b.label || b.type,
    })),
  };
}
//=============================================================================
function _createTemplatesFromManifest(
  manifest: Record<string, unknown>,
  renderers: Record<string, (...args: any[]) => any>,
  resizers: Record<string, (...args: any[]) => any> | null
) {
  return function createTemplates(helpers: Record<string, unknown>) {
    const templates: Record<string, unknown> = {};
    /* eslint-disable @typescript-eslint/no-explicit-any */
    for (const blockDef of (manifest.blocks as any[]) || []) {
      const { type, width, height, inputs, outputs, defaultParams } = blockDef as any;
      const renderFn = renderers[type];
      if (!renderFn) continue;
      const tmpl: any = {
        width: width ?? 80,
        height: height ?? 60,
        inputs: inputs ?? [],
        outputs: outputs ?? [{ x: width ?? 80, y: (height ?? 60) / 2, side: 'right' }],
        defaultParams: defaultParams ?? {},
        render: (block: any) => renderFn(block, helpers),
      };
      // The save/Run lift reads template.physicalPins / .acausal to build
      // `physicalWires`; if they are dropped here the block has no pins and the
      // engine rejects it (unsupported_block).
      const bd = blockDef as any;
      if (bd.acausal) tmpl.acausal = true;
      if (bd.physicalPins) tmpl.physicalPins = bd.physicalPins;
      if (bd.domain) tmpl.domain = bd.domain;
      if (resizers && resizers[type]) tmpl.resize = resizers[type];
      templates[type] = tmpl;
      // register namespaced ID for nflow to avoid collisions between libraries
      try {
        const ns = `nflow.${(manifest as any).id}.${type}`;
        templates[ns] = tmpl;
      } catch {}
    }
    return templates;
  };
}
//=============================================================================
function _createTemplatesFromDefinitions(
  manifest: Record<string, unknown>,
  defs: BlockDefinitionsMap,
  resizers: Record<string, (...args: any[]) => any> | null
) {
  return function createTemplates(helpers: Record<string, unknown>) {
    const templates: Record<string, unknown> = {};
    const manifestMap: Record<string, any> = {};
    for (const b of (manifest.blocks as any[]) || []) {
      if (b && b.type) manifestMap[b.type] = b;
    }
    for (const [k, def] of Object.entries(defs || {})) {
      const type = (def && def.type) || k;
      const m = manifestMap[type] || {};
      const width = def?.width ?? m.width ?? 80;
      const height = def?.height ?? m.height ?? 60;
      const inputs = def?.inputs ?? m.inputs ?? [];
      const outputs = def?.outputs ?? m.outputs ?? [{ x: width, y: height / 2, side: 'right' }];
      const defaultParams = def?.defaultParams ?? m.defaultParams ?? {};
      const renderFn = def?.render ?? null;
      if (!renderFn && !def?.createTemplate) continue;
      const tmpl: any = {
        width,
        height,
        inputs,
        outputs,
        defaultParams,
        render: (block: any) => (renderFn ? renderFn(block, helpers) : undefined),
      };
      // Acausal metadata comes from the definition or its manifest entry; see
      // _createTemplatesFromManifest for why it must be kept.
      const acausal = (def as any)?.acausal ?? m.acausal;
      if (acausal) tmpl.acausal = true;
      const physicalPins = (def as any)?.physicalPins ?? m.physicalPins;
      if (physicalPins) tmpl.physicalPins = physicalPins;
      const domain = (def as any)?.domain ?? m.domain;
      if (domain) tmpl.domain = domain;
      if (def?.resize) tmpl.resize = def.resize;
      if (resizers && resizers[type]) tmpl.resize = resizers[type];
      templates[type] = tmpl;
      try {
        const ns = `nflow.${(manifest as any).id}.${type}`;
        templates[ns] = tmpl;
      } catch {}
    }
    return templates;
  };
}
//=============================================================================
export async function discoverBuiltinLibraries(): Promise<void> {
  const fetches = BUILTIN_IDS.map(async (id) => {
    const dir = BUILTIN_DIR_MAP[id];
    if (!dir) return;
    try {
      const url = new URL(`../libraries/${dir}/library.json`, MODULE_URL).href;
      const resp = await fetch(url);
      if (!resp.ok) return;
      const manifest = await resp.json();
      (manifest as any)._dir = dir;
      discoveredManifests.set(id, manifest as Record<string, unknown>);
    } catch {
      // ignore
    }
  });
  await Promise.all(fetches);
}
//=============================================================================
export async function discoverFromPath(_dirUrl: string) {
  // placeholder for future native bridge
}
//=============================================================================
// Build a library entry from its manifest alone, for libraries that ship no
// blocks.js render module (image-rendered acausal blocks). A declared
// manifest-only library takes this path without first triggering a 404.
async function _loadLibraryFromManifest(
  id: string,
  manifest: Record<string, unknown>
): Promise<LoadedLibrary> {
  const library = _libraryFromManifest(manifest);
  const registry = buildRegistryFromManifest(manifest as any);
  const defs = registry.getDefinitions();
  const createTemplates = _createTemplatesFromDefinitions(
    manifest,
    defs as BlockDefinitionsMap,
    null
  );
  const entry: LoadedLibrary = { id, manifest, library, createTemplates } as LoadedLibrary;
  loadedLibraries.set(id, entry);
  await _persistEnabledIds();
  return entry;
}
//=============================================================================
export async function loadLibrary(id: string): Promise<LoadedLibrary | null> {
  if (loadedLibraries.has(id)) return loadedLibraries.get(id) || null;

  const manifest = discoveredManifests.get(id);
  if (!manifest) return null;

  const dir = (manifest as any)._dir as string | undefined;
  if (!dir) return null;

  // `renderModule: false` means no blocks.js: skip the dynamic import to avoid a
  // 404 per such library at every boot.
  if ((manifest as Record<string, unknown>).renderModule === false) {
    try {
      return await _loadLibraryFromManifest(id, manifest);
    } catch {
      // Fall through to the normal import path as a last resort.
    }
  }

  try {
    const moduleUrl = new URL(`../libraries/${dir}/blocks.js`, MODULE_URL).href;
    const mod = await import(moduleUrl);
    const library = mod.library ?? _libraryFromManifest(manifest);
    let createTemplates = mod.createTemplates ?? null;
    if (!createTemplates) {
      if (mod.blockDefinitions) {
        createTemplates = _createTemplatesFromDefinitions(
          manifest,
          mod.blockDefinitions as BlockDefinitionsMap,
          mod.resizers ?? null
        );
      } else if (mod.blockRegistry) {
        // `blockRegistry` can be an instance with `getDefinitions()` or a plain map
        try {
          let registry = mod.blockRegistry;
          if (typeof registry === 'function') {
            try {
              registry = new registry();
            } catch {}
          }
          const defs =
            typeof registry.getDefinitions === 'function'
              ? registry.getDefinitions()
              : registry.definitions ?? registry;
          createTemplates = _createTemplatesFromDefinitions(
            manifest,
            defs as BlockDefinitionsMap,
            mod.resizers ?? null
          );
        } catch {}
      } else if (mod.BlockRegistry) {
        try {
          const RegistryClass = mod.BlockRegistry as any;
          const registry = new RegistryClass(mod.blockDefinitions ?? undefined);
          const defs =
            typeof registry.getDefinitions === 'function'
              ? registry.getDefinitions()
              : registry.definitions ?? registry;
          createTemplates = _createTemplatesFromDefinitions(
            manifest,
            defs as BlockDefinitionsMap,
            mod.resizers ?? null
          );
        } catch {}
      } else if (mod.renderers) {
        createTemplates = _createTemplatesFromManifest(
          manifest,
          mod.renderers,
          mod.resizers ?? null
        );
      }
    }
    const entry: LoadedLibrary = { id, manifest, library, createTemplates } as LoadedLibrary;
    loadedLibraries.set(id, entry);
    await _persistEnabledIds();
    return entry;
  } catch (err) {
    // No blocks.js (manifest-only packaging): build from the manifest.
    try {
      return await _loadLibraryFromManifest(id, manifest);
    } catch (e2) {
      // eslint-disable-next-line no-console
      console.error(`[library-loader] Failed to load library "${id}":`, err);
      return null;
    }
  }
}
//=============================================================================
export async function loadAllDiscovered(): Promise<LoadedLibrary[]> {
  const enabled = await _loadEnabledIds();
  // Builtin libraries are always enabled: a persisted whitelist written before a
  // builtin library existed must not hide it. The whitelist only gates external
  // libraries.
  const ids = Array.from(discoveredManifests.keys()).filter(
    (id) => id.startsWith('builtin.') || enabled === null || enabled.has(id)
  );
  // Each loadLibrary dynamic-imports a blocks.js; load them in parallel.
  const results = await Promise.all(
    ids.map(async (id) => {
      let entry = await loadLibrary(id);
      if (!entry) {
        entry = await enableLibraryFromManifest(id);
      }
      return entry || null;
    })
  );
  await _persistEnabledIds();
  return results.filter(Boolean) as LoadedLibrary[];
}
/**
 * Warm the icon cache for all builtin icons with one request for the prebuilt
 * sprite. On the WebView2 host, 183 individual fetches are slow enough that a
 * diagram paints before its icons arrive. If the sprite is absent (dev server),
 * getIcon() falls back to per-icon fetches. External libraries are not in the
 * sprite.
 */
export function prewarmBuiltinIcons(): Promise<void> {
  try {
    const spriteUrl = new URL('../libraries/_builtin-icons.sprite.svg', MODULE_URL).href;
    return loadBuiltinSprite(spriteUrl);
  } catch {
    return Promise.resolve();
  }
}
//=============================================================================
export async function unloadLibrary(id: string) {
  loadedLibraries.delete(id);
  await _persistEnabledIds();
}
//=============================================================================
export function getLoadedLibraries() {
  return loadedLibraries;
}
//=============================================================================
export async function enableLibraryFromManifest(id: string): Promise<LoadedLibrary | null> {
  if (loadedLibraries.has(id)) return loadedLibraries.get(id) || null;
  const manifest = discoveredManifests.get(id);
  if (!manifest) return null;
  try {
    const library = _libraryFromManifest(manifest);
    let createTemplates: LoadedLibrary['createTemplates'] | undefined = undefined;
    try {
      const registry = buildRegistryFromManifest(manifest as any);
      const defs = registry.getDefinitions();
      createTemplates = _createTemplatesFromDefinitions(
        manifest,
        defs as BlockDefinitionsMap,
        null
      );
    } catch (e) {
      try {
        const maybeManifest = manifest as unknown as {
          renderers?: Record<string, (...args: any[]) => any>;
          resizers?: Record<string, (...args: any[]) => any> | null;
        };
        if (maybeManifest.renderers) {
          createTemplates = _createTemplatesFromManifest(
            manifest,
            maybeManifest.renderers,
            maybeManifest.resizers ?? null
          );
        }
      } catch {}
    }

    const entry: LoadedLibrary = { id, manifest, library, createTemplates } as LoadedLibrary;
    loadedLibraries.set(id, entry);
    await _persistEnabledIds();
    return entry;
  } catch (err) {
    // eslint-disable-next-line no-console
    console.error(`[library-loader] enableLibraryFromManifest failed for "${id}":`, err);
    return null;
  }
}
//=============================================================================
export function getDiscoveredManifests() {
  return discoveredManifests;
}
//=============================================================================
export function replaceExternalManifests(manifests: LibraryManifest[]): void {
  const nextManifests = new Map(
    manifests
      .map((manifest) => [String(manifest?.id || ''), manifest] as const)
      .filter(([id]) => Boolean(id) && !id.startsWith('builtin.'))
  );
  for (const id of Array.from(discoveredManifests.keys())) {
    if (!id.startsWith('builtin.')) {
      const current = discoveredManifests.get(id);
      const replacement = nextManifests.get(id);
      discoveredManifests.delete(id);
      if (!replacement || JSON.stringify(current) !== JSON.stringify(replacement)) {
        loadedLibraries.delete(id);
      }
    }
  }
  for (const [id, manifest] of nextManifests) {
    discoveredManifests.set(id, manifest);
  }
}
//=============================================================================
export function isLibraryLoaded(id: string) {
  return loadedLibraries.has(id);
}
//=============================================================================
export function getMissingBlockTypes(types: string[]) {
  const covered = new Set<string>();
  for (const entry of loadedLibraries.values()) {
    if (entry.manifest && Array.isArray((entry.manifest as any).blocks)) {
      for (const b of (entry.manifest as any).blocks) {
        if (b && b.type) covered.add(b.type);
      }
    }
  }

  const typeToLib = new Map<string, { libraryId: string; libraryTitle?: string }>();
  for (const [libId, manifest] of discoveredManifests) {
    if (Array.isArray((manifest as any).blocks)) {
      for (const b of (manifest as any).blocks) {
        if (b && b.type) {
          const info = {
            libraryId: libId,
            libraryTitle: (manifest as any).title || libId,
          };
          typeToLib.set(b.type, info);
          try {
            const ns = `nflow.${libId}.${b.type}`;
            typeToLib.set(ns, info);
          } catch {}
        }
      }
    }
  }

  const seen = new Set<string>();
  const missing: Array<{ type: string; libraryId?: string; libraryTitle?: string }> = [];
  for (const t of types) {
    if (!t || covered.has(t) || seen.has(t) || isEngineNativeBlockType(t)) continue;
    seen.add(t);
    const lib = typeToLib.get(t);
    missing.push(lib ? { type: t, ...lib } : { type: t });
  }
  return missing;
}
//=============================================================================
export function buildBlockLibrary() {
  const preferredOrder = [
    'builtin.source',
    'builtin.continuous',
    'builtin.math',
    'builtin.lookup',
    'builtin.logic',
    'builtin.discrete',
    'builtin.nonlinear',
    'builtin.userdefined',
    'builtin.utility',
    'builtin.sink',
  ];

  const ordered: Record<string, unknown>[] = [];
  const seen = new Set<string>();
  for (const id of preferredOrder) {
    const entry = loadedLibraries.get(id);
    if (entry) {
      ordered.push(entry.library);
      seen.add(id);
    }
  }
  for (const [id, entry] of loadedLibraries) {
    if (!seen.has(id)) ordered.push(entry.library);
  }
  return ordered;
}
//=============================================================================
export function buildAllBlockTemplates(helpers: Record<string, unknown>) {
  const merged: Record<string, unknown> = {};
  for (const entry of loadedLibraries.values()) {
    if (typeof entry.createTemplates === 'function') {
      Object.assign(merged, entry.createTemplates!(helpers));
    }
  }
  return merged;
}
//=============================================================================
let _configFilePath: string | null = null;
async function _resolveConfigPath(): Promise<string | null> {
  if (_configFilePath) return _configFilePath;
  try {
    const root = await getPreferencesPath();
    if (root) {
      const sep = root.includes('\\') ? '\\' : '/';
      _configFilePath = root + sep + 'nflow' + sep + 'libraries.json';
      return _configFilePath;
    }
  } catch {}
  return null;
}
//=============================================================================
async function _persistEnabledIds(raiseError = false) {
  try {
    const path = await _resolveConfigPath();
    if (!path) return;
    const enabledLibraries = Array.from(loadedLibraries.keys())
      .filter((id) => !id.startsWith('builtin.'))
      .sort();
    const data = JSON.stringify({ enabledLibraries, searchPaths: [...searchPaths] }, null, 2);
    if (canUseNFlowRpc('nflow.file.writeText')) {
      const raw = await nflowRpc<unknown>('nflow.file.writeText', { content: data, path });
      const response = typeof raw === 'string' ? JSON.parse(raw) : raw;
      if (
        response &&
        typeof response === 'object' &&
        (response as { success?: boolean }).success === false
      ) {
        const result = response as { error?: string; message?: string };
        throw new Error(
          result.message || result.error || 'Library preferences could not be saved.'
        );
      }
    }
  } catch (error) {
    if (raiseError) throw error;
  }
}
//=============================================================================
async function _loadConfig(): Promise<LibraryConfig | null> {
  try {
    const path = await _resolveConfigPath();
    if (!path || !canUseNFlowRpc('nflow.file.readText')) return null;
    const raw = await nflowRpc<unknown>('nflow.file.readText', { path });
    const response = typeof raw === 'string' ? JSON.parse(raw) : raw;
    if (!response || typeof response !== 'object') return null;
    const result = response as { success?: boolean; content?: string };
    if (!result.success || typeof result.content !== 'string') return null;
    const config = JSON.parse(result.content) as LibraryConfig;
    return config && typeof config === 'object' ? config : null;
  } catch {
    return null;
  }
}
//=============================================================================
async function _loadEnabledIds(): Promise<Set<string> | null> {
  try {
    const config = await _loadConfig();
    if (!config) return null;
    if (Array.isArray(config.enabledLibraries)) return new Set(config.enabledLibraries);
  } catch {}
  return null;
}
//=============================================================================
export function getSearchPaths(): string[] {
  return [...searchPaths];
}
//=============================================================================
export function setSearchPaths(paths: string[]) {
  searchPaths.length = 0;
  const seen = new Set<string>();
  for (const rawPath of paths) {
    const path = String(rawPath || '').trim();
    if (!path) continue;
    const key = /^[a-zA-Z]:[\\/]/.test(path) ? path.replace(/\//g, '\\').toLowerCase() : path;
    if (seen.has(key)) continue;
    seen.add(key);
    searchPaths.push(path);
  }
}
//=============================================================================
export function addSearchPath(path: string) {
  setSearchPaths([...searchPaths, path]);
}
//=============================================================================
function parseManifests(raw: unknown): LibraryManifest[] {
  if (!Array.isArray(raw)) return [];
  const manifests: LibraryManifest[] = [];
  for (const item of raw) {
    try {
      const manifest = typeof item === 'string' ? JSON.parse(item) : item;
      if (manifest && typeof manifest === 'object' && manifest.id) {
        manifests.push(manifest as LibraryManifest);
      }
    } catch {
      // Invalid manifests are reported by the native discovery diagnostics.
    }
  }
  return manifests;
}
//=============================================================================
function searchPathKey(path: string): string {
  const value = String(path || '').replace(/[\\/]+$/, '');
  return /^[a-zA-Z]:[\\/]/.test(value) ? value.replace(/\//g, '\\').toLowerCase() : value;
}
//=============================================================================
export async function readNativeSearchPaths(): Promise<string[]> {
  const raw = await nflowRpc<unknown>('nflow.library.getSearchPaths');
  const response = typeof raw === 'string' ? JSON.parse(raw) : raw;
  if (!Array.isArray(response)) {
    throw new Error('Library locations could not be read back.');
  }
  return response.map(String);
}
//=============================================================================
export async function applySearchPaths(paths: string[]): Promise<SearchPathResult> {
  const raw = await nflowRpc<unknown>('nflow.library.setSearchPaths', { paths });
  const response = (typeof raw === 'string' ? JSON.parse(raw) : raw) as Partial<SearchPathResult>;
  if (!response || response.success !== true) {
    throw new Error(response?.message || 'Library locations could not be scanned.');
  }
  const returnedPaths = Array.isArray(response.paths)
    ? response.paths.map(String)
    : paths.map(String);
  const normalizedPaths = canUseNFlowRpc('nflow.library.getSearchPaths')
    ? await readNativeSearchPaths()
    : returnedPaths;
  const manifests = parseManifests(response.manifests);
  const returnedLocations = Array.isArray(response.locations)
    ? response.locations.map((item) => ({
        path: String(item.path || ''),
        available: item.available === true,
        libraryCount: Number(item.libraryCount) || 0,
      }))
    : [];
  const locationsByPath = new Map(
    returnedLocations.map((location) => [searchPathKey(location.path), location])
  );
  const locations = normalizedPaths.map(
    (path) =>
      locationsByPath.get(searchPathKey(path)) ?? {
        path,
        available: false,
        libraryCount: 0,
      }
  );
  const result: SearchPathResult = {
    success: true,
    paths: normalizedPaths,
    manifests,
    locations,
    diagnostics: Array.isArray(response.diagnostics) ? response.diagnostics.map(String) : [],
    message: response.message,
  };
  setSearchPaths(result.paths);
  replaceExternalManifests(result.manifests);
  await loadAllDiscovered();
  for (const [id, manifest] of discoveredManifests) {
    if (
      !id.startsWith('builtin.') &&
      Boolean((manifest as LibraryManifest).native) &&
      !loadedLibraries.has(id)
    ) {
      try {
        await nflowRpc('nflow.library.unload', { id });
      } catch {
        // The GUI state remains disabled even if an older host cannot unload it.
      }
    }
  }
  await _persistEnabledIds();
  return result;
}
//=============================================================================
export async function persistLibraryPreferences(): Promise<void> {
  await _persistEnabledIds(true);
}
//=============================================================================
export async function initLibraries(): Promise<void> {
  // Start the sprite FIRST (its URL needs no manifests): an early palette/canvas
  // render can call getIcon() before init finishes and must wait for the sprite
  // instead of fetching per file. Awaited at the end.
  const spritePromise = prewarmBuiltinIcons();

  await discoverBuiltinLibraries();

  const config = await _loadConfig();
  if (Array.isArray(config?.searchPaths)) setSearchPaths(config.searchPaths);

  if (canUseNFlowRpc('nflow.library.setSearchPaths') && searchPaths.length > 0) {
    try {
      const result = await applySearchPaths(searchPaths);
      setSearchPaths(result.paths);
    } catch {
      // The palette remains usable with built-in libraries.
    }
  } else if (canUseNFlowRpc('nflow.library.getSearchPaths')) {
    try {
      setSearchPaths(await readNativeSearchPaths());
    } catch {
      // Keep configured paths when the host cannot report runtime state.
    }
  }

  try {
    if (canUseNFlowRpc('nflow.library.list')) {
      const raw = await nflowRpc<unknown>('nflow.library.list');
      if (raw) {
        try {
          const hostManifests = typeof raw === 'string' ? JSON.parse(raw) : raw;
          if (Array.isArray(hostManifests)) replaceExternalManifests(parseManifests(hostManifests));
        } catch {
          // ignore parse error of top-level response
        }
      }
    }
  } catch {
    // ignore host call errors
  }

  await loadAllDiscovered();

  // lib-ready gates the first diagram render, which needs the sprite in cache.
  await spritePromise;
}
//=============================================================================
