//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import {
  applySearchPaths,
  enableLibraryFromManifest,
  getDiscoveredManifests,
  getLoadedLibraries,
  getSearchPaths,
  loadLibrary,
  persistLibraryPreferences,
  type LibraryLocation,
  type LibraryManifest,
  unloadLibrary,
} from './library-loader.js';
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
import { addDiagnostic } from './diagnostics-service.js';
import { pushStatus } from './status-service.js';
import { state } from './state.js';
import { setBlockLibrary, renderBlockLibrary, type BlockGroup } from './block-library-handlers.js';
import { refreshBlockTemplates } from '../render/render.js';
import { buildBlockLibrary } from './library-loader.js';
import { t } from '../utils/i18n.js';
import { emitAppEvent } from './app-events.js';
//=============================================================================
export interface ManagedLibrary {
  id: string;
  title: string;
  version: string;
  blockCount: number;
  builtIn: boolean;
  enabled: boolean;
  native: boolean;
  path: string;
  usedBlockIds: string[];
  manifest: LibraryManifest;
}
//=============================================================================
export interface LibraryServiceSnapshot {
  busy: boolean;
  locations: LibraryLocation[];
  libraries: ManagedLibrary[];
}
//=============================================================================
type Subscriber = (snapshot: LibraryServiceSnapshot) => void;
let busy = false;
let locations: LibraryLocation[] = [];
let operation = Promise.resolve();
const subscribers = new Set<Subscriber>();
function blockIdsFor(manifest: LibraryManifest): string[] {
  const libraryId = String(manifest.id || '');
  const types = new Set<string>();
  for (const block of manifest.blocks || []) {
    const type = String(block?.type || '');
    if (!type) continue;
    types.add(type);
    types.add(`nflow.${libraryId}.${type}`);
  }
  const ids: string[] = [];
  for (const [fallbackId, value] of state.blocks) {
    const block = value as { id?: string | number; type?: string };
    if (block && types.has(String(block.type || ''))) {
      ids.push(String(block.id ?? fallbackId));
    }
  }
  return ids;
}
//=============================================================================
export function getLibraryServiceSnapshot(): LibraryServiceSnapshot {
  const loaded = getLoadedLibraries();
  const libraries = Array.from(getDiscoveredManifests(), ([id, value]) => {
    const manifest = value as LibraryManifest;
    return {
      id,
      title: String(manifest.title || id),
      version: String(manifest.version || ''),
      blockCount: Array.isArray(manifest.blocks) ? manifest.blocks.length : 0,
      builtIn: id.startsWith('builtin.'),
      enabled: id.startsWith('builtin.') || loaded.has(id),
      native: Boolean(manifest.native),
      path: String(manifest._dir || ''),
      usedBlockIds: blockIdsFor(manifest),
      manifest,
    };
  }).sort(
    (left, right) =>
      Number(left.builtIn) - Number(right.builtIn) || left.title.localeCompare(right.title)
  );
  return { busy, locations: locations.slice(), libraries };
}
//=============================================================================
function notify(): void {
  const snapshot = getLibraryServiceSnapshot();
  for (const subscriber of subscribers) subscriber(snapshot);
}
//=============================================================================
export function refreshLibraryServiceSnapshot(): void {
  notify();
}
//=============================================================================
export function subscribeLibraryService(subscriber: Subscriber): () => void {
  subscribers.add(subscriber);
  subscriber(getLibraryServiceSnapshot());
  return () => subscribers.delete(subscriber);
}
//=============================================================================
function reportDiscoveryMessages(messages: string[]): void {
  for (const message of messages) {
    addDiagnostic({ severity: 'warning', message: t(message), source: 'library' });
  }
}
//=============================================================================
function reportSuccess(message: string): void {
  pushStatus({ level: 'success', message: t(message), durationMs: 3000 });
}
//=============================================================================
function rebuildPalette(): void {
  setBlockLibrary(buildBlockLibrary() as unknown as BlockGroup[]);
  refreshBlockTemplates();
  renderBlockLibrary();
}
//=============================================================================
function enqueue<T>(task: () => Promise<T>): Promise<T> {
  const next = operation.then(async () => {
    busy = true;
    notify();
    try {
      return await task();
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      pushStatus({
        level: 'error',
        message: t(message),
        durationMs: 6000,
        diagnosticSource: 'library',
      });
      throw error;
    } finally {
      busy = false;
      notify();
    }
  });
  operation = next.then(
    () => undefined,
    () => undefined
  );
  return next;
}
//=============================================================================
async function applyAndReport(paths: string[]) {
  const result = await applySearchPaths(paths);
  locations = result.locations;
  reportDiscoveryMessages(result.diagnostics);
  rebuildPalette();
  return result;
}
//=============================================================================
export async function rescanLibraries(): Promise<void> {
  if (!canUseNFlowRpc('nflow.library.setSearchPaths')) {
    locations = getSearchPaths().map((path) => ({ path, available: false, libraryCount: 0 }));
    notify();
    return;
  }
  await enqueue(async () => {
    const result = await applyAndReport(getSearchPaths());
    reportSuccess(result.message || 'Library locations scanned.');
  });
}
//=============================================================================
export async function addLibraryLocation(path: string): Promise<void> {
  await enqueue(async () => {
    await applyAndReport([...getSearchPaths(), path]);
    await persistLibraryPreferences();
    reportSuccess('Library location added.');
  });
}
//=============================================================================
export async function replaceLibraryLocation(index: number, path: string): Promise<void> {
  await enqueue(async () => {
    const paths = getSearchPaths();
    paths[index] = path;
    await applyAndReport(paths);
    await persistLibraryPreferences();
    reportSuccess('Library location updated.');
  });
}
//=============================================================================
function librariesUnderPath(path: string): ManagedLibrary[] {
  const key = path.replace(/[\\/]+$/, '').toLowerCase();
  return getLibraryServiceSnapshot().libraries.filter(
    (library) =>
      !library.builtIn &&
      (library.path.toLowerCase().startsWith(`${key}\\`) ||
        library.path.toLowerCase().startsWith(`${key}/`))
  );
}
//=============================================================================
export async function removeLibraryLocation(index: number): Promise<void> {
  const path = getSearchPaths()[index];
  const used = librariesUnderPath(path).flatMap((library) => library.usedBlockIds);
  if (used.length > 0) throw new Error(t('This location contains libraries used by the diagram.'));
  await enqueue(async () => {
    const paths = getSearchPaths();
    paths.splice(index, 1);
    await applyAndReport(paths);
    await persistLibraryPreferences();
    reportSuccess('Library location removed.');
  });
}
//=============================================================================
export async function moveLibraryLocation(index: number, offset: -1 | 1): Promise<void> {
  const target = index + offset;
  const paths = getSearchPaths();
  if (target < 0 || target >= paths.length) return;
  [paths[index], paths[target]] = [paths[target], paths[index]];
  await enqueue(async () => {
    await applyAndReport(paths);
    await persistLibraryPreferences();
    reportSuccess('Library location moved.');
  });
}
//=============================================================================
export async function setLibraryEnabled(library: ManagedLibrary, enabled: boolean): Promise<void> {
  if (library.builtIn) return;
  if (!enabled && library.usedBlockIds.length > 0) {
    throw new Error(t('This library is used by the current diagram.'));
  }
  await enqueue(async () => {
    if (enabled) {
      if (library.native && library.path) {
        const response = await nflowRpc<{ success?: boolean; message?: string }>(
          'nflow.library.load',
          { path: library.path }
        );
        if (!response?.success)
          throw new Error(response?.message || 'Library could not be loaded.');
        await enableLibraryFromManifest(library.id);
      } else {
        const loaded =
          (await loadLibrary(library.id)) || (await enableLibraryFromManifest(library.id));
        if (!loaded) throw new Error('Library could not be enabled.');
      }
    } else {
      if (library.native) {
        const response = await nflowRpc<{ success?: boolean; message?: string }>(
          'nflow.library.unload',
          { id: library.id }
        );
        if (!response?.success)
          throw new Error(response?.message || 'Library could not be disabled.');
      }
      await unloadLibrary(library.id);
    }
    await persistLibraryPreferences();
    rebuildPalette();
    reportSuccess(enabled ? 'Library enabled.' : 'Library disabled.');
  });
}
//=============================================================================
export function selectLibraryBlocks(blockIds: string[]): void {
  if (blockIds.length === 0) return;
  emitAppEvent('nf-select-block', { blockId: blockIds[0], blockIds });
}
//=============================================================================
