//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { rpc } from '../rpc/client';
//=============================================================================
const SCHEMA_VERSION = 1;
const SAVE_DELAY_MS = 200;
export const persistentStorageKeys = [
  'nelson-gui.layout.last',
  'nelson-gui.layout.named',
  'nelson-gui.layout.schema',
  'nelson-gui.theme',
  'nelson-gui.console.fontSize',
  'nelson-gui.console.fontFamily',
  'nelson-gui.console.wrap',
  'nelson-gui.editor.fontSize',
  'nelson-gui.editor.tabSize',
  'nelson-gui.recentFiles',
  'nelson-gui.shell.visibility',
  'nelson-gui.ws.cols',
] as const;
//=============================================================================
const PRESERVED_ON_INTERFACE_RESET = new Set(['nelson-gui.recentFiles']);
//=============================================================================
// Preferences were stored under the former 'nelson-webview.*' namespace before
// the module was renamed to 'gui'. These helpers migrate old values to the
// current 'nelson-gui.*' keys, one-shot and idempotent.
const KEY_PREFIX = 'nelson-gui.';
const LEGACY_KEY_PREFIX = 'nelson-webview.';
function legacyKeyFor(key: string): string {
  return LEGACY_KEY_PREFIX + key.slice(KEY_PREFIX.length);
}
function migrateLegacyLocalStorage(): void {
  try {
    for (const key of persistentStorageKeys) {
      const legacy = legacyKeyFor(key);
      const legacyValue = localStorage.getItem(legacy);
      if (legacyValue === null) continue;
      if (localStorage.getItem(key) === null) localStorage.setItem(key, legacyValue);
      localStorage.removeItem(legacy);
    }
  } catch {
    // Browser storage is optional; migration is best-effort.
  }
}
function remapLegacyKeys(value: unknown): Record<string, unknown> {
  // Accept both current and legacy-namespaced keys from the host envelope.
  if (!value || typeof value !== 'object') return {};
  const remapped: Record<string, unknown> = {};
  for (const [k, item] of Object.entries(value)) {
    const key = k.startsWith(LEGACY_KEY_PREFIX)
      ? KEY_PREFIX + k.slice(LEGACY_KEY_PREFIX.length)
      : k;
    if (remapped[key] === undefined) remapped[key] = item;
  }
  return remapped;
}
interface PreferencesEnvelope {
  schema: number;
  values: Record<string, string>;
}
//=============================================================================
let cache: Record<string, string> = {};
let loaded = false;
let dirty = false;
let revision = 0;
let saveTimer: ReturnType<typeof setTimeout> | null = null;
let lifecycleBound = false;
function isManagedKey(key: string): boolean {
  return (persistentStorageKeys as readonly string[]).includes(key);
}
//=============================================================================
function readLocalValues(): Record<string, string> {
  const values: Record<string, string> = {};
  try {
    for (const key of persistentStorageKeys) {
      const value = localStorage.getItem(key);
      if (value !== null) values[key] = value;
    }
  } catch {
    // Browser storage is optional; the in-memory cache remains available.
  }
  return values;
}
//=============================================================================
function normalizeValues(value: unknown): Record<string, string> {
  if (!value || typeof value !== 'object') return {};
  const normalized: Record<string, string> = {};
  for (const [key, item] of Object.entries(value)) {
    if (isManagedKey(key) && typeof item === 'string') normalized[key] = item;
  }
  return normalized;
}
//=============================================================================
function mirrorToLocalStorage(values: Record<string, string>): void {
  try {
    for (const key of persistentStorageKeys) {
      const value = values[key];
      if (value === undefined) localStorage.removeItem(key);
      else localStorage.setItem(key, value);
    }
  } catch {
    // Host-backed preferences remain authoritative when local storage fails.
  }
}
//=============================================================================
async function saveNow(): Promise<void> {
  if (saveTimer !== null) {
    clearTimeout(saveTimer);
    saveTimer = null;
  }
  if (!dirty) return;
  const snapshot = { ...cache };
  const savedRevision = revision;
  try {
    await rpc('prefs.save', { schema: SCHEMA_VERSION, values: snapshot });
    if (revision === savedRevision) dirty = false;
  } catch {
    window.dispatchEvent(new CustomEvent('nelson-gui.prefs-error'));
  }
}
//=============================================================================
function scheduleSave(): void {
  dirty = true;
  revision += 1;
  if (saveTimer !== null) clearTimeout(saveTimer);
  saveTimer = setTimeout(() => void saveNow(), SAVE_DELAY_MS);
}
//=============================================================================
function bindLifecycleFlush(): void {
  if (lifecycleBound || typeof window === 'undefined') return;
  lifecycleBound = true;
  window.addEventListener('pagehide', () => void saveNow());
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') void saveNow();
  });
}
//=============================================================================
export async function initializePersistentStorage(): Promise<void> {
  if (loaded) return;
  loaded = true;
  migrateLegacyLocalStorage();
  const localValues = readLocalValues();
  try {
    const result = await rpc<PreferencesEnvelope>('prefs.load');
    const hostValues = normalizeValues(remapLegacyKeys(result.values));
    cache = { ...localValues, ...hostValues };
    mirrorToLocalStorage(cache);
    if (Object.keys(localValues).some((key) => hostValues[key] === undefined)) scheduleSave();
  } catch {
    cache = localValues;
  }
  bindLifecycleFlush();
}
//=============================================================================
export function getPersistentItem(key: string): string | null {
  const cached = cache[key];
  if (cached !== undefined) return cached;
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
}
//=============================================================================
export function setPersistentItem(key: string, value: string): void {
  if (!isManagedKey(key)) throw new Error(`Unmanaged desktop preference: ${key}`);
  cache[key] = value;
  try {
    localStorage.setItem(key, value);
  } catch {
    // Keep the in-memory and host-backed value.
  }
  scheduleSave();
}
//=============================================================================
export function removePersistentItem(key: string): void {
  if (!isManagedKey(key)) throw new Error(`Unmanaged desktop preference: ${key}`);
  delete cache[key];
  try {
    localStorage.removeItem(key);
  } catch {
    // Keep the host-backed removal queued.
  }
  scheduleSave();
}
//=============================================================================
export async function flushPersistentStorage(): Promise<void> {
  await saveNow();
}
//=============================================================================
export async function resetInterfacePreferences(): Promise<void> {
  for (const key of persistentStorageKeys) {
    if (PRESERVED_ON_INTERFACE_RESET.has(key)) continue;
    delete cache[key];
    try {
      localStorage.removeItem(key);
    } catch {
      // Keep the host-backed removal queued.
    }
  }
  scheduleSave();
  await saveNow();
}
//=============================================================================
export function _resetPersistentStorageForTests(): void {
  cache = {};
  loaded = false;
  dirty = false;
  revision = 0;
  if (saveTimer !== null) clearTimeout(saveTimer);
  saveTimer = null;
}
//=============================================================================
