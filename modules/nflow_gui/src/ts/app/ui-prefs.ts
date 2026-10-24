//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Persistent UI preferences. localStorage alone does not survive sessions in
// production because the webui host picks a random port (a new origin every
// launch): preferences are therefore stored in a JSON file inside the host
// preferences directory (same mechanism as the grid settings), and mirrored
// to localStorage as a fallback for browser-only runs.
//=============================================================================
import { getPreferencesPath } from './host-utils.js';
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
import { emitAppEvent } from './app-events.js';
//=============================================================================
const FILE_NAME = 'nflow-ui-prefs.json';
const SCHEMA_KEY = '_schemaVersion';
const SCHEMA_VERSION = '2';
const MANAGED_PREFIXES = [
  'nf-sidebar-',
  'nf-inspector-',
  'nf-console-',
  'nf-minimap-',
  'nf-canvas-',
  'nf-simulation-toolbar-',
  'nf-library-group-',
  'nf-codegen-',
  'nf-diagnostics-',
  'nf-dialog-',
  'nf-document-view-',
  'nf-window-geometry',
  'nf-scroll-',
  'nf-new-diagram-',
];
//=============================================================================
let cache: Record<string, unknown> = {};
let loaded = false;
let saveTimer: ReturnType<typeof setTimeout> | null = null;
let replaceOnNextSave = false;
let lifecycleBound = false;
const deletedKeys = new Set<string>();
const dirtyKeys = new Set<string>();
async function prefsFilePath(): Promise<string | null> {
  try {
    const dir = await getPreferencesPath();
    if (!dir) return null;
    const sep = dir.includes('\\') && !dir.includes('/') ? '\\' : '/';
    return dir.endsWith('/') || dir.endsWith('\\') ? dir + FILE_NAME : dir + sep + FILE_NAME;
  } catch {
    return null;
  }
}
//=============================================================================
export async function loadUiPrefs(): Promise<Record<string, unknown>> {
  if (loaded) return cache;
  loaded = true;
  try {
    const path = await prefsFilePath();
    if (canUseNFlowRpc('nflow.file.readText') && path) {
      const respText = await nflowRpc<string>('nflow.file.readText', { path });
      let content = respText;
      try {
        const obj = JSON.parse(respText) as { success?: boolean; content?: string };
        if (obj && typeof obj === 'object' && 'success' in obj) {
          content = obj.success && typeof obj.content === 'string' ? obj.content : '';
        }
      } catch {
        /* raw content */
      }
      if (content) {
        const data = JSON.parse(content) as Record<string, unknown>;
        if (data && typeof data === 'object') cache = { ...data };
      }
    }
  } catch {
    /* no host prefs - fall back to localStorage-only behavior */
  }
  const migrated = migrateLegacyPreferences();
  cache[SCHEMA_KEY] = SCHEMA_VERSION;
  bindLifecycleFlush();
  if (migrated) scheduleSave();
  return cache;
}
//=============================================================================
function migrateLegacyPreferences(): boolean {
  let migrated = false;
  try {
    if (cache['nf-sidebar-width'] === undefined) {
      const width = localStorage.getItem('sidebarWidth');
      if (width) {
        cache['nf-sidebar-width'] = width;
        dirtyKeys.add('nf-sidebar-width');
        migrated = true;
      }
    }
    if (cache['nf-sidebar-open'] === undefined) {
      const collapsed = localStorage.getItem('sidebarCollapsed');
      if (collapsed !== null) {
        cache['nf-sidebar-open'] = collapsed === 'true' ? '0' : '1';
        dirtyKeys.add('nf-sidebar-open');
        migrated = true;
      }
    }
    localStorage.removeItem('sidebarWidth');
    localStorage.removeItem('sidebarCollapsed');
  } catch {
    /* localStorage is optional */
  }
  return migrated;
}
//=============================================================================
async function readHostPrefs(path: string): Promise<Record<string, unknown>> {
  try {
    const response = await nflowRpc<string>('nflow.file.readText', { path });
    let content = response;
    try {
      const envelope = JSON.parse(response) as { success?: boolean; content?: string };
      if (envelope && typeof envelope === 'object' && 'success' in envelope) {
        content = envelope.success && typeof envelope.content === 'string' ? envelope.content : '';
      }
    } catch {
      /* raw content */
    }
    const parsed = content ? (JSON.parse(content) as Record<string, unknown>) : {};
    return parsed && typeof parsed === 'object' ? parsed : {};
  } catch {
    return {};
  }
}
//=============================================================================
async function saveNow(): Promise<void> {
  if (saveTimer !== null) {
    clearTimeout(saveTimer);
    saveTimer = null;
  }
  try {
    const path = await prefsFilePath();
    if (!canUseNFlowRpc('nflow.file.writeText') || !path) return;
    const latest = replaceOnNextSave ? {} : await readHostPrefs(path);
    const dirtySnapshot = new Set(dirtyKeys);
    const deletedSnapshot = new Set(deletedKeys);
    const updates = new Map<string, unknown>();
    dirtySnapshot.forEach((key) => updates.set(key, cache[key]));
    const merged: Record<string, unknown> = { ...latest, [SCHEMA_KEY]: SCHEMA_VERSION };
    if (replaceOnNextSave) {
      Object.assign(merged, cache);
    } else {
      updates.forEach((value, key) => {
        merged[key] = value;
      });
    }
    deletedSnapshot.forEach((key) => delete merged[key]);
    await nflowRpc('nflow.file.writeText', {
      content: JSON.stringify(merged, null, 2),
      path,
    });
    dirtySnapshot.forEach((key) => {
      if (cache[key] === updates.get(key)) dirtyKeys.delete(key);
    });
    deletedSnapshot.forEach((key) => deletedKeys.delete(key));
    const pendingUpdates = new Map<string, unknown>();
    dirtyKeys.forEach((key) => pendingUpdates.set(key, cache[key]));
    cache = { ...merged };
    pendingUpdates.forEach((value, key) => {
      cache[key] = value;
    });
    deletedKeys.forEach((key) => delete cache[key]);
    replaceOnNextSave = false;
  } catch (error) {
    if (typeof document !== 'undefined') {
      emitAppEvent('nf-ui-prefs-error', { error });
    }
  }
}
//=============================================================================
function scheduleSave(): void {
  if (saveTimer !== null) clearTimeout(saveTimer);
  saveTimer = setTimeout(() => {
    void saveNow();
  }, 200);
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
export function getUiPref(key: string, fallback: string | null = null): string | null {
  const v = cache[key];
  if (typeof v === 'string') return v;
  try {
    const ls = localStorage.getItem(key);
    if (ls !== null) return ls;
  } catch {
    /* ignore */
  }
  return fallback;
}
//=============================================================================
export function setUiPref(key: string, value: string): void {
  cache[key] = value;
  dirtyKeys.add(key);
  deletedKeys.delete(key);
  try {
    localStorage.setItem(key, value);
  } catch {
    /* ignore */
  }
  scheduleSave();
}
//=============================================================================
export function removeUiPref(key: string): void {
  delete cache[key];
  dirtyKeys.delete(key);
  deletedKeys.add(key);
  try {
    localStorage.removeItem(key);
  } catch {
    /* ignore */
  }
  scheduleSave();
}
//=============================================================================
export async function flushUiPrefs(): Promise<void> {
  await saveNow();
}
//=============================================================================
export async function resetUiPrefs(): Promise<void> {
  const keys = new Set(Object.keys(cache).filter((key) => key !== SCHEMA_KEY));
  try {
    for (let index = 0; index < localStorage.length; index += 1) {
      const key = localStorage.key(index);
      if (key && MANAGED_PREFIXES.some((prefix) => key.startsWith(prefix))) keys.add(key);
    }
  } catch {
    /* localStorage is optional */
  }
  cache = { [SCHEMA_KEY]: SCHEMA_VERSION };
  dirtyKeys.clear();
  deletedKeys.clear();
  dirtyKeys.clear();
  replaceOnNextSave = true;
  keys.forEach((key) => {
    try {
      localStorage.removeItem(key);
    } catch {
      /* ignore */
    }
  });
  await saveNow();
  if (typeof document !== 'undefined') {
    emitAppEvent('nf-ui-prefs-reset');
  }
}
//=============================================================================
export function _resetUiPrefsForTests(): void {
  cache = {};
  loaded = false;
  replaceOnNextSave = false;
  deletedKeys.clear();
  if (saveTimer !== null) clearTimeout(saveTimer);
  saveTimer = null;
}
//=============================================================================
