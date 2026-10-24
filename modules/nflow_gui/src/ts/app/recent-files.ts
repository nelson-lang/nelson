//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Recent files helper
// Persist to host preferences directory when available (via getPreferencesPath)
// Fallback to localStorage for immediate synchronous access.
//=============================================================================
import { getPreferencesPath } from './host-utils.js';
import { canUseNFlowRpc, nflowRpc } from './native-rpc.js';
//=============================================================================
const KEY = 'nf-recent-files-v1';
const PREF_FILE = 'nf-recent-files-v1.json';
const MAX_ENTRIES = 10;
function getFromLocalStorage(): string[] {
  try {
    const raw = localStorage.getItem(KEY);
    if (!raw) return [];
    const parsed = JSON.parse(raw);
    if (!Array.isArray(parsed)) return [];
    return parsed.map((p) => String(p));
  } catch (_e) {
    return [];
  }
}
//=============================================================================
export function getRecentFiles(): string[] {
  return getFromLocalStorage();
}
//=============================================================================
export function persistRecentFiles(list: string[]): void {
  try {
    const toSave = list.slice(0, MAX_ENTRIES);
    try {
      localStorage.setItem(KEY, JSON.stringify(toSave));
    } catch (_e) {}
    // Attempt to persist to preferences file asynchronously (best-effort)
    (async () => {
      try {
        const prefs = await getPreferencesPath();
        if (!prefs) return;
        const sep = prefs.includes('\\') ? '\\' : '/';
        const prefPath = prefs + sep + 'nflow' + sep + PREF_FILE;
        if (canUseNFlowRpc('nflow.file.writeText')) {
          await nflowRpc('nflow.file.writeText', {
            content: JSON.stringify(toSave),
            path: prefPath,
          });
        }
      } catch (_e) {
        // ignore write errors
      }
    })();
  } catch (_e) {
    // ignore
  }
}
//=============================================================================
export function addRecentFile(path: string | null | undefined): void {
  try {
    if (!path) return;
    const p = String(path);
    if (!p) return;
    const cur = getFromLocalStorage().filter((x) => x !== p);
    cur.unshift(p);
    persistRecentFiles(cur.slice(0, MAX_ENTRIES));
  } catch (_e) {
    // ignore
  }
}
//=============================================================================
export function clearRecentFiles(): void {
  try {
    try {
      localStorage.removeItem(KEY);
    } catch (_e) {}
    (async () => {
      try {
        const prefs = await getPreferencesPath();
        if (!prefs) return;
        const sep = prefs.includes('\\') ? '\\' : '/';
        const prefPath = prefs + sep + 'nflow' + sep + PREF_FILE;
        if (canUseNFlowRpc('nflow.file.writeText')) {
          await nflowRpc('nflow.file.writeText', { content: JSON.stringify([]), path: prefPath });
        }
      } catch (_e) {
        // ignore
      }
    })();
  } catch (_e) {
    // ignore
  }
}
//=============================================================================
// Loads the prefs file once and seeds localStorage so later reads stay synchronous.
export async function ensureRecentFilesLoaded(): Promise<void> {
  try {
    const prefs = await getPreferencesPath();
    if (!prefs) return;
    const sep = prefs.includes('\\') ? '\\' : '/';
    const prefPath = prefs + sep + 'nflow' + sep + PREF_FILE;
    if (!canUseNFlowRpc('nflow.file.readText')) return;
    try {
      const raw = await nflowRpc<string>('nflow.file.readText', { path: prefPath });
      const resp = JSON.parse(String(raw || '{}')) as Record<string, unknown>;
      if (resp && resp['success'] === true && typeof resp['content'] === 'string') {
        try {
          const parsed = JSON.parse(String(resp['content'] || '[]'));
          if (Array.isArray(parsed)) {
            const arr = parsed.map((p) => String(p)).slice(0, MAX_ENTRIES);
            try {
              localStorage.setItem(KEY, JSON.stringify(arr));
            } catch (_e) {}
          }
        } catch (_e) {
          // ignore JSON parse error
        }
      }
    } catch (_e) {
      // ignore read errors
    }
  } catch (_e) {
    // ignore
  }
}
//=============================================================================
export default null;
//=============================================================================
