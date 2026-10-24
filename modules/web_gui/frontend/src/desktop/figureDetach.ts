//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Detached-figure registry, shared between the desktop window and the
// standalone figure popups through localStorage (same origin).
//
// A popped-out figure must survive a desktop reload without being re-docked
// by the figure poll, and must return to the dock when its window goes away,
// even if the direct `win.closed` watcher died with the previous desktop
// page. So the popup heartbeats its entry every second, and the desktop
// restores any entry whose heartbeat went stale.
type Entry = { title: string; beat: number };
const KEY = 'nelson-webview.detachedFigures';
const BEAT_MS = 1000;
// A popup reload pauses the heartbeat for a moment; stay well above that.
const STALE_MS = 4000;
function load(): Record<string, Entry> {
  try {
    const raw = localStorage.getItem(KEY);
    return raw ? (JSON.parse(raw) as Record<string, Entry>) : {};
  } catch {
    return {};
  }
}
//=============================================================================
function save(map: Record<string, Entry>): void {
  try {
    if (Object.keys(map).length === 0) {
      localStorage.removeItem(KEY);
    } else {
      localStorage.setItem(KEY, JSON.stringify(map));
    }
  } catch {
    /* private mode: detach won't survive a reload */
  }
}
//=============================================================================
export function markDetached(handle: number, title: string): void {
  const map = load();
  map[String(handle)] = { title, beat: Date.now() };
  save(map);
}
//=============================================================================
export function clearDetached(handle: number): void {
  const map = load();
  if (String(handle) in map) {
    delete map[String(handle)];
    save(map);
  }
}
//=============================================================================
export function isDetached(handle: number): boolean {
  return String(handle) in load();
}
//=============================================================================
// Standalone window side: keep the entry alive while this window exists.
export function startDetachedHeartbeat(handle: number): () => void {
  const beat = () => {
    const map = load();
    const entry = map[String(handle)];
    if (entry) {
      entry.beat = Date.now();
      save(map);
    }
  };
  beat();
  const id = window.setInterval(beat, BEAT_MS);
  return () => window.clearInterval(id);
}
//=============================================================================
// Desktop side: bring back any detached figure whose popup stopped beating.
export function startDetachedWatcher(open: (handle: number, title: string) => void): () => void {
  const sweep = () => {
    const map = load();
    const now = Date.now();
    for (const [key, entry] of Object.entries(map)) {
      if (now - entry.beat > STALE_MS) {
        delete map[key];
        save(map);
        open(Number(key), entry.title);
      }
    }
  };
  const id = window.setInterval(sweep, BEAT_MS);
  return () => window.clearInterval(id);
}
//=============================================================================
