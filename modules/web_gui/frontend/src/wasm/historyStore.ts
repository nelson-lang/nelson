//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
const HISTORY_KEY = 'nelson-webview.wasm.history.v1';
const MAX_HISTORY_ENTRIES = 5000;
const HISTORY_HEADER_PATTERN = /^%% -- \d{2}-\d{2}-\d{4} \d{2}:\d{2}:\d{2} -- %%$/;
export interface WasmHistoryStorage {
  getItem(key: string): string | null;
  setItem(key: string, value: string): void;
  removeItem(key: string): void;
}
//=============================================================================
export function defaultHistoryStorage(): WasmHistoryStorage | null {
  try {
    return typeof localStorage === 'undefined' ? null : localStorage;
  } catch {
    return null;
  }
}
//=============================================================================
export function loadWasmHistory(storage: WasmHistoryStorage | null): string[] {
  if (!storage) return [];
  try {
    const parsed: unknown = JSON.parse(storage.getItem(HISTORY_KEY) ?? '[]');
    if (!Array.isArray(parsed)) return [];
    return parsed
      .filter((entry): entry is string => typeof entry === 'string')
      .slice(-MAX_HISTORY_ENTRIES);
  } catch {
    return [];
  }
}
//=============================================================================
function pad2(value: number): string {
  return value.toString().padStart(2, '0');
}
//=============================================================================
export function createWasmHistoryHeader(now = new Date()): string {
  const month = pad2(now.getMonth() + 1);
  const day = pad2(now.getDate());
  const year = now.getFullYear().toString();
  const hour = pad2(now.getHours());
  const minutes = pad2(now.getMinutes());
  const seconds = pad2(now.getSeconds());
  return `%% -- ${month}-${day}-${year} ${hour}:${minutes}:${seconds} -- %%`;
}
//=============================================================================
export function isWasmHistoryHeader(entry: string): boolean {
  return HISTORY_HEADER_PATTERN.test(entry);
}
//=============================================================================
export function loadWasmHistoryForSession(
  storage: WasmHistoryStorage | null,
  now = new Date()
): string[] {
  const entries = loadWasmHistory(storage);
  if (!entries.length || !isWasmHistoryHeader(entries[entries.length - 1])) {
    entries.push(createWasmHistoryHeader(now));
    saveWasmHistory(storage, entries);
  }
  return entries;
}
//=============================================================================
export function clearWasmHistoryForSession(
  storage: WasmHistoryStorage | null,
  now = new Date()
): string[] {
  const entries = [createWasmHistoryHeader(now)];
  saveWasmHistory(storage, entries);
  return entries;
}
//=============================================================================
export function saveWasmHistory(storage: WasmHistoryStorage | null, entries: string[]): void {
  if (!storage) return;
  try {
    if (entries.length === 0) storage.removeItem(HISTORY_KEY);
    else storage.setItem(HISTORY_KEY, JSON.stringify(entries.slice(-MAX_HISTORY_ENTRIES)));
  } catch {
    // History remains available for the current browser session.
  }
}
//=============================================================================
