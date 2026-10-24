//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Most-recently-opened editor files, persisted by the desktop store.
import { getPersistentItem, removePersistentItem, setPersistentItem } from './persistentStorage';
//=============================================================================
const KEY = 'nelson-webview.recentFiles';
const MAX = 10;
export function getRecentFiles(): string[] {
  try {
    const raw = getPersistentItem(KEY);
    const list = raw ? (JSON.parse(raw) as unknown) : [];
    return Array.isArray(list) ? list.filter((x): x is string => typeof x === 'string') : [];
  } catch {
    return [];
  }
}
//=============================================================================
export function addRecentFile(path: string): void {
  if (!path) return;
  const next = [path, ...getRecentFiles().filter((p) => p !== path)].slice(0, MAX);
  setPersistentItem(KEY, JSON.stringify(next));
}
//=============================================================================
export function clearRecentFiles(): void {
  removePersistentItem(KEY);
}
//=============================================================================
