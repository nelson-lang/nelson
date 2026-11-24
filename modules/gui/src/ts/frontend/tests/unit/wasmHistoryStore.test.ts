//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import {
  clearWasmHistoryForSession,
  createWasmHistoryHeader,
  isWasmHistoryHeader,
  loadWasmHistoryForSession,
  saveWasmHistory,
  type WasmHistoryStorage,
} from '../../src/wasm/historyStore';
//=============================================================================
class MemoryStorage implements WasmHistoryStorage {
  private readonly values = new Map<string, string>();
  getItem(key: string): string | null {
    return this.values.get(key) ?? null;
  }
  //=============================================================================
  setItem(key: string, value: string): void {
    this.values.set(key, value);
  }
  //=============================================================================
  removeItem(key: string): void {
    this.values.delete(key);
  }
}
//=============================================================================
describe('WebAssembly command history storage', () => {
  it('formats session headers like the native history manager', () => {
    const header = createWasmHistoryHeader(new Date(2026, 9, 4, 8, 7, 6));

    expect(header).toBe('%% -- 10-04-2026 08:07:06 -- %%');
    expect(isWasmHistoryHeader(header)).toBe(true);
  });

  it('adds a session header when loading persisted history', () => {
    const storage = new MemoryStorage();
    saveWasmHistory(storage, ['a = 1']);

    const entries = loadWasmHistoryForSession(storage, new Date(2026, 0, 2, 3, 4, 5));

    expect(entries).toEqual(['a = 1', '%% -- 01-02-2026 03:04:05 -- %%']);
    expect(loadWasmHistoryForSession(storage, new Date(2026, 0, 2, 3, 4, 6))).toEqual(entries);
  });

  it('keeps a fresh session header after clearing history', () => {
    const storage = new MemoryStorage();
    saveWasmHistory(storage, ['old']);

    const entries = clearWasmHistoryForSession(storage, new Date(2026, 1, 3, 4, 5, 6));

    expect(entries).toEqual(['%% -- 02-03-2026 04:05:06 -- %%']);
  });
});
//=============================================================================
