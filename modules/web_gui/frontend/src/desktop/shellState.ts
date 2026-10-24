//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useSyncExternalStore } from 'react';
import { getPersistentItem, setPersistentItem } from './persistentStorage';
//=============================================================================
export interface ShellVisibility {
  toolbar: boolean;
  rail: boolean;
  status: boolean;
}
//=============================================================================
const STORAGE_KEY = 'nelson-webview.shell.visibility';
const DEFAULT_STATE: ShellVisibility = { toolbar: true, rail: true, status: true };
const listeners = new Set<() => void>();
function readState(): ShellVisibility {
  if (typeof window === 'undefined') return DEFAULT_STATE;
  try {
    const saved = JSON.parse(getPersistentItem(STORAGE_KEY) ?? '{}') as Partial<ShellVisibility>;
    return {
      toolbar: saved.toolbar ?? true,
      rail: saved.rail ?? true,
      status: saved.status ?? true,
    };
  } catch {
    return DEFAULT_STATE;
  }
}
//=============================================================================
let state = readState();
export function getShellVisibility(): ShellVisibility {
  return state;
}
//=============================================================================
export function setShellVisibility<K extends keyof ShellVisibility>(
  key: K,
  visible: boolean
): void {
  if (state[key] === visible) return;
  state = { ...state, [key]: visible };
  try {
    setPersistentItem(STORAGE_KEY, JSON.stringify(state));
  } catch {
    // A storage failure must not prevent the UI from changing for this session.
  }
  listeners.forEach((listener) => listener());
}
//=============================================================================
export function subscribeShellVisibility(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}
//=============================================================================
export function useShellVisibility(): ShellVisibility {
  return useSyncExternalStore(subscribeShellVisibility, getShellVisibility, () => DEFAULT_STATE);
}
//=============================================================================
