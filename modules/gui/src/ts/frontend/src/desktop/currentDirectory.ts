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
import { rpc } from '../rpc/client';
//=============================================================================
export interface CurrentDirectoryState {
  cwd: string;
  separator: string;
  recentDirectories: string[];
}
//=============================================================================
const EMPTY: CurrentDirectoryState = { cwd: '', separator: '/', recentDirectories: [] };
let state = EMPTY;
let refreshPromise: Promise<CurrentDirectoryState> | null = null;
const listeners = new Set<() => void>();
function publish(next: CurrentDirectoryState): CurrentDirectoryState {
  state = next;
  listeners.forEach((listener) => listener());
  return state;
}
//=============================================================================
export function normalizeCurrentDirectory(
  result: Partial<CurrentDirectoryState>
): CurrentDirectoryState {
  const recentDirectories = Array.isArray(result.recentDirectories)
    ? result.recentDirectories.filter(
        (directory): directory is string => typeof directory === 'string' && directory.length > 0
      )
    : [];
  return {
    cwd: result.cwd ?? '',
    separator: result.separator || '/',
    recentDirectories: recentDirectories.slice(0, 10),
  };
}
//=============================================================================
export function getCurrentDirectory(): CurrentDirectoryState {
  return state;
}
//=============================================================================
export function refreshCurrentDirectory(): Promise<CurrentDirectoryState> {
  if (refreshPromise) return refreshPromise;
  refreshPromise = rpc<CurrentDirectoryState>('fs.cwd')
    .then((result) => publish(normalizeCurrentDirectory(result)))
    .finally(() => {
      refreshPromise = null;
    });
  return refreshPromise;
}
//=============================================================================
export function changeCurrentDirectory(path: string): Promise<CurrentDirectoryState> {
  return rpc<CurrentDirectoryState>('fs.chdir', { path }).then((result) =>
    publish(normalizeCurrentDirectory(result))
  );
}
//=============================================================================
export function subscribeCurrentDirectory(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}
//=============================================================================
export function useCurrentDirectory(): CurrentDirectoryState {
  return useSyncExternalStore(subscribeCurrentDirectory, getCurrentDirectory, () => EMPTY);
}
//=============================================================================
