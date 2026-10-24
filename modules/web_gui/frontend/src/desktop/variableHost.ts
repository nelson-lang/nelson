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
//=============================================================================
interface VariableHostState {
  names: string[];
  active: string;
}
//=============================================================================
let state: VariableHostState = { names: [], active: '' };
const listeners = new Set<() => void>();
function publish(next: VariableHostState): void {
  state = next;
  listeners.forEach((listener) => listener());
}
//=============================================================================
export function selectVariable(name: string): void {
  if (!name) return;
  publish({
    names: state.names.includes(name) ? state.names : [...state.names, name],
    active: name,
  });
}
//=============================================================================
export function closeVariable(name: string): void {
  const index = state.names.indexOf(name);
  const names = state.names.filter((candidate) => candidate !== name);
  const active = state.active === name ? names[Math.max(0, index - 1)] ?? '' : state.active;
  publish({ names, active });
}
//=============================================================================
function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}
//=============================================================================
export function useVariableHost(): VariableHostState {
  return useSyncExternalStore(
    subscribe,
    () => state,
    () => ({ names: [], active: '' })
  );
}
//=============================================================================
