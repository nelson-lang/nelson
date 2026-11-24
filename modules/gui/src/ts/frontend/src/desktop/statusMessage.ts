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
let message = '';
let timer: number | undefined;
const listeners = new Set<() => void>();
export function showStatusMessage(text: string, duration = 3000): void {
  message = text;
  if (timer !== undefined) window.clearTimeout(timer);
  timer = window.setTimeout(() => {
    message = '';
    listeners.forEach((listener) => listener());
  }, duration);
  listeners.forEach((listener) => listener());
}
//=============================================================================
function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}
//=============================================================================
export function useStatusMessage(): string {
  return useSyncExternalStore(
    subscribe,
    () => message,
    () => ''
  );
}
//=============================================================================
