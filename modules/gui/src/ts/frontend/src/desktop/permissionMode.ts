//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Session-wide assistant permission mode, shared by every assistant surface so
// their selectors stay in sync. The engine is the source of truth: read it once
// on first use, and push every change back over the assistant.* RPCs.
//   always_ask: prompt on every tool request (default)
//   auto:       auto-allow safe requests, prompt only risky ones
//   full:       never prompt, auto-allow everything
import { rpc } from '../rpc/client';
//=============================================================================
export type PermissionMode = 'always_ask' | 'auto' | 'full';
const MODES: readonly PermissionMode[] = ['always_ask', 'auto', 'full'];
//=============================================================================
let current: PermissionMode = 'always_ask';
let loaded = false;
const subscribers = new Set<(mode: PermissionMode) => void>();
//=============================================================================
function normalize(mode: unknown): PermissionMode | null {
  return typeof mode === 'string' && (MODES as readonly string[]).includes(mode)
    ? (mode as PermissionMode)
    : null;
}
//=============================================================================
function notify(): void {
  subscribers.forEach((cb) => cb(current));
}
//=============================================================================
// Read the engine's current mode once; the engine owns the value across surfaces.
async function load(): Promise<void> {
  if (loaded) {
    return;
  }
  loaded = true;
  try {
    const result = await rpc<{ mode?: string }>('assistant.getPermissionMode', {});
    const mode = normalize(result.mode);
    if (mode && mode !== current) {
      current = mode;
      notify();
    }
  } catch {
    /* keep the default until the engine answers */
  }
}
//=============================================================================
export function getPermissionMode(): PermissionMode {
  return current;
}
//=============================================================================
export function setPermissionMode(mode: PermissionMode): void {
  if (normalize(mode) === null || mode === current) {
    return;
  }
  current = mode;
  notify();
  void rpc('assistant.setPermissionMode', { mode }).catch(() => undefined);
}
//=============================================================================
export function subscribePermissionMode(cb: (mode: PermissionMode) => void): () => void {
  subscribers.add(cb);
  void load();
  return () => {
    subscribers.delete(cb);
  };
}
//=============================================================================
