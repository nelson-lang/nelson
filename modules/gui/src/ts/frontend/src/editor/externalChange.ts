//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { EditorFileStamp } from '../rpc/types';
//=============================================================================
export type ExternalChangeAction = 'none' | 'reload' | 'ask' | 'forget';
//=============================================================================
// What the editor does when a check finds the file on disk: reload, ask, or only track it.
export function externalChangeAction(
  known: EditorFileStamp | null | undefined,
  current: EditorFileStamp | null | undefined,
  dirty: boolean
): ExternalChangeAction {
  if (!known || !current) return 'none';
  // a deleted file keeps its text in the editor, without any question
  if (!current.exists) return known.exists ? 'forget' : 'none';
  if (known.exists && known.version === current.version) return 'none';
  return dirty ? 'ask' : 'reload';
}
//=============================================================================
// The reason part of an RPC failure, without the transport prefix.
export function rpcFailureReason(error: unknown): string {
  const text = error instanceof Error ? error.message : String(error ?? '');
  const match = /RPC "[^"]*": ([\s\S]*)$/.exec(text);
  return (match ? match[1] : text).trim();
}
//=============================================================================
// Content fingerprint for backends without file times (wasm, mock).
export function contentStamp(content: string | Uint8Array | null): EditorFileStamp {
  if (content === null) return { exists: false, version: '', size: 0 };
  const bytes = typeof content === 'string' ? new TextEncoder().encode(content) : content;
  let hash = 0x811c9dc5;
  for (let i = 0; i < bytes.length; i++) {
    hash ^= bytes[i];
    hash = Math.imul(hash, 0x01000193) >>> 0;
  }
  return { exists: true, version: `${hash.toString(16)}:${bytes.length}`, size: bytes.length };
}
//=============================================================================
