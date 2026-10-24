//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Store for `inserthtml` output. The content push (html.show) and the panel
// that displays it are decoupled: the panel is opened on the same push, so it
// mounts *after* the content already arrived. Buffering here means the panel
// reads what is already in and then follows further pushes; no lost first
// block (the bug where the panel opened empty).
import { onPush } from '../rpc/client';
//=============================================================================
const blocks: string[] = [];
const listeners = new Set<() => void>();
// Installed once, unconditionally, so blocks accumulate even before any panel
// (or App wiring) subscribes.
onPush('html.show', (p) => {
  const html = (p as { html?: string }).html;
  if (typeof html === 'string') {
    blocks.push(html);
    for (const l of listeners) l();
  }
});
//=============================================================================
export function getHtmlBlocks(): readonly string[] {
  return blocks;
}
//=============================================================================
export function subscribeHtml(cb: () => void): () => void {
  listeners.add(cb);
  return () => listeners.delete(cb);
}
//=============================================================================
