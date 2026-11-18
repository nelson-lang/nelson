//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Routes a filename-addressed assistant insert (insert_into_nelson_file) to the
// right editor. The editor panel is lazy-loaded, so this eagerly-installed
// router opens the target file even when no editor is open yet; the panel for
// that file then appends the code via takePendingFileInsertFor once it is ready.
// A target-less editorInsert ({text} only) is handled by the focused editor
// panel instead and never reaches here.
import { subscribeToolEvents } from './webTools';
import { openEditor } from './desktop';
import { samePath } from '../editor/debugDecorations';
//=============================================================================
let pendingFileInsert: { filename: string; text: string; replace: boolean } | null = null;
let installed = false;
//=============================================================================
export function installEditorInsertRouter(): void {
  if (installed) {
    return;
  }
  installed = true;
  subscribeToolEvents('assistant', (channel, raw) => {
    if (channel !== 'editorInsert') {
      return;
    }
    let text = '';
    let filename = '';
    let replace = false;
    try {
      const payload = JSON.parse(raw) as { text?: string; filename?: string; replace?: boolean };
      text = payload.text ?? '';
      filename = payload.filename ?? '';
      replace = payload.replace ?? false;
    } catch {
      return;
    }
    if (text === '' || filename === '') {
      return;
    }
    pendingFileInsert = { filename, text, replace };
    openEditor(filename);
    // Nudge the (possibly already open) panel to consume once; a freshly opened
    // panel instead consumes when its content becomes ready.
    setTimeout(
      () =>
        window.dispatchEvent(
          new CustomEvent('nelson-webview.editor-insert', { detail: { filename } })
        ),
      0
    );
  });
}
//=============================================================================
// If a pending file insert targets `path`, return its code (and whether it
// replaces the whole buffer) and clear it (so it lands exactly once); otherwise
// null.
export function takePendingFileInsertFor(path: string): { text: string; replace: boolean } | null {
  if (pendingFileInsert && samePath(path, pendingFileInsert.filename)) {
    const { text, replace } = pendingFileInsert;
    pendingFileInsert = null;
    return { text, replace };
  }
  return null;
}
//=============================================================================
