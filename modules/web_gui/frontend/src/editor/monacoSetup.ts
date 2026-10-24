//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Self-hosted Monaco setup (no CDN; required under the WebUI CSP).
// Wires the base editor worker via Vite and registers a minimal Nelson
// language for syntax highlighting.
import { loader } from '@monaco-editor/react';
//=============================================================================
// Editor core + all EDITOR contributions (context menu, suggest, hover,
// parameter hints, find, etc.) but NOT the ~80 built-in language grammars.
// `edcore.main` re-exports the API and registers the contributions as a side
// effect, unlike bare `editor.api`, which ships no context menu and no suggest
// widget. Only the Nelson language is registered below.
import * as monaco from 'monaco-editor/esm/vs/editor/edcore.main';
//=============================================================================
// Inline the editor worker into the main bundle (?worker&inline → base64 blob)
// so there is no separate worker file to ship.
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker&inline';
import { NELSON_KEYWORDS } from './nelsonSyntax';
//=============================================================================
let configured = false;
export function setupMonaco(): void {
  if (configured) return;
  configured = true;

  (self as unknown as { MonacoEnvironment: monaco.Environment }).MonacoEnvironment = {
    getWorker() {
      return new EditorWorker();
    },
  };

  // Use the bundled monaco instance instead of loading from a CDN.
  loader.config({ monaco });

  monaco.languages.register({ id: 'nelson' });
  monaco.languages.setMonarchTokensProvider('nelson', {
    keywords: [...NELSON_KEYWORDS],
    tokenizer: {
      root: [
        [/%.*$/, 'comment'],
        [/\/\/.*$/, 'comment'],
        [/"([^"\\]|\\.)*"/, 'string'],
        [/'([^'\\]|\\.)*'/, 'string'],
        [/\b\d+(\.\d+)?([eE][-+]?\d+)?\b/, 'number'],
        [/[A-Za-z_]\w*/, { cases: { '@keywords': 'keyword', '@default': 'identifier' } }],
        [/[+\-*/=<>~&|^]/, 'operator'],
      ],
    },
  });
}
//=============================================================================
