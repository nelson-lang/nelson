//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// edcore.main re-exports the editor.api types (`export * from './editor.api'`)
// but ships no own .d.ts, so mirror the API types onto it.
declare module 'monaco-editor/esm/vs/editor/edcore.main' {
  export * from 'monaco-editor/esm/vs/editor/editor.api';
}
