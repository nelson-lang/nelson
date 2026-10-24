//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tiny bus so other panels (e.g. the History browser's "Copy to Command
// Window") can drop text into the Command Window's input line without a
// backend round-trip. The ConsolePanel registers the setter on mount.
type Setter = (text: string) => void;
let setInput: Setter | null = null;
export function registerConsoleInput(fn: Setter | null): void {
  setInput = fn;
}
//=============================================================================
export function sendToConsoleInput(text: string): void {
  setInput?.(text);
}
//=============================================================================
