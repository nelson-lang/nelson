//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Scriptable terminal panels register here under their engine-assigned handle so
// the backend's terminal.* pushes (input / clear / focus / props / read) reach
// the right shell panel. A panel without a handle (the "New Terminal" menu) is
// not scriptable and does not register.
//=============================================================================
export interface TerminalPanelApi {
  runCommand(command: string): void;
  clear(): void;
  focus(): void;
  setProps(props: { name?: string; theme?: string }): void;
  getTranscript(): string;
}
//=============================================================================
const registry = new Map<number, TerminalPanelApi>();
//=============================================================================
export function registerTerminal(handle: number, api: TerminalPanelApi): () => void {
  registry.set(handle, api);
  return () => {
    if (registry.get(handle) === api) {
      registry.delete(handle);
    }
  };
}
//=============================================================================
export function getTerminal(handle: number): TerminalPanelApi | undefined {
  return registry.get(handle);
}
//=============================================================================
