//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { getDockApi } from '../desktop/desktop';
//=============================================================================
export type NotebookCommand = 'open' | 'save' | 'saveAs' | 'saveM' | 'runAll' | 'closeDocument';
export const NOTEBOOK_COMMAND_EVENT = 'nelson-notebook.command';
//=============================================================================
/** Send a menu-bar command to the active Notebook document. */
export function sendNotebookCommand(command: NotebookCommand): void {
  if (command === 'closeDocument') {
    getDockApi()?.activePanel?.api.close();
    return;
  }
  window.dispatchEvent(new CustomEvent(NOTEBOOK_COMMAND_EVENT, { detail: { command } }));
}
//=============================================================================
/** Let a document handle the commands while `isActive` says it is the one addressed. */
export function onNotebookCommand(
  isActive: () => boolean,
  handle: (command: NotebookCommand) => void
): () => void {
  const listener = (event: Event) => {
    const command = (event as CustomEvent<{ command?: NotebookCommand }>).detail?.command;
    if (command && isActive()) handle(command);
  };
  window.addEventListener(NOTEBOOK_COMMAND_EVENT, listener);
  return () => window.removeEventListener(NOTEBOOK_COMMAND_EVENT, listener);
}
//=============================================================================
