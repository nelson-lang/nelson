//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useMemo } from 'react';
import { AssistantPane } from '@nelson/assistant';
import type { AssistantBackend, AssistantCodeAction } from '@nelson/assistant';
import { rpc } from '../rpc/client';
import { subscribeToolEvents } from '../desktop/webTools';
import { isWasmRuntime } from '../desktop/platform';
import { PermissionModeSelect } from '../components/PermissionModeSelect';
import { t } from '../i18n';
//=============================================================================
// Notebook-side wrapper around the shared AssistantPane. Prompts and models go
// through the same assistant.* RPCs as the docked web desktop pane, and streamed
// events arrive on the 'assistant' tool-event channel. The agent adds cells via
// the insert_into_nelson_notebook tool (the notebookInsert channel); the actions
// under each chat code block add that code as a new cell (and optionally run it),
// so the target is the notebook rather than the detached console.
const NOTEBOOK_EXAMPLE_PROMPTS = [
  'Add a cell that computes 2 + 2',
  'Plot sin(x) from -pi to pi in a new cell',
  'Create a 3-by-3 magic square and show its row sums',
  'Explain what eig does',
];
//=============================================================================
export function NotebookAssistant({
  targetLabel,
  onAddCell,
}: {
  targetLabel: string;
  onAddCell: (code: string, run: boolean) => void;
}): JSX.Element {
  const backend = useMemo<AssistantBackend>(
    () => ({
      listModels: () =>
        rpc<{ models?: string[] }>('assistant.models', {}).then((result) =>
          Array.isArray(result.models) ? result.models : []
        ),
      sendPrompt: (prompt, model) =>
        rpc<{ accepted?: boolean }>('assistant.prompt', model ? { prompt, model } : { prompt }),
      subscribe: (handler) => subscribeToolEvents('assistant', handler),
      interrupt: () => {
        void rpc('term.interrupt', {}).catch(() => undefined);
      },
      // Fallback for the default Run button; superseded by codeActions below.
      runCode: (code) => onAddCell(code, true),
      // Drive the ACP reply poll from the pane (the 'web' surface runs with
      // driver='external', so no Nelson timer advances it). Fire-and-forget.
      step: () => {
        void rpc('assistant.step', {}).catch(() => undefined);
      },
    }),
    [onAddCell]
  );
  const codeActions = useMemo<AssistantCodeAction[]>(
    () => [
      { label: t('Add cell'), run: (code) => onAddCell(code, false) },
      { label: t('Add & run'), run: (code) => onAddCell(code, true) },
    ],
    [onAddCell]
  );
  return (
    <div className="assistant-stack">
      <PermissionModeSelect />
      <AssistantPane
        backend={backend}
        t={t}
        examplePrompts={NOTEBOOK_EXAMPLE_PROMPTS}
        targetLabel={targetLabel}
        codeActions={codeActions}
        offerAcp={!isWasmRuntime()}
      />
    </div>
  );
}
//=============================================================================
