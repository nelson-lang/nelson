//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useMemo, useSyncExternalStore } from 'react';
import { AssistantPane } from '@nelson/assistant';
import type { AssistantBackend, AssistantCodeAction } from '@nelson/assistant';
import { rpc } from '../rpc/client';
import { subscribeToolEvents } from '../desktop/webTools';
import { loadAssistantHistory } from '../desktop/assistantHistory';
import { isWasmRuntime } from '../desktop/platform';
import { getDesktopCapabilities, subscribeDesktopCapabilities } from '../desktop/capabilities';
import { PermissionModeSelect } from '../components/PermissionModeSelect';
import { t } from '../i18n';
//=============================================================================
// Editor-side wrapper around the shared AssistantPane, embedded beside the code
// so the assistant is reached the same way as in the notebook. Prompts/models
// use the assistant.* RPCs and events arrive on the 'assistant' channel; the
// action under each chat code block inserts the code at the editor cursor.
const EDITOR_EXAMPLE_PROMPTS = [
  'Write a function that reverses a vector',
  'Insert a loop that sums 1 to n',
  'Add input checks to this function',
  'Explain what this code does',
];
//=============================================================================
export function EditorAssistant({
  targetLabel,
  onInsert,
}: {
  targetLabel: string;
  onInsert: (code: string) => void;
}): JSX.Element | null {
  const assistantEnabled = useSyncExternalStore(
    subscribeDesktopCapabilities,
    () => getDesktopCapabilities().assistant === true
  );
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
      runCode: (code) => onInsert(code),
      // Drive the ACP reply poll from the pane (the 'web' surface runs with
      // driver='external', so no Nelson timer advances it). Fire-and-forget.
      step: () => {
        void rpc('assistant.step', {}).catch(() => undefined);
      },
      // Re-display the 'web' surface's stored conversation on open (shared with
      // the docked pane, since both drive the same surface).
      loadHistory: () => loadAssistantHistory(),
    }),
    [onInsert]
  );
  const codeActions = useMemo<AssistantCodeAction[]>(
    () => [{ label: t('Insert at cursor'), run: onInsert }],
    [onInsert]
  );
  // Opt-in: hidden unless NELSON_ASSISTANT_AI=1 (off by default for 2.0).
  if (!assistantEnabled) {
    return null;
  }
  return (
    <div className="assistant-stack">
      <PermissionModeSelect />
      <AssistantPane
        backend={backend}
        t={t}
        examplePrompts={EDITOR_EXAMPLE_PROMPTS}
        targetLabel={targetLabel}
        codeActions={codeActions}
        offerAcp={!isWasmRuntime()}
      />
    </div>
  );
}
//=============================================================================
