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
import { loadAssistantHistory } from '../desktop/assistantHistory';
import { isWasmRuntime } from '../desktop/platform';
import { PermissionModeSelect } from '../components/PermissionModeSelect';
import { t } from '../i18n';
//=============================================================================
// Web desktop wrapper around the shared AssistantPane. It supplies the web
// backend: prompts and models go through the assistant.* RPCs, streamed events
// arrive on the 'assistant' tool-event channel, and the interpreter interrupt /
// code run reuse the term.* RPCs. The pane markup and behavior are shared with
// NFlow (see modules/webview/frontend/assistant).
export function AssistantPanel(): JSX.Element {
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
      runCode: (code) => {
        void rpc('term.input', { line: code }).catch(() => undefined);
      },
      // Drive the ACP reply poll from the pane (the engine runs the turn with
      // driver='external', so no Nelson timer advances it). Fire-and-forget; the
      // backend steps the 'web' surface quietly on the interpreter thread.
      step: () => {
        void rpc('assistant.step', {}).catch(() => undefined);
      },
      // Restore the 'web' surface's stored conversation so a restart re-displays
      // the prior turns. Best-effort: an unreachable RPC just starts empty.
      loadHistory: () => loadAssistantHistory(),
      // Forget the shared conversation and reset the live agent (same push-to-
      // interpreter path as a prompt). The pane clears its own transcript.
      newConversation: () =>
        rpc('term.input', { line: "nelson.ai.internal.newConversation('web')" })
          .then(() => undefined)
          .catch(() => undefined),
    }),
    []
  );
  // Make the target explicit: this pane runs code in the live console.
  const codeActions = useMemo<AssistantCodeAction[]>(
    () => [
      {
        label: t('Run in console'),
        run: (code) => {
          void rpc('term.input', { line: code }).catch(() => undefined);
        },
      },
    ],
    []
  );
  // The browser engine has no local process, so the claude-agent-acp backend is
  // hidden there; native desktop keeps offering it.
  return (
    <div className="assistant-stack">
      <PermissionModeSelect />
      <AssistantPane
        backend={backend}
        t={t}
        targetLabel={t('console')}
        codeActions={codeActions}
        offerAcp={!isWasmRuntime()}
      />
    </div>
  );
}
//=============================================================================
