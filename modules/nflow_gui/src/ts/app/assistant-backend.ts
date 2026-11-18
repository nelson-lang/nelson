//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { AssistantBackend } from '@nelson/assistant';
import { nflowRpc, onNFlowPush } from './native-rpc.js';
import { buildBlockCatalog } from './block-catalog.js';
//=============================================================================
// NFlow transport for the shared AssistantPane. Prompts run on the live
// interpreter through the generic callback bridge; the agent's streamed events
// arrive on the 'assistant' NFlow push channel, which the engine __ai_notify__
// fan-out feeds through the NFlow assistant sink registered by the host. The
// model-authoring skill is injected engine-side, so NFlow stays host-agnostic.
//=============================================================================
const ACP_MODEL = 'claude-agent-acp';
//=============================================================================
interface AssistantPushEvent {
  channel?: string;
  payload?: string;
}
//=============================================================================
// Escape a prompt for a single-quoted Nelson char array: double the quotes and
// flatten line breaks (a literal spans one line), matching the other hosts.
function toNelsonLiteral(text: string): string {
  return text.replace(/'/g, "''").replace(/[\r\n]+/g, ' ');
}
//=============================================================================
export const nflowAssistantBackend: AssistantBackend = {
  // Query the live provider list the same way the Qt dock does (so a running
  // Ollama shows up), reusing the __ai_notify__ fan-out instead of a dedicated
  // RPC: the engine emits a one-shot 'models' event that the NFlow sink routes
  // back here. The pane itself prepends the always-present ACP backend.
  listModels: () =>
    new Promise<string[]>((resolve) => {
      let settled = false;
      const finish = (models: string[]): void => {
        if (settled) {
          return;
        }
        settled = true;
        unsubscribe();
        clearTimeout(timer);
        resolve(models);
      };
      const unsubscribe = onNFlowPush('assistant', (payload) => {
        const event = payload as AssistantPushEvent;
        if (!event || event.channel !== 'models') {
          return;
        }
        try {
          const parsed = JSON.parse(event.payload ?? '[]');
          finish(Array.isArray(parsed) ? parsed.map(String) : []);
        } catch {
          finish([]);
        }
      });
      const timer = setTimeout(() => finish([]), 4000);
      void nflowRpc('nflow.callback.invoke', {
        code:
          "try; __ai_notify__('models', jsonencode(nelson.ai.models()), 'nflow'); " +
          "catch; __ai_notify__('models', '[]', 'nflow'); end",
        blockId: '',
      }).catch(() => finish([]));
    }),

  sendPrompt: async (prompt, model) => {
    const chosen = model && model.trim() !== '' ? model : ACP_MODEL;
    // Prepend the LIVE block catalog so the agent always sees the real, current
    // block set (the skill no longer hard-codes a list that drifts). The pane
    // still shows only the user's own text; this augments only the model prompt.
    const catalog = buildBlockCatalog();
    const augmented = catalog ? `${catalog}\n\n--- Request: ${prompt}` : prompt;
    // surfaceId 'nflow' routes this run's events (and its Stop) to the NFlow
    // sink only, AND selects the NFlow build guide engine-side, so the Qt dock
    // and web pane keep their own sessions and their own guidance. driver
    // 'external' stops the engine from starting a Nelson timer to poll the ACP
    // reply (that timer never ticks in a webui host without a GUI event loop);
    // the pane drives the poll instead through step() below.
    const code = `nelson.ai.internal.assistantRun('${toNelsonLiteral(augmented)}', '${toNelsonLiteral(
      chosen
    )}', 'external', 'nflow');`;
    await nflowRpc('nflow.callback.invoke', { code, blockId: '' });
    return { accepted: true };
  },

  subscribe: (handler) =>
    onNFlowPush('assistant', (payload) => {
      const event = payload as AssistantPushEvent;
      if (event && typeof event.channel === 'string') {
        handler(event.channel, typeof event.payload === 'string' ? event.payload : '');
      }
    }),

  interrupt: () => {
    void nflowRpc('nflow.ai.stop', {}).catch(() => undefined);
  },

  runCode: (code) => {
    void nflowRpc('nflow.callback.invoke', { code, blockId: '' }).catch(() => undefined);
  },

  // Advance the ACP reply by one step on the live interpreter (the engine runs
  // the turn with driver='external', so the pane owns the poll). Fire-and-forget
  // through the generic callback bridge; acpAsyncStep no-ops once the turn ends.
  step: () => {
    void nflowRpc('nflow.callback.invoke', {
      code: "nelson.ai.internal.assistantStep('nflow');",
      blockId: '',
    }).catch(() => undefined);
  },

  // Restore the 'nflow' surface's stored conversation on open, the same one-shot
  // way listModels does: the engine emits a 'history' event the NFlow sink routes
  // back here. Best-effort; a timeout or a malformed reply just starts empty.
  loadHistory: () =>
    new Promise((resolve) => {
      let settled = false;
      const finish = (turns: { role: 'user' | 'assistant'; text: string }[]): void => {
        if (settled) {
          return;
        }
        settled = true;
        unsubscribe();
        clearTimeout(timer);
        resolve(turns);
      };
      const unsubscribe = onNFlowPush('assistant', (payload) => {
        const event = payload as AssistantPushEvent;
        if (!event || event.channel !== 'history') {
          return;
        }
        try {
          const parsed: unknown = JSON.parse(event.payload ?? '[]');
          finish(Array.isArray(parsed) ? parsed.filter(isHistoryTurn) : []);
        } catch {
          finish([]);
        }
      });
      const timer = setTimeout(() => finish([]), 4000);
      void nflowRpc('nflow.callback.invoke', {
        code:
          "try; __ai_notify__('history', nelson.ai.internal.history.json('nflow'), 'nflow'); " +
          "catch; __ai_notify__('history', '[]', 'nflow'); end",
        blockId: '',
      }).catch(() => finish([]));
    }),
};
//=============================================================================
function isHistoryTurn(value: unknown): value is { role: 'user' | 'assistant'; text: string } {
  if (typeof value !== 'object' || value === null) {
    return false;
  }
  const turn = value as { role?: unknown; text?: unknown };
  return (turn.role === 'user' || turn.role === 'assistant') && typeof turn.text === 'string';
}
//=============================================================================
