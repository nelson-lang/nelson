//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// NFlow-side permission UI for the assistant, mirroring what the web desktop
// offers so the NFlow host reaches the same session-wide engine state:
//  - a mode selector (Ask every time / Approve for me / Full access), read and
//    written through nelson.ai.acp.permissionMode over the NFlow bridge;
//  - a prompt shown when the agent asks to use a tool (e.g. file access): the
//    engine pushes a 'permission' event on the 'nflow' surface and waits, and
//    the reply goes back through nelson.ai.assistantPermissionReply('nflow', ...).
// Both are self-contained: they use the same native bridge and the same
// 'assistant' push channel the chat backend already listens on, so the shared
// pane stays host-agnostic (no permission code in it).
import { useEffect, useState } from 'react';
import { nflowRpc, onNFlowPush } from '../native-rpc.js';
import { t } from '../../utils/i18n.js';
//=============================================================================
export type PermissionMode = 'always_ask' | 'auto' | 'full';
const MODES: readonly PermissionMode[] = ['always_ask', 'auto', 'full'];
function isMode(value: unknown): value is PermissionMode {
  return typeof value === 'string' && (MODES as readonly string[]).includes(value);
}
//=============================================================================
interface AssistantPushEvent {
  channel?: string;
  payload?: string;
}
//=============================================================================
// Run a short script on the live interpreter through the generic callback bridge
// (the same path the chat backend uses); failures are swallowed so the UI never
// throws when the engine is busy or absent.
function runEngine(code: string): void {
  void nflowRpc('nflow.callback.invoke', { code, blockId: '' }).catch(() => undefined);
}
//=============================================================================
// Session-wide selector for how the assistant's file and tool actions are
// approved. The engine owns the value across every surface, so this reads it once
// on mount (via the one-shot __ai_notify__ round-trip the model list also uses)
// and writes every change back; it therefore matches the web desktop selector.
export function AssistantPermissionModeSelect(): JSX.Element {
  const [mode, setMode] = useState<PermissionMode>('always_ask');

  useEffect(() => {
    let settled = false;
    const unsubscribe = onNFlowPush('assistant', (payload) => {
      const event = payload as AssistantPushEvent;
      if (!event || event.channel !== 'permissionMode') {
        return;
      }
      try {
        const parsed: unknown = JSON.parse(event.payload ?? '""');
        if (isMode(parsed)) {
          setMode(parsed);
        }
      } catch {
        /* ignore malformed event */
      }
      settled = true;
      unsubscribe();
    });
    runEngine(
      "try; __ai_notify__('permissionMode', jsonencode(nelson.ai.acp.permissionMode()), " +
        "'nflow'); catch; end"
    );
    return () => {
      if (!settled) {
        unsubscribe();
      }
    };
  }, []);

  const change = (next: PermissionMode): void => {
    setMode(next);
    runEngine(`nelson.ai.acp.permissionMode('${next}');`);
  };

  return (
    <div className="assistant-mode-bar">
      <label className="assistant-mode-bar__label">
        <span className="assistant-mode-bar__caption">{t('Permissions')}</span>
        <select
          className="assistant-mode-bar__select"
          value={mode}
          onChange={(event) => change(event.target.value as PermissionMode)}
          title={t('How the assistant file and tool actions are approved')}
          aria-label={t('Assistant permission mode')}
        >
          <option value="always_ask">{t('Ask every time')}</option>
          <option value="auto">{t('Approve for me')}</option>
          <option value="full">{t('Full access')}</option>
        </select>
      </label>
    </div>
  );
}
//=============================================================================
interface PermissionRequest {
  id: number;
  tool: string;
  path: string;
  title: string;
}
type Decision = 'allow_once' | 'allow_always_path' | 'reject';
//=============================================================================
// Prompt shown when the agent asks to use a tool (the engine pushes a
// 'permission' event and waits). The run stays in "Thinking…" while the user
// decides (intended); the prompt also closes on answer/error/reset so it is never
// left orphaned when a run ends, and the engine auto-rejects after a timeout.
export function AssistantPermissionDialog(): JSX.Element | null {
  const [req, setReq] = useState<PermissionRequest | null>(null);

  useEffect(() => {
    return onNFlowPush('assistant', (payload) => {
      const event = payload as AssistantPushEvent;
      if (event && event.channel === 'permission') {
        try {
          const parsed = JSON.parse(event.payload ?? '{}') as Partial<PermissionRequest>;
          if (typeof parsed.id === 'number') {
            setReq({
              id: parsed.id,
              tool: String(parsed.tool ?? ''),
              path: String(parsed.path ?? ''),
              title: String(parsed.title ?? ''),
            });
          }
        } catch {
          /* ignore malformed event */
        }
        return;
      }
      if (
        event &&
        (event.channel === 'answer' || event.channel === 'error' || event.channel === 'reset')
      ) {
        setReq(null);
      }
    });
  }, []);

  if (!req) {
    return null;
  }

  const decide = (decision: Decision): void => {
    runEngine(`nelson.ai.assistantPermissionReply('nflow', ${req.id}, '${decision}');`);
    setReq(null);
  };

  const label =
    req.title ||
    (req.path
      ? t('Claude wants to access {path}', { path: req.path })
      : t('Claude wants to use a tool'));

  return (
    <div
      className="assistant-permission-overlay"
      role="presentation"
      onMouseDown={() => decide('reject')}
    >
      <div
        className="assistant-permission-card"
        role="dialog"
        aria-modal="true"
        aria-label={label}
        onMouseDown={(event) => event.stopPropagation()}
        onKeyDown={(event) => {
          if (event.key === 'Escape') {
            event.preventDefault();
            decide('reject');
          }
        }}
      >
        <div className="assistant-permission-card__title">{t('Assistant permission')}</div>
        <div className="assistant-permission-card__message">{label}</div>
        <div className="assistant-permission-card__buttons">
          <button type="button" onClick={() => decide('reject')}>
            {t('Deny')}
          </button>
          <button type="button" onClick={() => decide('allow_always_path')}>
            {t('Always for this file')}
          </button>
          <button
            type="button"
            className="assistant-permission-card__default"
            autoFocus
            onClick={() => decide('allow_once')}
          >
            {t('Allow once')}
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
