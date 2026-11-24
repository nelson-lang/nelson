//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useState } from 'react';
import { rpc } from '../rpc/client';
import { subscribeToolEvents } from '../desktop/webTools';
import { DesktopIcon } from './DesktopIcon';
import { t, tf } from '../i18n';
//=============================================================================
// Claude-Code-style permission prompt for the AI assistant. When the agent asks
// to use one of its own tools (e.g. read/write a file), the engine pushes a
// 'permission' event on the assistant channel and waits; this modal lets the
// user Allow once / Always for this file / Deny, and the decision is sent back
// over the assistant.permissionReply RPC. The run stays in "Thinking…" while the
// user decides (intended). The prompt also closes on answer/error/reset so it is
// never left orphaned if the run ends (the engine auto-rejects after a timeout).
// "Always for this file" remembers just this path for the session; global
// "never ask" is the Full access permission mode, so no global allow button here.
type PermissionRequest = { id: number; tool: string; path: string; title: string };
type Decision = 'allow_once' | 'allow_always_path' | 'reject';
//=============================================================================
export function AssistantPermissionHost(): JSX.Element | null {
  const [req, setReq] = useState<PermissionRequest | null>(null);

  useEffect(() => {
    return subscribeToolEvents('assistant', (channel, raw) => {
      if (channel === 'permission') {
        try {
          const p = JSON.parse(raw) as Partial<PermissionRequest>;
          if (typeof p.id === 'number') {
            setReq({
              id: p.id,
              tool: String(p.tool ?? ''),
              path: String(p.path ?? ''),
              title: String(p.title ?? ''),
            });
          }
        } catch {
          /* ignore malformed event */
        }
        return;
      }
      // The run ended or restarted: never leave the prompt orphaned.
      if (channel === 'answer' || channel === 'error' || channel === 'reset') {
        setReq(null);
      }
    });
  }, []);

  if (!req) return null;

  const decide = (decision: Decision) => {
    void rpc('assistant.permissionReply', { id: req.id, decision }).catch(() => undefined);
    setReq(null);
  };

  const label =
    req.title ||
    (req.path ? tf('Claude wants to access %1', req.path) : t('Claude wants to use a tool'));

  const onKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Escape') {
      e.preventDefault();
      decide('reject');
    }
  };

  return (
    <div className="dialog-overlay" role="presentation" onMouseDown={() => decide('reject')}>
      <div
        className="dialog-card"
        role="dialog"
        aria-modal="true"
        aria-label={label}
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={onKeyDown}
      >
        <div className="dialog-card__title">{t('Assistant permission')}</div>
        <div className="dialog-card__body dialog-card__body--message">
          <span className="dialog-card__icon" aria-hidden="true">
            <DesktopIcon name="question" size={24} />
          </span>
          <span className="dialog-card__message">{label}</span>
        </div>
        <div className="dialog-card__buttons">
          <button className="dialog-btn" onClick={() => decide('reject')}>
            {t('Deny')}
          </button>
          <button className="dialog-btn" onClick={() => decide('allow_always_path')}>
            {t('Always for this file')}
          </button>
          <button
            className="dialog-btn dialog-btn--default"
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
