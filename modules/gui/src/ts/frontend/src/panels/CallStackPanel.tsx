//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// call stack of the paused function, innermost first; a click opens the frame's file
//=============================================================================
import { useEffect, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import { onEngineReady } from '../desktop/engineSession';
import { openEditor } from '../desktop/desktop';
import type { DebuggerState } from '../rpc/types';
import { t, subscribeTranslations } from '../i18n';
import { baseName } from '../desktop/paths';
//=============================================================================
export function CallStackPanel() {
  const [state, setState] = useState<DebuggerState | null>(null);
  const [, tick] = useState(0);

  useEffect(() => {
    const pull = () =>
      rpc<DebuggerState>('debugger.state')
        .then((s) => setState(s ?? null))
        .catch(() => undefined);
    pull();
    const offReady = onEngineReady(pull);
    const offState = onPush('debugger.state', (s) => setState((s as DebuggerState) ?? null));
    const offT = subscribeTranslations(() => tick((n) => n + 1));
    return () => {
      offReady();
      offState();
      offT();
    };
  }, []);

  if (!state || !state.paused) {
    return (
      <div className="callstack-panel" data-testid="callstack-panel">
        <div className="bp-panel__empty">{t('Not paused')}</div>
      </div>
    );
  }
  return (
    <div className="callstack-panel" data-testid="callstack-panel">
      <ul className="callstack-panel__list">
        {state.stack.map((frame, index) => (
          <li
            key={`${frame.file}:${frame.line}:${index}`}
            className={index === 0 ? 'callstack-panel__frame--current' : ''}
            title={`${frame.file}:${frame.line}`}
            onClick={() => frame.file && openEditor(frame.file, frame.line)}
          >
            {frame.function || baseName(frame.file)} ({baseName(frame.file)}:{frame.line})
          </li>
        ))}
      </ul>
    </div>
  );
}
//=============================================================================
