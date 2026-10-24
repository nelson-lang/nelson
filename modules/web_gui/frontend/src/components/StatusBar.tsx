//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useSyncExternalStore } from 'react';
import { isEngineBusy, subscribeEngine } from '../desktop/engineSession';
import { useStatusMessage } from '../desktop/statusMessage';
//=============================================================================
export function StatusBar(): JSX.Element {
  const busy = useSyncExternalStore(subscribeEngine, isEngineBusy, () => false);
  const message = useStatusMessage();
  return (
    <div
      className="statusbar"
      role="status"
      aria-live="polite"
      tabIndex={-1}
      data-focus-zone="status"
    >
      <span className="statusbar__message">{message}</span>
      <span className="statusbar__spacer" />
      <span className={'statusbar__state ' + (busy ? 'busy' : 'ready')}>
        {busy ? 'Busy' : 'Ready'}
      </span>
    </div>
  );
}
//=============================================================================
