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
import {
  getPermissionMode,
  setPermissionMode,
  subscribePermissionMode,
  type PermissionMode,
} from '../desktop/permissionMode';
import { t } from '../i18n';
//=============================================================================
// Session-wide selector for how the assistant's file and tool actions are
// approved. Sits above an assistant pane; every surface shares one engine value.
export function PermissionModeSelect(): JSX.Element {
  const [mode, setMode] = useState<PermissionMode>(getPermissionMode());
  useEffect(() => subscribePermissionMode(setMode), []);
  return (
    <div className="assistant-mode-bar">
      <label className="assistant-mode-bar__label">
        <span className="assistant-mode-bar__caption">{t('Permissions')}</span>
        <select
          className="assistant-mode-bar__select"
          value={mode}
          onChange={(event) => setPermissionMode(event.target.value as PermissionMode)}
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
