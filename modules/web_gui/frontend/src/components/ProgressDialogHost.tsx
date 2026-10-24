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
import { onPush } from '../rpc/client';
import type { ProgressDialogPush } from '../rpc/types';
import './ProgressDialogHost.css';
//=============================================================================
export function ProgressDialogHost(): JSX.Element | null {
  const [dialogs, setDialogs] = useState<ProgressDialogPush[]>([]);

  useEffect(
    () =>
      onPush('dialog.progress', (payload) => {
        const update = payload as ProgressDialogPush;
        setDialogs((current) => {
          if (!update.visible) return current.filter((dialog) => dialog.id !== update.id);
          const exists = current.some((dialog) => dialog.id === update.id);
          return exists
            ? current.map((dialog) => (dialog.id === update.id ? update : dialog))
            : [...current, update];
        });
      }),
    []
  );

  if (dialogs.length === 0) return null;

  return (
    <div className="progress-dialog-stack">
      {dialogs.map((dialog) => {
        const minimum = Number.isFinite(dialog.minimum) ? dialog.minimum : 0;
        const maximum = Number.isFinite(dialog.maximum) ? dialog.maximum : 100;
        const value = Math.min(maximum, Math.max(minimum, dialog.value));
        const range = maximum - minimum;
        const percent = range > 0 ? Math.round(((value - minimum) / range) * 100) : 0;
        const indeterminate = dialog.indeterminate || range <= 0;

        return (
          <section
            className="progress-dialog"
            data-progress-id={dialog.id}
            role="status"
            aria-live="polite"
            aria-label={dialog.title || 'Progress'}
            key={dialog.id}
          >
            <div className="progress-dialog__title">{dialog.title || 'Progress'}</div>
            {dialog.message && <div className="progress-dialog__message">{dialog.message}</div>}
            {indeterminate ? (
              <div
                className="progress-dialog__track progress-dialog__track--indeterminate"
                role="progressbar"
                aria-label={dialog.message || dialog.title || 'Progress'}
                aria-valuetext="In progress"
              >
                <span />
              </div>
            ) : (
              <div className="progress-dialog__determinate">
                <progress
                  aria-label={dialog.message || dialog.title || 'Progress'}
                  max={range}
                  value={value - minimum}
                />
                <span>{percent}%</span>
              </div>
            )}
          </section>
        );
      })}
    </div>
  );
}
//=============================================================================
