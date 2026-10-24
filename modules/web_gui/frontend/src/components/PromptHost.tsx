//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useEffect, useRef, useState } from 'react';
import { DesktopIcon } from './DesktopIcon';
//=============================================================================
// Non-blocking replacements for window.prompt / window.confirm.
//
// The native dialogs freeze the JS event loop while they are open, which stops
// the app.heartbeat timer; after ~4 s of silence the backend watchdog assumes
// the browser window is gone and quits nelson-webview. These modals are ordinary
// React state, so the heartbeat keeps firing while the user takes their time.
type PromptRequest = {
  id: number;
  kind: 'prompt' | 'confirm';
  message: string;
  defaultValue: string;
  okLabel: string;
  cancelLabel: string;
  danger: boolean;
  resolve: (value: string | null | boolean) => void;
};
//=============================================================================
let nextId = 1;
let dispatch: ((req: PromptRequest) => void) | null = null;
// Resolves to the entered string, or null if cancelled.
export function webPrompt(message: string, defaultValue = ''): Promise<string | null> {
  return new Promise((resolve) => {
    const req: PromptRequest = {
      id: nextId++,
      kind: 'prompt',
      message,
      defaultValue,
      okLabel: 'OK',
      cancelLabel: 'Cancel',
      danger: false,
      resolve: resolve as (v: string | null | boolean) => void,
    };
    if (dispatch) dispatch(req);
    else resolve(null); // host not mounted; behave like a cancelled dialog
  });
}
//=============================================================================
export function webConfirm(
  message: string,
  opts: { okLabel?: string; cancelLabel?: string; danger?: boolean } = {}
): Promise<boolean> {
  return new Promise((resolve) => {
    const req: PromptRequest = {
      id: nextId++,
      kind: 'confirm',
      message,
      defaultValue: '',
      okLabel: opts.okLabel ?? 'OK',
      cancelLabel: opts.cancelLabel ?? 'Cancel',
      danger: !!opts.danger,
      resolve: resolve as (v: string | null | boolean) => void,
    };
    if (dispatch) dispatch(req);
    else resolve(false);
  });
}
//=============================================================================
export function PromptHost(): JSX.Element | null {
  const [req, setReq] = useState<PromptRequest | null>(null);
  const [value, setValue] = useState('');
  const inputRef = useRef<HTMLInputElement | null>(null);

  useEffect(() => {
    // One prompt at a time: a new request replaces (and cancels) the pending one.
    dispatch = (r) => {
      setReq((prev) => {
        if (prev) prev.resolve(prev.kind === 'confirm' ? false : null);
        return r;
      });
      setValue(r.defaultValue);
    };
    return () => {
      dispatch = null;
    };
  }, []);

  useEffect(() => {
    if (req?.kind === 'prompt') {
      inputRef.current?.focus();
      inputRef.current?.select();
    }
  }, [req]);

  if (!req) return null;

  const finish = (result: string | null | boolean) => {
    req.resolve(result);
    setReq(null);
  };
  const cancel = () => finish(req.kind === 'confirm' ? false : null);
  const accept = () => finish(req.kind === 'confirm' ? true : value);

  const onKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Escape') {
      e.preventDefault();
      cancel();
    } else if (e.key === 'Enter') {
      e.preventDefault();
      accept();
    }
  };

  return (
    <div className="dialog-overlay" role="presentation" onMouseDown={cancel}>
      <div
        className="dialog-card"
        role="dialog"
        aria-modal="true"
        aria-label={req.message}
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={onKeyDown}
      >
        <div className="dialog-card__title">Nelson</div>
        {req.kind === 'prompt' ? (
          <div className="dialog-card__body">
            <label className="dialog-field">
              <span className="dialog-field__label">{req.message}</span>
              <input
                ref={inputRef}
                type="text"
                value={value}
                onChange={(e) => setValue(e.target.value)}
              />
            </label>
          </div>
        ) : (
          <div className="dialog-card__body dialog-card__body--message">
            <span className="dialog-card__icon" aria-hidden="true">
              <DesktopIcon name={req.danger ? 'warning' : 'question'} size={24} />
            </span>
            <span className="dialog-card__message">{req.message}</span>
          </div>
        )}
        <div className="dialog-card__buttons">
          <button className="dialog-btn" onClick={cancel}>
            {req.cancelLabel}
          </button>
          <button
            className={'dialog-btn dialog-btn--default' + (req.danger ? ' dialog-btn--danger' : '')}
            autoFocus={req.kind === 'confirm'}
            onClick={accept}
          >
            {req.okLabel}
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
