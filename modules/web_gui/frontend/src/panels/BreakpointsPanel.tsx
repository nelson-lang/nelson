//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Breakpoints panel: every breakpoint with its state and condition, plus the error breakpoints
//=============================================================================
import { useEffect, useState } from 'react';
import { onPush, rpc } from '../rpc/client';
import { onEngineReady } from '../desktop/engineSession';
import { openEditor } from '../desktop/desktop';
import type { AllBreakpointsResult, BreakpointInfo } from '../rpc/types';
import { t, subscribeTranslations } from '../i18n';
import { baseName } from '../desktop/paths';
//=============================================================================
const emptyResult: AllBreakpointsResult = {
  breakpoints: [],
  errorBreakpoints: { error: false, caughtError: false },
  revision: 0,
};
//=============================================================================
export function BreakpointsPanel() {
  const [data, setData] = useState<AllBreakpointsResult>(emptyResult);
  const [selected, setSelected] = useState<number | null>(null);
  const [editing, setEditing] = useState<{ id: number; value: string } | null>(null);
  const [error, setError] = useState('');
  const [, tick] = useState(0);

  const refresh = () => {
    rpc<AllBreakpointsResult>('debugger.listBreakpoints', {})
      .then((r) => setData(r ?? emptyResult))
      .catch(() => undefined);
  };

  useEffect(() => {
    refresh();
    const offReady = onEngineReady(refresh);
    const offBp = onPush('debugger.breakpoints', refresh);
    const offState = onPush('debugger.state', refresh);
    const offT = subscribeTranslations(() => tick((n) => n + 1));
    return () => {
      offReady();
      offBp();
      offState();
      offT();
    };
  }, []);

  const call = (method: string, params: Record<string, unknown>) =>
    rpc(method, params)
      .then(() => refresh())
      .catch(() => undefined);

  const setEnabled = (bp: BreakpointInfo, enabled: boolean) =>
    call('debugger.setBreakpoint', { file: bp.file, line: bp.line, enabled });

  const applyCondition = (bp: BreakpointInfo, condition: string) => {
    rpc<{ error?: string }>('debugger.setBreakpoint', { file: bp.file, line: bp.line, condition })
      .then((r) => {
        if (r?.error) {
          setError(r.error);
          return;
        }
        setError('');
        setEditing(null);
        refresh();
      })
      .catch(() => undefined);
  };

  const selectedBp = data.breakpoints.find((bp) => bp.id === selected);

  return (
    <div className="bp-panel" data-testid="breakpoints-panel">
      <div className="bp-panel__toolbar">
        <button
          className="editor-toolbar__btn"
          disabled={!selectedBp}
          onClick={() =>
            selectedBp &&
            call('debugger.removeBreakpoint', { file: selectedBp.file, line: selectedBp.line })
          }
        >
          {t('Clear')}
        </button>
        <button
          className="editor-toolbar__btn"
          onClick={() => {
            // Clear All Breakpoints also removes the error breakpoints
            void rpc('debugger.setErrorBreakpoints', { error: false, caughtError: false }).catch(
              () => undefined
            );
            void call('debugger.removeAllBreakpoints', {});
          }}
        >
          {t('Clear All')}
        </button>
        <button
          className="editor-toolbar__btn"
          disabled={!data.breakpoints.length}
          onClick={() => call('debugger.setAllEnabled', { enabled: true })}
        >
          {t('Enable All')}
        </button>
        <button
          className="editor-toolbar__btn"
          disabled={!data.breakpoints.length}
          onClick={() => call('debugger.setAllEnabled', { enabled: false })}
        >
          {t('Disable All')}
        </button>
      </div>
      <table className="bp-panel__table">
        <thead>
          <tr>
            <th>{t('Enabled')}</th>
            <th>{t('File')}</th>
            <th>{t('Function')}</th>
            <th>{t('Line')}</th>
            <th>{t('Condition')}</th>
          </tr>
        </thead>
        <tbody>
          {data.breakpoints.length === 0 && (
            <tr>
              <td colSpan={5} className="bp-panel__empty">
                {t('No breakpoints')}
              </td>
            </tr>
          )}
          {data.breakpoints.map((bp) => (
            <tr
              key={bp.id}
              className={bp.id === selected ? 'bp-panel__row--selected' : ''}
              onClick={() => setSelected(bp.id)}
              onDoubleClick={() => bp.file && openEditor(bp.file, bp.line)}
            >
              <td>
                <input
                  type="checkbox"
                  checked={bp.enabled}
                  onChange={(e) => setEnabled(bp, e.target.checked)}
                  aria-label={t('Enabled')}
                />
              </td>
              <td title={bp.file}>{baseName(bp.file ?? '')}</td>
              <td>{bp.function ?? ''}</td>
              <td>{bp.line}</td>
              <td
                onDoubleClick={(e) => {
                  e.stopPropagation();
                  setEditing({ id: bp.id, value: bp.condition });
                  setError('');
                }}
              >
                {editing?.id === bp.id ? (
                  <form
                    onSubmit={(e) => {
                      e.preventDefault();
                      applyCondition(bp, editing.value);
                    }}
                  >
                    <input
                      className="bp-panel__condition-input"
                      autoFocus
                      value={editing.value}
                      onChange={(e) => setEditing({ id: bp.id, value: e.target.value })}
                      onBlur={() => applyCondition(bp, editing.value)}
                      onKeyDown={(e) => {
                        if (e.key === 'Escape') setEditing(null);
                      }}
                      spellCheck={false}
                    />
                  </form>
                ) : (
                  bp.condition
                )}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
      {error && (
        <div className="bp-panel__error" role="alert">
          {error}
        </div>
      )}
      <div className="bp-panel__errors">
        <label>
          <input
            type="checkbox"
            checked={data.errorBreakpoints.error}
            onChange={(e) => call('debugger.setErrorBreakpoints', { error: e.target.checked })}
          />{' '}
          {t('Pause on Errors')}
        </label>
        <label>
          <input
            type="checkbox"
            checked={data.errorBreakpoints.caughtError}
            onChange={(e) =>
              call('debugger.setErrorBreakpoints', { caughtError: e.target.checked })
            }
          />{' '}
          {t('Pause on Caught Errors')}
        </label>
      </div>
    </div>
  );
}
//=============================================================================
