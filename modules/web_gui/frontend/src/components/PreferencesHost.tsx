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
import { DEFAULT_PREFS, getPrefs, setPrefs, type Prefs } from '../desktop/prefs';
import { resetInterfacePreferences } from '../desktop/persistentStorage';
import { requestTheme } from '../desktop/theme';
import { setShellVisibility } from '../desktop/shellState';
import { showStatusMessage } from '../desktop/statusMessage';
//=============================================================================
// Preferences modal (fonts, sizes, behaviour). Opened by a window event so the
// menu bar can trigger it without threading state through the dock. Changes
// apply live because the panels subscribe to the prefs store.
export const OPEN_PREFS_EVENT = 'nelson-webview.open-prefs';
const FONT_FAMILIES = [
  'Consolas, "Cascadia Mono", "DejaVu Sans Mono", monospace',
  '"Courier New", monospace',
  '"Lucida Console", monospace',
  '"Cascadia Code", Consolas, monospace',
  '"Fira Code", Consolas, monospace',
  '"JetBrains Mono", Consolas, monospace',
  'ui-monospace, monospace',
];
//=============================================================================
export function PreferencesHost(): JSX.Element | null {
  const [openState, setOpenState] = useState(false);
  const [draft, setDraft] = useState<Prefs>(getPrefs());

  useEffect(() => {
    const onOpen = () => {
      setDraft(getPrefs());
      setOpenState(true);
    };
    window.addEventListener(OPEN_PREFS_EVENT, onOpen);
    return () => window.removeEventListener(OPEN_PREFS_EVENT, onOpen);
  }, []);

  if (!openState) return null;

  const apply = () => {
    setPrefs(draft);
    setOpenState(false);
  };
  const reset = () => setDraft(DEFAULT_PREFS);
  const resetInterface = async () => {
    await resetInterfacePreferences();
    setPrefs(DEFAULT_PREFS);
    setShellVisibility('toolbar', true);
    setShellVisibility('rail', true);
    setShellVisibility('status', true);
    requestTheme('system');
    setDraft(DEFAULT_PREFS);
    setOpenState(false);
    showStatusMessage('Interface preferences reset.');
  };

  const num = (label: string, key: keyof Prefs, min: number, max: number) => (
    <label className="prefs-row">
      <span className="prefs-row__label">{label}</span>
      <input
        type="number"
        min={min}
        max={max}
        value={draft[key] as number}
        onChange={(e) =>
          setDraft((d) => ({
            ...d,
            [key]: Math.max(min, Math.min(max, Number(e.target.value) || min)),
          }))
        }
      />
    </label>
  );

  return (
    <div className="dialog-overlay" role="presentation" onMouseDown={() => setOpenState(false)}>
      <div
        className="dialog-card prefs-card"
        role="dialog"
        aria-modal="true"
        aria-label="Preferences"
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={(e) => {
          if (e.key === 'Escape') setOpenState(false);
        }}
      >
        <div className="dialog-card__title">Preferences</div>
        <div className="dialog-card__body">
          <div className="prefs-section">Command Window</div>
          {num('Font size', 'consoleFontSize', 8, 40)}
          <label className="prefs-row">
            <span className="prefs-row__label">Font family</span>
            <select
              value={draft.consoleFontFamily}
              onChange={(e) => setDraft((d) => ({ ...d, consoleFontFamily: e.target.value }))}
            >
              {FONT_FAMILIES.map((f) => (
                <option key={f} value={f}>
                  {f.split(',')[0].replace(/"/g, '')}
                </option>
              ))}
            </select>
          </label>
          <label className="prefs-row prefs-row--check">
            <input
              type="checkbox"
              checked={draft.consoleWrap}
              onChange={(e) => setDraft((d) => ({ ...d, consoleWrap: e.target.checked }))}
            />
            <span>Wrap long lines</span>
          </label>

          <div className="prefs-section">Editor</div>
          {num('Font size', 'editorFontSize', 8, 40)}
          {num('Tab size', 'editorTabSize', 1, 8)}
        </div>
        <div className="dialog-card__buttons">
          <button className="dialog-btn" onClick={reset}>
            Reset fields
          </button>
          <button className="dialog-btn" onClick={() => void resetInterface()}>
            Reset interface
          </button>
          <span className="editor-toolbar__spacer" />
          <button className="dialog-btn" onClick={() => setOpenState(false)}>
            Cancel
          </button>
          <button className="dialog-btn dialog-btn--default" onClick={apply}>
            Apply
          </button>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
