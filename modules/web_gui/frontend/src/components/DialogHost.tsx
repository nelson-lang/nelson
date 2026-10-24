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
import { onPush, rpc } from '../rpc/client';
import { FilePickerDialog, type FilePickerMode } from '@nelson/dialogs';
import { DesktopIcon, type DesktopIconName } from './DesktopIcon';
//=============================================================================
// Serves the native GUI dialogs (inputdlg, uialert, uiconfirm, questdlg,
// listdlg) that the engine would otherwise draw with Qt. The nlsWeb_gui backend
// pushes `dialog.show`, blocks the interpreter thread, and waits for
// `dialog.reply`.
type DialogField = { prompt: string; value: string; lines: number };
function dialogIconName(icon: string): DesktopIconName {
  if (icon === 'error') return 'error';
  if (icon === 'warn' || icon === 'warning') return 'warning';
  if (icon === 'help' || icon === 'question') return 'question';
  if (icon === 'success') return 'check';
  return 'info';
}
//=============================================================================
type DialogRequest = {
  id: number;
  kind: 'input' | 'message' | 'question' | 'list' | 'color' | 'file' | string;
  title: string;
  message: string;
  icon: string;
  fields: DialogField[];
  buttons: string[];
  buttonValues?: string[];
  extraButtons?: string[];
  defaultButton: string;
  checkboxVisible?: boolean;
  checkboxText?: string;
  checkboxState?: boolean;
  helpString?: string;
  helpFcn?: string;
  items?: string[];
  multiSelect?: boolean;
  initialSelection?: number[]; // 1-based
  fileMode?: 'open' | 'save' | 'dir';
  fileFilters?: string[];
  defaultPath?: string;
  suggestedName?: string;
  historyKey?: string;
  fontFamily?: string;
  fontSize?: number;
  fontBold?: boolean;
  fontItalic?: boolean;
};
//=============================================================================
const FONT_FAMILIES = [
  'Arial',
  'Helvetica',
  'Times New Roman',
  'Courier New',
  'Consolas',
  'Georgia',
  'Verdana',
];
//=============================================================================
export function DialogHost(): JSX.Element | null {
  const [dialog, setDialog] = useState<DialogRequest | null>(null);
  const [values, setValues] = useState<string[]>([]);
  const [selection, setSelection] = useState<number[]>([]); // 0-based
  const [checked, setChecked] = useState(false);
  const [color, setColor] = useState('#ffffff');
  const [font, setFont] = useState({ family: 'Arial', size: 10, bold: false, italic: false });
  // Fire-and-forget message (msgbox): no interpreter is blocked on a reply.
  const [message, setMessage] = useState<{ title: string; message: string; icon: string } | null>(
    null
  );
  const firstFieldRef = useRef<HTMLInputElement | HTMLTextAreaElement | null>(null);

  useEffect(() => {
    // One dialog at a time: the interpreter thread is blocked, so a second
    // request cannot arrive until the first is answered.
    return onPush('dialog.show', (p) => {
      const req = p as DialogRequest;
      setDialog(req);
      setValues((req.fields ?? []).map((f) => f.value ?? ''));
      setSelection((req.initialSelection ?? []).map((i) => i - 1));
      setChecked(!!req.checkboxState);
      if (req.kind === 'color') setColor(req.defaultPath || '#ffffff');
      if (req.kind === 'font')
        setFont({
          family: req.fontFamily || 'Arial',
          size: req.fontSize || 10,
          bold: !!req.fontBold,
          italic: !!req.fontItalic,
        });
    });
  }, []);

  useEffect(() => {
    // Non-blocking message boxes (msgbox), shown and dismissed locally.
    return onPush('dialog.message', (p) => {
      const m = p as { title: string; message: string; icon: string };
      setMessage(m);
    });
  }, []);

  useEffect(() => {
    if (dialog) firstFieldRef.current?.focus();
  }, [dialog]);

  const messageModal = message ? (
    <div className="dialog-overlay" role="presentation" onMouseDown={() => setMessage(null)}>
      <div
        className="dialog-card"
        role="dialog"
        aria-modal="true"
        aria-label={message.title}
        onMouseDown={(e) => e.stopPropagation()}
        onKeyDown={(e) => {
          if (e.key === 'Escape' || e.key === 'Enter') {
            e.preventDefault();
            setMessage(null);
          }
        }}
      >
        <div className="dialog-card__title">{message.title || 'Nelson'}</div>
        <div className="dialog-card__body dialog-card__body--message">
          <span className="dialog-card__icon" aria-hidden="true">
            <DesktopIcon name={dialogIconName(message.icon)} size={24} />
          </span>
          <span className="dialog-card__message">{message.message}</span>
        </div>
        <div className="dialog-card__buttons">
          <button
            className="dialog-btn dialog-btn--default"
            autoFocus
            onClick={() => setMessage(null)}
          >
            OK
          </button>
        </div>
      </div>
    </div>
  ) : null;

  if (!dialog) return messageModal;

  const reply = (payload: {
    ok: boolean;
    values?: string[];
    button?: string;
    checkboxChecked?: boolean;
    selectedIndices?: number[];
    path?: string;
  }) => {
    void rpc('dialog.reply', { id: dialog.id, ...payload });
    setDialog(null);
  };

  const cancel = () => reply({ ok: false, values: [], button: '' });

  // File dialogs are served by the shared, Qt-free FilePickerDialog.
  if (dialog.kind === 'file') {
    return (
      <>
        {messageModal}
        <FilePickerDialog
          mode={(dialog.fileMode as FilePickerMode) || 'open'}
          title={dialog.title}
          defaultPath={dialog.defaultPath || '.'}
          suggestedName={dialog.suggestedName}
          filters={dialog.fileFilters}
          listDir={(path) => rpc('fs.list', { path })}
          onSubmit={(path) => reply({ ok: true, path })}
          onCancel={cancel}
        />
      </>
    );
  }

  const submitColor = () => reply({ ok: true, path: color });
  const submitFont = () =>
    reply({
      ok: true,
      values: [
        font.family,
        String(font.size),
        font.bold ? 'bold' : 'normal',
        font.italic ? 'italic' : 'normal',
      ],
    });
  const submitInput = () => reply({ ok: true, values });

  const submitList = () => reply({ ok: true, selectedIndices: selection.map((i) => i + 1) });
  const submitPreferenceButton = (label: string, index: number) =>
    reply({
      ok: true,
      button: (dialog.buttonValues ?? [])[index] ?? label,
      checkboxChecked: checked,
    });
  const submitExtraButton = (label: string) =>
    reply({ ok: true, button: label, checkboxChecked: checked });

  const toggleListItem = (i: number) => {
    if (dialog.multiSelect) {
      setSelection((s) => (s.includes(i) ? s.filter((x) => x !== i) : [...s, i]));
    } else {
      setSelection([i]);
    }
  };

  const onKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Escape') {
      e.preventDefault();
      cancel();
    } else if (e.key === 'Enter' && (e.ctrlKey || dialog.kind !== 'input')) {
      // Ctrl+Enter always submits; plain Enter submits single-line inputs.
      e.preventDefault();
      if (dialog.kind === 'list') submitList();
      else if (dialog.kind === 'color') submitColor();
      else if (dialog.kind === 'font') submitFont();
      else submitInput();
    }
  };

  return (
    <>
      {messageModal}
      <div className="dialog-overlay" role="presentation" onMouseDown={cancel}>
        <div
          className="dialog-card"
          role="dialog"
          aria-modal="true"
          aria-label={dialog.title}
          onMouseDown={(e) => e.stopPropagation()}
          onKeyDown={onKeyDown}
        >
          <div className="dialog-card__title">{dialog.title || 'Nelson'}</div>

          {dialog.kind === 'color' ? (
            <div className="dialog-card__body">
              <label className="dialog-field dialog-field--color">
                <input type="color" value={color} onChange={(e) => setColor(e.target.value)} />
                <span className="browser__mono">{color}</span>
              </label>
            </div>
          ) : dialog.kind === 'font' ? (
            <div className="dialog-card__body">
              <label className="dialog-field">
                <span className="dialog-field__label">Font</span>
                <select
                  value={font.family}
                  onChange={(e) => setFont((f) => ({ ...f, family: e.target.value }))}
                >
                  {(FONT_FAMILIES.includes(font.family)
                    ? FONT_FAMILIES
                    : [font.family, ...FONT_FAMILIES]
                  ).map((fam) => (
                    <option key={fam} value={fam}>
                      {fam}
                    </option>
                  ))}
                </select>
              </label>
              <label className="dialog-field">
                <span className="dialog-field__label">Size</span>
                <input
                  type="number"
                  min={4}
                  max={200}
                  value={font.size}
                  onChange={(e) => setFont((f) => ({ ...f, size: Number(e.target.value) || 10 }))}
                />
              </label>
              <label className="dialog-field dialog-field--check">
                <input
                  type="checkbox"
                  checked={font.bold}
                  onChange={(e) => setFont((f) => ({ ...f, bold: e.target.checked }))}
                />
                <span>Bold</span>
              </label>
              <label className="dialog-field dialog-field--check">
                <input
                  type="checkbox"
                  checked={font.italic}
                  onChange={(e) => setFont((f) => ({ ...f, italic: e.target.checked }))}
                />
                <span>Italic</span>
              </label>
              <div
                className="dialog-font__preview"
                style={{
                  fontFamily: font.family,
                  fontSize: Math.min(font.size, 32),
                  fontWeight: font.bold ? 700 : 400,
                  fontStyle: font.italic ? 'italic' : 'normal',
                }}
              >
                AaBbYyZz 123
              </div>
            </div>
          ) : dialog.kind === 'list' ? (
            <div className="dialog-card__body">
              {dialog.message ? (
                <span className="dialog-field__label">{dialog.message}</span>
              ) : null}
              <div className="dialog-list" role="listbox" aria-multiselectable={dialog.multiSelect}>
                {(dialog.items ?? []).map((item, i) => (
                  <div
                    key={i}
                    role="option"
                    aria-selected={selection.includes(i)}
                    className={
                      'dialog-list__item' + (selection.includes(i) ? ' dialog-list__item--sel' : '')
                    }
                    onClick={() => toggleListItem(i)}
                    onDoubleClick={() => {
                      setSelection([i]);
                      reply({ ok: true, selectedIndices: [i + 1] });
                    }}
                  >
                    {item}
                  </div>
                ))}
              </div>
            </div>
          ) : dialog.kind === 'input' ? (
            <div className="dialog-card__body">
              {dialog.fields.map((field, i) => (
                <label key={i} className="dialog-field">
                  <span className="dialog-field__label">{field.prompt}</span>
                  {field.lines > 1 ? (
                    <textarea
                      ref={i === 0 ? (el) => (firstFieldRef.current = el) : undefined}
                      rows={field.lines}
                      value={values[i] ?? ''}
                      onChange={(e) =>
                        setValues((v) => v.map((x, k) => (k === i ? e.target.value : x)))
                      }
                    />
                  ) : (
                    <input
                      ref={i === 0 ? (el) => (firstFieldRef.current = el) : undefined}
                      type="text"
                      value={values[i] ?? ''}
                      onChange={(e) =>
                        setValues((v) => v.map((x, k) => (k === i ? e.target.value : x)))
                      }
                    />
                  )}
                </label>
              ))}
            </div>
          ) : (
            <div className="dialog-card__body dialog-card__body--message">
              <span className="dialog-card__icon" aria-hidden="true">
                <DesktopIcon name={dialogIconName(dialog.icon)} size={24} />
              </span>
              <span className="dialog-card__message">{dialog.message}</span>
              {dialog.checkboxVisible ? (
                <label className="dialog-field dialog-field--check">
                  <input
                    type="checkbox"
                    checked={checked}
                    onChange={(e) => setChecked(e.target.checked)}
                  />
                  <span>{dialog.checkboxText || 'Do not show this dialog again.'}</span>
                </label>
              ) : null}
            </div>
          )}

          <div className="dialog-card__buttons">
            {dialog.kind === 'preference' ? (
              <>
                {(dialog.buttons.length ? dialog.buttons : ['OK']).map((b, i) => (
                  <button
                    key={`pref-${b}-${i}`}
                    className={
                      b === dialog.defaultButton ||
                      ((dialog.buttonValues ?? [])[i] ?? b) === dialog.defaultButton
                        ? 'dialog-btn dialog-btn--default'
                        : 'dialog-btn'
                    }
                    onClick={() => submitPreferenceButton(b, i)}
                  >
                    {b}
                  </button>
                ))}
                {(dialog.extraButtons ?? []).map((b) => (
                  <button
                    key={`extra-${b}`}
                    className="dialog-btn"
                    onClick={() => submitExtraButton(b)}
                  >
                    {b}
                  </button>
                ))}
              </>
            ) : dialog.kind === 'question' ? (
              (dialog.buttons.length ? dialog.buttons : ['OK']).map((b) => (
                <button
                  key={b}
                  className={
                    b === dialog.defaultButton ? 'dialog-btn dialog-btn--default' : 'dialog-btn'
                  }
                  onClick={() => reply({ ok: true, button: b })}
                >
                  {b}
                </button>
              ))
            ) : dialog.kind === 'message' ? (
              <button
                className="dialog-btn dialog-btn--default"
                onClick={() => reply({ ok: true })}
              >
                OK
              </button>
            ) : dialog.kind === 'color' ? (
              <>
                <button className="dialog-btn" onClick={cancel}>
                  Cancel
                </button>
                <button className="dialog-btn dialog-btn--default" onClick={submitColor}>
                  OK
                </button>
              </>
            ) : dialog.kind === 'font' ? (
              <>
                <button className="dialog-btn" onClick={cancel}>
                  Cancel
                </button>
                <button className="dialog-btn dialog-btn--default" onClick={submitFont}>
                  OK
                </button>
              </>
            ) : dialog.kind === 'list' ? (
              <>
                <button className="dialog-btn" onClick={cancel}>
                  Cancel
                </button>
                <button
                  className="dialog-btn dialog-btn--default"
                  disabled={selection.length === 0}
                  onClick={submitList}
                >
                  OK
                </button>
              </>
            ) : (
              <>
                <button className="dialog-btn" onClick={cancel}>
                  Cancel
                </button>
                <button className="dialog-btn dialog-btn--default" onClick={submitInput}>
                  OK
                </button>
              </>
            )}
          </div>
        </div>
      </div>
    </>
  );
}
//=============================================================================
