//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { useCallback, useState } from 'react';
import { MenuItem } from './MenuBar';
import { OPEN_ABOUT_EVENT } from './AboutHost';
import { useMenuDismiss } from './useMenuDismiss';
import { openNotebook } from '../desktop/desktop';
import { sendNotebookCommand, type NotebookCommand } from '../notebook/commands';
import { rpc } from '../rpc/client';
import { t } from '../i18n';
//=============================================================================
const MENU_NAMES = ['file', 'help'];
//=============================================================================
/** Run a console command in the host desktop; its command window shows it. */
function runInHost(line: string): void {
  void rpc('term.input', { line }).catch(() => undefined);
}
//=============================================================================
/** Menu bar of the Notebook window of the Qt desktop: documents and help only. */
export function NotebookMenuBar(): JSX.Element {
  const [open, setOpen] = useState<string | null>(null);
  const close = useCallback(() => setOpen(null), []);
  useMenuDismiss(open !== null, close, '.menubar');

  const run = (action: () => void) => () => {
    close();
    action();
  };
  const command = (name: NotebookCommand) => run(() => sendNotebookCommand(name));
  const onKeyDown = (event: React.KeyboardEvent) => {
    if (event.key === 'Escape') return close();
    if (!open || (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight')) return;
    event.preventDefault();
    const current = MENU_NAMES.indexOf(open);
    const delta = event.key === 'ArrowRight' ? 1 : -1;
    const next = MENU_NAMES[(current + delta + MENU_NAMES.length) % MENU_NAMES.length];
    setOpen(next);
    document
      .querySelector<HTMLButtonElement>(`.menubar__item[data-menu="${next}"] > button`)
      ?.focus();
  };

  return (
    <div
      className="menubar"
      role="menubar"
      aria-label={t('Notebook menu')}
      data-focus-zone="menubar"
      onKeyDown={onKeyDown}
    >
      <span className="menubar__brand">{t('&Notebook')}</span>
      <MenuItem name="file" label={t('File')} open={open} setOpen={setOpen}>
        <>
          <button role="menuitem" onClick={run(() => openNotebook())}>
            {t('New Notebook')}
          </button>
          <button role="menuitem" onClick={command('open')}>
            {t('Open…')}
          </button>
          <hr />
          <button role="menuitem" onClick={command('runAll')}>
            {t('Run All')}
          </button>
          <hr />
          <button role="menuitem" onClick={command('save')}>
            {t('&Save')}
          </button>
          <button role="menuitem" onClick={command('saveAs')}>
            {t('Save As…')}
          </button>
          <button role="menuitem" onClick={command('saveM')}>
            {t('Save as Script (.m)…')}
          </button>
          <hr />
          <button role="menuitem" onClick={command('closeDocument')}>
            {t('Close Document')}
          </button>
        </>
      </MenuItem>
      <MenuItem name="help" label={t('Help')} open={open} setOpen={setOpen}>
        <>
          <button role="menuitem" onClick={run(() => runInHost("doc('notebook')"))}>
            {t('Notebook Documentation')}
          </button>
          <button role="menuitem" onClick={run(() => runInHost('doc'))}>
            {t('Nelson Documentation')}
          </button>
          <hr />
          <button
            role="menuitem"
            onClick={run(() => window.dispatchEvent(new Event(OPEN_ABOUT_EVENT)))}
          >
            {t('About Nelson')}
          </button>
        </>
      </MenuItem>
      <div className="menubar__spacer" />
    </div>
  );
}
//=============================================================================
