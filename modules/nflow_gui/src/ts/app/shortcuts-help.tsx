//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { Fragment } from 'react';
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
import * as FieldNames from '../FieldNames.js';
import { t } from '../utils/i18n.js';
//=============================================================================
const SHORTCUTS_ROOT_ID = 'nflow-shortcuts-root';
let root: Root | null = null;
let host: HTMLDivElement | null = null;
function shortcuts(): [string, string][] {
  const isMac = navigator.platform.toUpperCase().indexOf('MAC') >= 0;
  const mod = isMac ? 'Cmd' : 'Ctrl';
  return [
    ['Escape', t('Cancel pending action / clear selection or close dialogs')],
    ['Delete / Backspace', t('Delete selected block or wire')],
    ['Arrow Keys', t('Nudge selected block by grid step (hold Shift for x10)')],
    ['Click + Drag', t('Move blocks (drag handle) - Middle button or Space+drag to pan canvas')],
    [`${mod} + S`, t('Save diagram')],
    [`${mod} + Z`, t('Undo (if available)')],
    [`${mod} + Shift + Z`, t('Redo (or {alt})', { alt: isMac ? `${mod}+Y` : 'Ctrl+Y' })],
    ['Ctrl/Cmd + D', t('Duplicate selected block(s)')],
    ['Ctrl/Cmd + C / X / V', t('Copy / Cut / Paste selected blocks')],
    ['F1 or ?', t('Open this help overlay')],
    ['Ctrl + Wheel / Pinch', t('Zoom in/out (use Ctrl or gesture)')],
    ['R', t('Rotate selected block 90°')],
    ['G', t('Toggle snap-to-grid')],
    ['Ctrl/Cmd + A', t('Select all blocks')],
    ['Shift + Click', t('Add/remove block to selection (multi-select)')],
    [`${mod} + Shift + ← / →`, t('Align selected blocks (2+) to left / right edge')],
    [`${mod} + Shift + ↑ / ↓`, t('Align selected blocks (2+) to top / bottom edge')],
  ];
}
//=============================================================================
function ShortcutsHelpOverlay(): JSX.Element {
  return (
    <>
      <div
        id="nflow-shortcuts-backdrop"
        style={{
          position: 'fixed',
          left: '0',
          top: '0',
          right: '0',
          bottom: '0',
          background: 'rgba(0,0,0,0.45)',
          zIndex: 2999,
        }}
        onClick={hideShortcutsHelp}
      />
      <div
        id="nflow-shortcuts-help"
        role="dialog"
        aria-modal="true"
        aria-label={t('Keyboard Shortcuts')}
        style={{
          position: 'fixed',
          left: '50%',
          top: '50%',
          transform: 'translate(-50%, -50%)',
          zIndex: 3000,
          minWidth: '360px',
          maxWidth: '90vw',
          maxHeight: '80vh',
          overflow: 'auto',
          background: 'var(--nflow-editor-background, #fff)',
          color: 'var(--nflow-editor-foreground, #222)',
          border: '1px solid var(--nflow-editorWidget-border, rgba(0,0,0,0.12))',
          boxShadow: '0 8px 30px rgba(0,0,0,0.25)',
          borderRadius: '8px',
          padding: '16px',
          display: 'block',
        }}
      >
        <h3 style={{ marginTop: 0, marginBottom: '8px' }}>{t('Keyboard Shortcuts')}</h3>
        <dl
          style={{
            display: 'grid',
            gridTemplateColumns: '1fr auto',
            rowGap: '8px',
            columnGap: '16px',
          }}
        >
          {shortcuts().map(([key, description]) => {
            const Dt = FieldNames.kDt as 'dt';
            return (
              <Fragment key={key}>
                <Dt style={{ fontWeight: 600 }}>{key}</Dt>
                <dd style={{ margin: 0 }}>{description}</dd>
              </Fragment>
            );
          })}
        </dl>
        <button style={{ marginTop: '12px' }} onClick={hideShortcutsHelp}>
          {t('Close')}
        </button>
      </div>
    </>
  );
}
//=============================================================================
function onKeyDown(event: KeyboardEvent): void {
  if (event.key === 'Escape') hideShortcutsHelp();
}
//=============================================================================
function ensureHost(): HTMLDivElement {
  const existing = document.getElementById(SHORTCUTS_ROOT_ID) as HTMLDivElement | null;
  if (existing && root) return existing;

  const nextHost = document.createElement('div');
  nextHost.id = SHORTCUTS_ROOT_ID;
  nextHost.dataset.reactRoot = 'shortcuts-help';
  nextHost.style.display = 'contents';
  document.body.appendChild(nextHost);
  host = nextHost;
  root = createRoot(nextHost);
  return nextHost;
}
//=============================================================================
export function initShortcutsHelp(): void {
  /* noop for now */
}
//=============================================================================
export function showShortcutsHelp(): void {
  try {
    if (document.getElementById('nflow-shortcuts-help')) return;
    ensureHost();
    flushSync(() => {
      root?.render(<ShortcutsHelpOverlay />);
    });
    window.addEventListener('keydown', onKeyDown);
  } catch (e) {
    /* ignore */
  }
}
//=============================================================================
export function hideShortcutsHelp(): void {
  try {
    window.removeEventListener('keydown', onKeyDown);
    root?.unmount();
    root = null;
    const activeHost =
      host || (document.getElementById(SHORTCUTS_ROOT_ID) as HTMLDivElement | null);
    if (activeHost?.parentNode) activeHost.parentNode.removeChild(activeHost);
    host = null;
  } catch (e) {
    /* ignore */
  }
}
//=============================================================================
