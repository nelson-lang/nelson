//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Collapsible bottom console panel hosted under the editor.
//=============================================================================
import { useEffect, useState } from 'react';
import { flushSync } from 'react-dom';
import { createRoot } from 'react-dom/client';
import ConsolePanel from './ConsolePanel.js';
import DiagnosticsPanel from './DiagnosticsPanel.js';
import { getUiPref, setUiPref } from '../app/ui-prefs.js';
import { refreshDiagnosticCounters, subscribeDiagnostics } from '../app/diagnostics-service.js';
import { t } from '../utils/i18n.js';
import { onAppEvent } from '../app/app-events.js';
//=============================================================================
// The Diagnostics tab badge subscribes to the same diagnostics service as the
// panel, so the two cannot disagree.
export function DiagnosticTabBadge(): JSX.Element {
  const [count, setCount] = useState(0);
  useEffect(
    () =>
      subscribeDiagnostics((items) => {
        setCount(
          items.filter((item) => item.severity === 'error' || item.severity === 'warning').length
        );
      }),
    []
  );
  return (
    <span id="diagnosticTabCount" className={`bottom-panel-count${count > 0 ? ' active' : ''}`}>
      {count}
    </span>
  );
}
//=============================================================================
const OPEN_KEY = 'nf-console-open';
const HEIGHT_KEY = 'nf-console-height';
const TAB_KEY = 'nf-bottom-panel-tab';
const DEFAULT_HEIGHT = 128;
const MIN_HEIGHT = 60;
const MAX_HEIGHT_RATIO = 0.6;
function BottomConsoleShell(): JSX.Element {
  return (
    <>
      <div className="bottom-panel-resize" aria-hidden="true" />
      <div className="bottom-panel-header" role="tablist" aria-label={t('Output panels')}>
        <button
          type="button"
          className="bottom-panel-title bottom-panel-tab active"
          data-bottom-tab="console"
          role="tab"
          aria-selected="true"
          aria-controls="consolePanelHost"
          data-i18n="Console"
        >
          {t('Console')}
        </button>
        <button
          type="button"
          className="bottom-panel-tab"
          data-bottom-tab="diagnostics"
          role="tab"
          aria-selected="false"
          aria-controls="diagnosticsPanelHost"
        >
          <span data-i18n="Diagnostics">{t('Diagnostics')}</span>
          <DiagnosticTabBadge />
        </button>
        <span className="bottom-panel-spacer" />
        <button
          id="consoleToggleBtn"
          className="bottom-panel-toggle"
          data-i18n-title="Toggle console"
          title={t('Toggle console')}
          aria-expanded="false"
          aria-controls="consolePanelHost"
        >
          <svg
            className="bottom-panel-chevron"
            viewBox="0 0 24 24"
            width="14"
            height="14"
            fill="none"
            stroke="currentColor"
            strokeWidth="2"
          >
            <path d="M6 15l6-6 6 6" />
          </svg>
        </button>
      </div>
      <div className="bottom-panel-content">
        <div id="consolePanelHost" className="bottom-panel-view active" data-bottom-view="console">
          <ConsolePanel />
        </div>
        <div
          id="diagnosticsPanelHost"
          className="bottom-panel-view"
          data-bottom-view="diagnostics"
          hidden
        >
          <DiagnosticsPanel />
        </div>
      </div>
    </>
  );
}
//=============================================================================
function readBool(key: string, fallback: boolean): boolean {
  const v = getUiPref(key, null);
  return v === null ? fallback : v === '1';
}
//=============================================================================
function readHeight(): number {
  const v = Number(getUiPref(HEIGHT_KEY, null));
  const maxHeight = Math.max(MIN_HEIGHT, Math.round(window.innerHeight * MAX_HEIGHT_RATIO));
  return Number.isFinite(v) && v >= MIN_HEIGHT ? Math.min(v, maxHeight) : DEFAULT_HEIGHT;
}
//=============================================================================
function applyState(panel: HTMLElement, open: boolean, height: number): void {
  panel.classList.toggle('collapsed', !open);
  const content = panel.querySelector<HTMLElement>('.bottom-panel-content');
  if (content) content.style.height = open ? `${height}px` : '0px';
  const chevron = panel.querySelector<HTMLElement>('.bottom-panel-chevron');
  if (chevron) chevron.style.transform = open ? 'rotate(180deg)' : '';
  const btn = panel.querySelector<HTMLElement>('#consoleToggleBtn');
  if (btn) btn.setAttribute('aria-expanded', open ? 'true' : 'false');
}
//=============================================================================
function activateTab(panel: HTMLElement, tab: 'console' | 'diagnostics'): void {
  panel.querySelectorAll<HTMLElement>('[data-bottom-tab]').forEach((button) => {
    const active = button.dataset.bottomTab === tab;
    button.classList.toggle('active', active);
    button.setAttribute('aria-selected', active ? 'true' : 'false');
  });
  panel.querySelectorAll<HTMLElement>('[data-bottom-view]').forEach((view) => {
    const active = view.dataset.bottomView === tab;
    view.classList.toggle('active', active);
    view.hidden = !active;
  });
  setUiPref(TAB_KEY, tab);
}
//=============================================================================
export function openBottomPanelTab(tab: 'console' | 'diagnostics'): void {
  const panel = document.getElementById('bottomPanel');
  if (!panel) return;
  activateTab(panel, tab);
  applyState(panel, true, readHeight());
  setUiPref(OPEN_KEY, '1');
}
//=============================================================================
export function toggleConsolePanel(force?: boolean): void {
  const panel = document.getElementById('bottomPanel');
  if (!panel) return;
  const open = force !== undefined ? force : panel.classList.contains('collapsed');
  if (open) activateTab(panel, 'console');
  applyState(panel, open, readHeight());
  setUiPref(OPEN_KEY, open ? '1' : '0');
}
//=============================================================================
/** Mount the bottom console panel under the editor. Idempotent. */
export function mountConsolePanel(): void {
  try {
    if (document.getElementById('bottomPanel')) return;
    const mainEditor = document.querySelector('.main-editor');
    if (!mainEditor) return;

    const panel = document.createElement('div');
    panel.id = 'bottomPanel';
    panel.className = 'bottom-panel';
    panel.dataset.reactRoot = 'bottom-console';
    mainEditor.appendChild(panel);
    const root = createRoot(panel);
    flushSync(() => root.render(<BottomConsoleShell />));

    panel.querySelectorAll<HTMLElement>('[data-bottom-tab]').forEach((button) => {
      button.addEventListener('click', () => {
        const tab = button.dataset.bottomTab === 'diagnostics' ? 'diagnostics' : 'console';
        openBottomPanelTab(tab);
      });
    });
    panel.querySelector('.bottom-panel-header')?.addEventListener('click', (event) => {
      if ((event.target as Element | null)?.closest('button')) return;
      const open = panel.classList.contains('collapsed');
      applyState(panel, open, readHeight());
      setUiPref(OPEN_KEY, open ? '1' : '0');
    });
    panel.querySelector('#consoleToggleBtn')?.addEventListener('click', () => {
      const open = panel.classList.contains('collapsed');
      applyState(panel, open, readHeight());
      setUiPref(OPEN_KEY, open ? '1' : '0');
    });
    onAppEvent('nf-open-diagnostics', () => openBottomPanelTab('diagnostics'));

    const grip = panel.querySelector<HTMLElement>('.bottom-panel-resize');
    if (grip) {
      let startY = 0;
      let startH = 0;
      const onMove = (ev: PointerEvent): void => {
        const maxH = Math.round(window.innerHeight * MAX_HEIGHT_RATIO);
        const h = Math.min(maxH, Math.max(MIN_HEIGHT, startH + (startY - ev.clientY)));
        const content = panel.querySelector<HTMLElement>('.bottom-panel-content');
        if (content) content.style.height = `${h}px`;
        setUiPref(HEIGHT_KEY, String(h));
      };
      grip.addEventListener('pointerdown', (ev) => {
        if (panel.classList.contains('collapsed')) return;
        startY = ev.clientY;
        const content = panel.querySelector<HTMLElement>('.bottom-panel-content');
        startH = content ? content.offsetHeight : DEFAULT_HEIGHT;
        const onUp = (): void => {
          window.removeEventListener('pointermove', onMove);
          window.removeEventListener('pointerup', onUp);
        };
        window.addEventListener('pointermove', onMove);
        window.addEventListener('pointerup', onUp);
        ev.preventDefault();
      });
    }

    const savedTab = getUiPref(TAB_KEY, 'console') === 'diagnostics' ? 'diagnostics' : 'console';
    activateTab(panel, savedTab);
    applyState(panel, readBool(OPEN_KEY, false), readHeight());
    refreshDiagnosticCounters();
  } catch (_e) {
    /* the console panel must never break app startup */
  }
}
//=============================================================================
export default mountConsolePanel;
//=============================================================================
