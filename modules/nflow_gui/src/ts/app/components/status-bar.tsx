//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { flushSync } from 'react-dom';
import { createRoot, type Root } from 'react-dom/client';
import { localizeDOM } from '../../utils/i18n.js';
import { emitAppEvent } from '../app-events.js';
//=============================================================================
let statusRoot: Root | null = null;
let statusContainer: Element | null = null;
function InfoIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
      <circle cx="12" cy="12" r="10" />
      <line x1="12" y1="16" x2="12" y2="12" />
      <line x1="12" y1="8" x2="12.01" y2="8" />
    </svg>
  );
}
//=============================================================================
function LockIcon(): JSX.Element {
  return (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="2"
      width="18"
      height="18"
    >
      <rect x="3" y="11" width="18" height="10" rx="2" ry="2" />
      <path d="M7 11V8a5 5 0 0 1 10 0v3" />
    </svg>
  );
}
//=============================================================================
function StatusBar(): JSX.Element {
  return (
    <>
      <div id="statusBar">
        <div className="status-item">
          <InfoIcon />
          <span
            id="statusBarInfo"
            role="status"
            aria-live="polite"
            aria-atomic="true"
            data-i18n="Ready"
          >
            Ready
          </span>
        </div>
        <span id="statusBarTime" className="visually-hidden">
          0.00s
        </span>
        <div className="status-spacer" />
        <div
          className="status-item status-lock"
          id="lockIndicator"
          data-i18n-title="Diagram locked"
          title="Diagram locked"
          aria-hidden="true"
        >
          <LockIcon />
        </div>
        <div className="status-item diagnostic-status">
          <button
            type="button"
            className="diagnostic-counter diagnostic-errors"
            title="Open diagnostics"
            aria-label="Open error diagnostics"
            onClick={() => emitAppEvent('nf-open-diagnostics')}
          >
            <span aria-hidden="true">●</span>
            <span id="diagnosticErrorCount">0</span>
          </button>
          <button
            type="button"
            className="diagnostic-counter diagnostic-warnings"
            title="Open diagnostics"
            aria-label="Open warning diagnostics"
            onClick={() => emitAppEvent('nf-open-diagnostics')}
          >
            <span aria-hidden="true">▲</span>
            <span id="diagnosticWarningCount">0</span>
          </button>
        </div>
        <div className="status-item">
          <span id="statusBarBlocks" data-i18n="0 blocks">
            0 blocks
          </span>
        </div>
        <div className="status-item">
          <span id="statusBarConnections" data-i18n="0 connections">
            0 connections
          </span>
        </div>
        <div className="status-item">
          <span id="statusBarZoom">100%</span>
        </div>
      </div>
      <div
        id="statusBarProgress"
        role="progressbar"
        aria-label="Simulation progress"
        aria-valuemin={0}
        aria-valuemax={100}
        aria-valuenow={0}
        hidden
      />
    </>
  );
}
//=============================================================================
export function mountStatusBar(): void {
  const container = document.querySelector('.status-bar');
  if (!container) return;
  if (container !== statusContainer) {
    statusContainer = container;
    statusRoot = createRoot(container);
  }
  flushSync(() => {
    statusRoot?.render(<StatusBar />);
  });
  try {
    localizeDOM(document);
  } catch (e) {
    /* ignore localization errors */
  }
}
//=============================================================================
