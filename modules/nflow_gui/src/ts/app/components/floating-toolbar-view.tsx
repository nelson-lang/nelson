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
//=============================================================================
type FloatingToolbarViewProps = {
  running: boolean;
  pending: boolean;
  stoppable: boolean;
  runnable: boolean;
  currentTime: number;
  targetTime: number;
  progress: number;
  onToggleRun: () => void;
  onOpenSettings: () => void;
};
//=============================================================================
const roots = new WeakMap<HTMLElement, Root>();
function RunIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.6">
      <path d="M7 5l12 7-12 7z" />
    </svg>
  );
}
//=============================================================================
function PauseIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.6">
      <path d="M7 5h4v14H7zM13 5h4v14h-4z" />
    </svg>
  );
}
//=============================================================================
function StopIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.6">
      <rect x="6" y="6" width="12" height="12" />
    </svg>
  );
}
//=============================================================================
function ResetIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.6">
      <path d="M20 11a8 8 0 1 0-2.34 5.66" />
      <path d="M20 5v6h-6" />
    </svg>
  );
}
//=============================================================================
function SettingsIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.6">
      <circle cx="12" cy="12" r="3" />
      <path d="M19.4 15a1.7 1.7 0 0 0 .34 1.88l.06.06-2.83 2.83-.06-.06a1.7 1.7 0 0 0-1.88-.34 1.7 1.7 0 0 0-1.03 1.56V21h-4v-.08A1.7 1.7 0 0 0 8.94 19.4a1.7 1.7 0 0 0-1.88.34l-.06.06-2.83-2.83.06-.06A1.7 1.7 0 0 0 4.57 15 1.7 1.7 0 0 0 3 14H3v-4h.08A1.7 1.7 0 0 0 4.6 8.94a1.7 1.7 0 0 0-.34-1.88L4.2 7l2.83-2.83.06.06A1.7 1.7 0 0 0 9 4.57 1.7 1.7 0 0 0 10 3h4v.08a1.7 1.7 0 0 0 1.06 1.52 1.7 1.7 0 0 0 1.88-.34L17 4.2 19.83 7l-.06.06a1.7 1.7 0 0 0-.34 1.88A1.7 1.7 0 0 0 21 10v4h-.08A1.7 1.7 0 0 0 19.4 15z" />
    </svg>
  );
}
//=============================================================================
function FloatingToolbarView({
  running,
  pending,
  stoppable,
  runnable,
  currentTime,
  targetTime,
  progress,
  onToggleRun,
  onOpenSettings,
}: FloatingToolbarViewProps): JSX.Element {
  const title = running ? 'Pause' : 'Run';
  return (
    <>
      <button
        id="playPauseBtn"
        className={`run-button${running ? ' running' : ''}`}
        data-i18n-title={title}
        data-action={running ? 'pause' : 'run'}
        aria-pressed={running ? 'true' : 'false'}
        title={title}
        disabled={!runnable || pending}
        onClick={onToggleRun}
      >
        {running ? <PauseIcon /> : <RunIcon />}
        <span className="run-label">{title}</span>
      </button>
      <button
        id="stopSimToolbarBtn"
        data-action="stop"
        data-i18n-title="Stop"
        title="Stop"
        disabled={!stoppable}
      >
        <StopIcon />
      </button>
      <button id="resetSimToolbarBtn" data-action="reset" data-i18n-title="Reset" title="Reset">
        <ResetIcon />
      </button>
      <span className="simulation-command-divider" aria-hidden="true" />
      <span id="simTimeDisplay" className="simulation-time-display" aria-live="polite">
        {currentTime.toFixed(2)} / {targetTime.toFixed(2)} s
      </span>
      <span
        className="simulation-progress-track"
        role="progressbar"
        aria-label="Simulation progress"
        aria-valuemin={0}
        aria-valuemax={100}
        aria-valuenow={Math.round(progress)}
      >
        <span style={{ width: `${progress}%` }} />
      </span>
      <button
        id="simSettingsToolbarBtn"
        type="button"
        data-i18n-title="Simulation Settings"
        title="Simulation Settings"
        aria-label="Simulation Settings"
        onClick={onOpenSettings}
      >
        <SettingsIcon />
      </button>
    </>
  );
}
//=============================================================================
export function renderFloatingToolbarView(
  container: HTMLElement,
  props: FloatingToolbarViewProps
): void {
  let root = roots.get(container);
  if (!root) {
    if (container.childNodes.length) container.replaceChildren();
    root = createRoot(container);
    roots.set(container, root);
  }
  flushSync(() => {
    root?.render(<FloatingToolbarView {...props} />);
  });
}
//=============================================================================
