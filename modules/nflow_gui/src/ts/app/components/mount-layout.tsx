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
import { createRoot } from 'react-dom/client';
import type { ReactNode } from 'react';
import { localizeDOM } from '../../utils/i18n.js';
import { onAppEvent } from '../app-events.js';
//=============================================================================
function renderDetachedElement(element: JSX.Element): HTMLElement {
  const host = document.createElement('div');
  const root = createRoot(host);
  flushSync(() => {
    root.render(element);
  });
  return host.firstElementChild as HTMLElement;
}
//=============================================================================
function AppIcon(): JSX.Element {
  return (
    <div id="appIcon" className="app-icon" title="Nelson">
      <svg
        viewBox="0 0 24 24"
        aria-hidden="true"
        focusable="false"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.6"
      >
        <circle cx="12" cy="12" r="9" />
        <text x="12" y="16" textAnchor="middle" fontSize="10" fill="currentColor">
          N
        </text>
      </svg>
      <span className="app-spinner" aria-hidden="true" />
    </div>
  );
}
//=============================================================================
function MenuItem({
  action,
  label,
  shortcut,
  children,
  id,
  disabled = false,
}: {
  action?: string;
  label: string;
  shortcut?: string;
  children?: ReactNode;
  id?: string;
  disabled?: boolean;
}): JSX.Element {
  return (
    <div
      className={disabled ? 'menubar-item disabled' : 'menubar-item'}
      data-action={action}
      id={id}
      aria-disabled={disabled}
    >
      <span data-i18n={label}>{label}</span>
      {shortcut ? <span className="shortcut">{shortcut}</span> : null}
      {children}
    </div>
  );
}
//=============================================================================
function Titlebar(): JSX.Element {
  return (
    <div className="titlebar">
      <AppIcon />
      <div className="titlebar-center">
        <div className="menubar">
          <div className="menubar-item" data-menu="file">
            <span data-i18n="File">File</span>
            <div className="menubar-dropdown" id="menu-file">
              <MenuItem action="new" label="New" shortcut="Ctrl+N" />
              <div className="menubar-divider" />
              <MenuItem action="open" label="Open..." shortcut="Ctrl+O" />
              <MenuItem id="menu-open-recent-wrapper" label="Open Recent">
                <div className="menubar-dropdown" id="menu-open-recent-list" />
              </MenuItem>
              <div className="menubar-divider" />
              <MenuItem action="openSsp" label="Open SSP..." />
              <MenuItem action="importFmu" label="Import FMU..." />
              <div className="menubar-divider" />
              <MenuItem action="save" label="Save" shortcut="Ctrl+S" />
              <MenuItem action="saveAs" label="Save As..." shortcut="Ctrl+Shift+S" />
              <MenuItem action="exportSsp" label="Export as SSP..." />
              <MenuItem action="exportFmu" label="Export as FMU..." />
              {/* Browser runtime only: Open/Save use the engine /workspace, these
                  move a model between it and the user's computer. */}
              <div id="menu-local-files" hidden>
                <div className="menubar-divider" />
                <MenuItem action="importFromComputer" label="Import from This Computer..." />
                <MenuItem action="exportToComputer" label="Export to This Computer..." />
              </div>
              <div className="menubar-divider" />
              <div className="menubar-item">
                <span data-i18n="Export Model to Image">Export Model to Image</span>
                <div className="menubar-dropdown">
                  <div className="menubar-item" data-action="export-svg">
                    <span>SVG</span>
                  </div>
                </div>
              </div>
              <div className="menubar-divider" />
              <MenuItem action="exit" label="Exit" shortcut="Ctrl+Q" />
            </div>
          </div>
          <div className="menubar-item" data-menu="edit">
            <span data-i18n="Edit">Edit</span>
            <div className="menubar-dropdown" id="menu-edit">
              <MenuItem action="undo" label="Undo" shortcut="Ctrl+Z" />
              <MenuItem action="redo" label="Redo" shortcut="Ctrl+Y" />
              <div className="menubar-divider" />
              <MenuItem action="delete" label="Delete" shortcut="Delete" />
            </div>
          </div>
          <div className="menubar-item" data-menu="view">
            <span data-i18n="View">View</span>
            <div className="menubar-dropdown" id="menu-view">
              <MenuItem action="resetView" label="Reset View" shortcut="Ctrl+0" />
              <MenuItem action="zoomIn" label="Zoom In" shortcut="Ctrl++" />
              <MenuItem action="zoomOut" label="Zoom Out" shortcut="Ctrl+-" />
              <div className="menubar-divider" />
              <MenuItem action="toggleConsole" label="Console" />
              <MenuItem action="toggleMinimap" label="Minimap" />
              <MenuItem action="toggleViewControls" label="View Controls" />
              <MenuItem action="toggleSimulationToolbar" label="Simulation Toolbar" />
              <div className="menubar-divider" />
              <MenuItem action="resetUiPreferences" label="Reset UI Preferences" />
            </div>
          </div>
          <div className="menubar-item" data-menu="simulation">
            <span data-i18n="Simulation">Simulation</span>
            <div className="menubar-dropdown" id="menu-simulation">
              <MenuItem
                id="simulationRunAction"
                action="run"
                label="Run or Resume"
                shortcut="Ctrl+T"
              />
              <MenuItem id="simulationPauseAction" action="pause" label="Pause" />
              <MenuItem action="stop" label="Stop" />
              <MenuItem action="reset" label="Reset" />
              <div className="menubar-divider" />
              <MenuItem action="simulationSettings" label="Simulation Settings" />
            </div>
          </div>
          <div className="menubar-item" data-menu="help">
            <span data-i18n="Help">Help</span>
            <div className="menubar-dropdown" id="menu-help">
              <MenuItem
                id="selectedBlockHelpAction"
                action="selectedBlockHelp"
                label="Selected Block Help"
                shortcut="F1"
                disabled
              />
              <div className="menubar-divider" />
              <MenuItem action="openExamples" label="Examples" />
              <div className="menubar-divider" />
              <MenuItem action="about" label="About" />
            </div>
          </div>
        </div>
        <div id="titlebarDocumentName" className="titlebar-title">
          nflow.nflow
        </div>
      </div>
    </div>
  );
}
//=============================================================================
function ActivityIcon({
  panel,
  title,
  active,
  children,
}: {
  panel: string;
  title: string;
  active?: boolean;
  children: ReactNode;
}): JSX.Element {
  return (
    <button
      className={active ? 'activity-icon active' : 'activity-icon'}
      data-panel={panel}
      data-i18n-title={title}
      title={title}
      type="button"
      aria-label={title}
    >
      {children}
    </button>
  );
}
//=============================================================================
function LibraryIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
      <path d="M4 19.5A2.5 2.5 0 0 1 6.5 17H20" />
      <path d="M6.5 2H20v20H6.5A2.5 2.5 0 0 1 4 19.5v-15A2.5 2.5 0 0 1 6.5 2z" />
    </svg>
  );
}
//=============================================================================
function PropertiesIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
      <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" />
      <polyline points="14 2 14 8 20 8" />
      <line x1="16" y1="13" x2="8" y2="13" />
      <line x1="16" y1="17" x2="8" y2="17" />
      <polyline points="10 9 9 9 8 9" />
    </svg>
  );
}
//=============================================================================
function PlayIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
      <polygon points="5 3 19 12 5 21 5 3" />
    </svg>
  );
}
//=============================================================================
function ExamplesIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
      <rect x="3" y="3" width="7" height="7" rx="1" />
      <rect x="14" y="3" width="7" height="7" rx="1" />
      <rect x="3" y="14" width="7" height="7" rx="1" />
      <rect x="14" y="14" width="7" height="7" rx="1" />
    </svg>
  );
}
//=============================================================================
function SettingsIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
      <circle cx="12" cy="12" r="3" />
      <path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1 0 2.83 2 2 0 0 1-2.83 0l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-2 2 2 2 0 0 1-2-2v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83 0 2 2 0 0 1 0-2.83l.06-.06a1.65 1.65 0 0 0 .33-1.82 1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1-2-2 2 2 0 0 1 2-2h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 0-2.83 2 2 0 0 1 2.83 0l.06.06a1.65 1.65 0 0 0 1.82.33H9a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 2-2 2 2 0 0 1 2 2v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 0 2 2 0 0 1 0 2.83l-.06.06a1.65 1.65 0 0 0-.33 1.82V9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 2 2 2 2 0 0 1-2 2h-.09a1.65 1.65 0 0 0-1.51 1z" />
    </svg>
  );
}
//=============================================================================
function FitIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.7">
      <path d="M8 3H3v5M16 3h5v5M8 21H3v-5M16 21h5v-5" />
    </svg>
  );
}
//=============================================================================
function GridIcon(): JSX.Element {
  return (
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.5">
      <path d="M4 4h5v5H4zM15 4h5v5h-5zM4 15h5v5H4zM15 15h5v5h-5z" />
    </svg>
  );
}
//=============================================================================
function CanvasNavigationTools(): JSX.Element {
  return (
    <div className="canvas-navigation-tools" role="toolbar" aria-label="Diagram view controls">
      <button
        id="homeBtn"
        type="button"
        data-i18n-title="Reset View"
        data-shortcut="Ctrl+0"
        title="Reset View (Ctrl+0)"
      >
        <FitIcon />
      </button>
      <button
        id="zoomInBtn"
        type="button"
        data-i18n-title="Zoom In"
        data-shortcut="Ctrl++"
        title="Zoom In (Ctrl++)"
      >
        +
      </button>
      <button
        id="zoomOutBtn"
        type="button"
        data-i18n-title="Zoom Out"
        data-shortcut="Ctrl+-"
        title="Zoom Out (Ctrl+-)"
      >
        -
      </button>
      <span className="canvas-tool-divider" aria-hidden="true" />
      <button id="gridToggleBtn" className="active" type="button" title="Toggle Grid">
        <GridIcon />
      </button>
      <button
        id="gridSettingsBtn"
        type="button"
        data-i18n-title="Grid Settings"
        title="Grid Settings"
      >
        <SettingsIcon />
      </button>
    </div>
  );
}
//=============================================================================
function MainContainer(): JSX.Element {
  return (
    <div className="nflow-container">
      <div className="activity-bar">
        <ActivityIcon panel="library" title="Library" active>
          <LibraryIcon />
        </ActivityIcon>
        <ActivityIcon panel="properties" title="Properties">
          <PropertiesIcon />
        </ActivityIcon>
        <ActivityIcon panel="simulation" title="Simulation">
          <PlayIcon />
        </ActivityIcon>
        <ActivityIcon panel="examples" title="Examples">
          <ExamplesIcon />
        </ActivityIcon>
        <div className="activity-spacer" />
        <ActivityIcon panel="settings" title="Settings">
          <SettingsIcon />
        </ActivityIcon>
      </div>

      <div className="sidebar">
        <div className="sidebar-resize-handle" />
      </div>

      <div className="main-editor">
        <div className="editor-content">
          <div id="canvas" className="canvas-container">
            <div className="subsystem-nav" />
            <svg id="svgCanvas">
              <g id="wireLayer" />
              <g id="blockLayer" />
              <g id="overlayLayer" />
            </svg>
            <CanvasNavigationTools />
          </div>
        </div>
      </div>

      <aside className="inspector-pane" aria-label="Inspector">
        <div className="inspector-resize-handle" />
        <div id="inspectorDock" />
      </aside>
    </div>
  );
}
//=============================================================================
export function createTitlebar(): HTMLElement {
  return renderDetachedElement(<Titlebar />);
}
//=============================================================================
export function createMainContainer(): HTMLElement {
  return renderDetachedElement(<MainContainer />);
}
//=============================================================================
export function createStatusBar(): HTMLElement {
  const status = document.createElement('div');
  status.className = 'status-bar';
  return status;
}
//=============================================================================
export function createHiddenInputs(): HTMLInputElement[] {
  const fileOpen = document.createElement('input');
  fileOpen.id = 'fileOpenInput';
  fileOpen.type = 'file';
  fileOpen.accept = '.nflow,*/*';
  fileOpen.hidden = true;

  const loadSubsystem = document.createElement('input');
  loadSubsystem.id = 'loadSubsystemInput';
  loadSubsystem.type = 'file';
  loadSubsystem.accept = '.nblock';
  loadSubsystem.hidden = true;

  const loadInput = document.createElement('input');
  loadInput.id = 'loadInput';
  loadInput.type = 'file';
  loadInput.accept = '.nflow,*/*';
  loadInput.hidden = true;

  return [fileOpen, loadSubsystem, loadInput];
}
//=============================================================================
export function mountToDOM(
  titlebar: HTMLElement,
  container: HTMLElement,
  status: HTMLElement,
  inputs: HTMLInputElement[]
): void {
  const ref = document.querySelector('.nf-loading-overlay') || document.body.firstChild;
  if (ref && ref.parentNode) {
    ref.parentNode.insertBefore(titlebar, ref.nextSibling);
    ref.parentNode.insertBefore(container, titlebar.nextSibling);
    ref.parentNode.insertBefore(status, container.nextSibling);
    let after = status.nextSibling as Node | null;
    for (const inp of inputs) {
      try {
        ref.parentNode.insertBefore(inp, after);
      } catch (_e) {
        ref.parentNode.appendChild(inp);
      }
      after = inp.nextSibling;
    }
  } else {
    document.body.appendChild(titlebar);
    document.body.appendChild(container);
    document.body.appendChild(status);
    for (const inp of inputs) document.body.appendChild(inp);
  }
}
//=============================================================================
function bindDocumentTitle(): void {
  const target = document.getElementById('titlebarDocumentName');
  if (!target || target.dataset.bound === '1') return;
  target.dataset.bound = '1';
  const update = (name?: string): void => {
    const fromTitle = document.title.replace(/^NFlow(?:\s*-\s*)?/, '').trim();
    const base = (name || fromTitle || 'nflow').trim();
    target.textContent = base.toLowerCase().endsWith('.nflow') ? base : `${base}.nflow`;
  };
  update();
  const title = document.querySelector('title');
  if (title && typeof MutationObserver !== 'undefined') {
    new MutationObserver(() => update()).observe(title, { childList: true });
  }
  onAppEvent('nf-diagram-name-change', (detail) => {
    update(String(detail?.name || ''));
  });
}
//=============================================================================
/** Try to initialise the floating toolbar if the script exposed its initializer. */
export function initFloatingToolbarIfAvailable(): void {
  try {
    const initFT = (globalThis as unknown as { initFloatingToolbar?: unknown }).initFloatingToolbar;
    if (typeof initFT === 'function') {
      try {
        (initFT as () => void)();
      } catch (_e) {
        /* ignore init errors */
      }
    }
  } catch (_e) {
    /* ignore global access errors */
  }
}
//=============================================================================
/** Compose and mount the full layout if it's not already present. */
export function mountLayout(): void {
  try {
    if (document.querySelector('.nflow-container') || document.querySelector('.titlebar')) return;
    const titlebar = createTitlebar();
    const container = createMainContainer();
    const status = createStatusBar();
    const inputs = createHiddenInputs();
    mountToDOM(titlebar, container, status, inputs);
    bindDocumentTitle();
    try {
      const toolbar = document.getElementById('floatingToolbar');
      const canvas = container.querySelector('#canvas');
      if (toolbar && canvas) canvas.appendChild(toolbar);
    } catch (_e) {
      /* the simulation toolbar is optional */
    }
    try {
      localizeDOM(document);
    } catch (_e) {
      /* ignore localization errors */
    }
    initFloatingToolbarIfAvailable();
  } catch (_e) {
    /* ignore DOM errors */
  }
}
//=============================================================================
export default mountLayout;
//=============================================================================
