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
import { SettingsPanel } from './settings-panel.js';
import { filterBlockLibraryView } from './block-library-view.js';
import { bindSettingsPreferences } from '../settings-preferences.js';
import { bindScrollPreferences } from '../scroll-preferences.js';
import { openLibraryManager } from '../library-manager.js';
//=============================================================================
let sidebarRoot: Root | null = null;
let mountedSidebar: Element | null = null;
let inspectorRoot: Root | null = null;
let mountedInspector: Element | null = null;
function LibraryPanel(): JSX.Element {
  return (
    <div className="sidebar-panel active" id="panel-library">
      <div className="sidebar-header library-panel-header">
        <span className="pane-heading-copy">
          <span className="pane-eyebrow" data-i18n="Workspace">
            Workspace
          </span>
          <strong data-i18n="Library">Library</strong>
        </span>
        <span className="sidebar-header-actions">
          <button
            id="manageLibrariesBtn"
            type="button"
            className="sidebar-action"
            data-i18n-title="Manage libraries"
            title="Manage libraries"
            aria-label="Manage libraries"
            onClick={openLibraryManager}
          >
            <svg
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              strokeWidth="1.8"
              aria-hidden="true"
            >
              <path d="M4 5h16v14H4z" />
              <path d="M8 9h8M8 13h8M8 17h5" />
            </svg>
          </button>
          <button
            id="sidebarCollapseBtn"
            type="button"
            className="sidebar-action"
            data-i18n-title="Hide library"
            title="Hide library"
            aria-label="Hide library"
            onClick={() =>
              (
                document.querySelector('.activity-icon[data-panel="library"]') as HTMLElement
              )?.click()
            }
          >
            &lsaquo;
          </button>
        </span>
      </div>
      <div id="blockLibraryView" className="block-library-view">
        <div className="library-search">
          <span className="library-search-icon" aria-hidden="true">
            ⌕
          </span>
          <input
            id="blockLibrarySearch"
            type="search"
            autoComplete="off"
            data-i18n-placeholder="Search blocks"
            placeholder="Search blocks"
            aria-label="Search blocks"
          />
          <kbd>Ctrl+K</kbd>
        </div>
        <div className="sidebar-content" data-preference-scroll="library">
          <div id="blockLibraryGroups" className="block-library" />
          <div
            id="blockLibraryEmpty"
            className="block-library-empty"
            hidden
            data-i18n="No blocks found"
          >
            No blocks found
          </div>
          <div className="sidebar-section">
            <button
              id="loadSubsystemBtn"
              className="tool"
              data-i18n-title="Load Subsystem"
              title="Load Subsystem"
            >
              <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" />
                <polyline points="7 10 12 15 17 10" />
                <line x1="12" y1="15" x2="12" y2="3" />
              </svg>
              <span data-i18n="Load Subsystem">Load Subsystem</span>
            </button>
          </div>
        </div>
        <button
          id="quickInsertBtn"
          type="button"
          className="quick-insert-command"
          data-i18n-title="Insert block"
          title="Insert block"
          onClick={() => {
            void import('../quick-insert.js').then((module) =>
              module.openQuickInsertAtCanvasCenter()
            );
          }}
        >
          <span aria-hidden="true">+</span>
          <span data-i18n="Quick insert">Quick insert</span>
        </button>
      </div>
      <div id="libraryManager" hidden />
    </div>
  );
}
//=============================================================================
function PropertiesPanel(): JSX.Element {
  return (
    <div className="sidebar-panel" id="panel-properties">
      <div className="sidebar-header">
        <span className="sidebar-properties-icon" aria-hidden="true" />
        <span data-i18n="Properties">Properties</span>
      </div>
      <div className="sidebar-content" data-preference-scroll="block-inspector">
        <div id="inspectorBody" className="properties-content">
          <div className="properties-placeholder" data-i18n="Select a block to view properties">
            Select a block to view properties
          </div>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
function SimulationPanel(): JSX.Element {
  return (
    <div className="sidebar-panel" id="panel-simulation">
      <div className="sidebar-header">
        <span data-i18n="Simulation">Simulation</span>
      </div>
      <div className="sidebar-content" data-preference-scroll="model-inspector">
        <div className="sim-controls">
          <section className="model-settings-section">
            <h3 data-i18n="Simulation">Simulation</h3>
            <div className="control-group-inline">
              <label htmlFor="simDt" data-i18n="Sample Time (s)">
                Sample Time (s)
              </label>
              <input
                id="simDt"
                type="number"
                defaultValue="0.01"
                step="0.001"
                min="0.0001"
                className="input"
              />
            </div>
            <div className="control-group-inline">
              <label htmlFor="runtimeInput" data-i18n="Runtime (s)">
                Runtime (s)
              </label>
              <input
                id="runtimeInput"
                type="number"
                defaultValue="10"
                step="0.5"
                className="input"
              />
            </div>
            <div className="control-group-inline">
              <label htmlFor="realTimeMode" data-i18n="Execution">
                Execution
              </label>
              <select id="realTimeMode" className="input" defaultValue="auto">
                <option value="auto" data-i18n="Auto">
                  Auto
                </option>
                <option value="real-time" data-i18n="Real-time">
                  Real-time
                </option>
                <option value="fast" data-i18n="Fast">
                  Fast
                </option>
              </select>
            </div>
            <div className="control-group-inline">
              <label htmlFor="simSolver" data-i18n="Solver">
                Solver
              </label>
              <select id="simSolver" className="input" defaultValue="discrete">
                <option value="discrete" data-i18n="Fixed-step (discrete)">
                  Fixed-step (discrete)
                </option>
                <option value="ode1" data-i18n="Fixed-step ode1 (Euler)">
                  Fixed-step ode1 (Euler)
                </option>
                <option value="ode4" data-i18n="Fixed-step ode4 (RK4)">
                  Fixed-step ode4 (RK4)
                </option>
                <option value="ode45" data-i18n="Variable-step ode45 (Dormand-Prince)">
                  Variable-step ode45 (Dormand-Prince)
                </option>
                <option value="variableNonstiff" data-i18n="Variable-step (nonstiff)">
                  Variable-step (nonstiff)
                </option>
                <option value="variableStiff" data-i18n="Variable-step (stiff)">
                  Variable-step (stiff)
                </option>
              </select>
            </div>
            <div id="solverToleranceGroup" style={{ display: 'none' }}>
              <div className="control-group-inline">
                <label htmlFor="simRelTol" data-i18n="Relative tolerance">
                  Relative tolerance
                </label>
                <input
                  id="simRelTol"
                  type="number"
                  defaultValue="1e-6"
                  step="1e-6"
                  min="0"
                  className="input"
                />
              </div>
              <div className="control-group-inline">
                <label htmlFor="simAbsTol" data-i18n="Absolute tolerance">
                  Absolute tolerance
                </label>
                <input
                  id="simAbsTol"
                  type="number"
                  defaultValue="1e-8"
                  step="1e-8"
                  min="0"
                  className="input"
                />
              </div>
              <div className="control-group-inline">
                <label htmlFor="simMaxStep" data-i18n="Max step (0 = auto)">
                  Max step (0 = auto)
                </label>
                <input
                  id="simMaxStep"
                  type="number"
                  defaultValue="0"
                  step="0.001"
                  min="0"
                  className="input"
                />
              </div>
            </div>
            <div className="control-group-inline">
              <label htmlFor="simStopFcn" data-i18n="Stop function">
                Stop function
              </label>
              <textarea
                id="simStopFcn"
                className="input"
                rows={2}
                placeholder="NFlow.plotScopes(out)"
              ></textarea>
            </div>
          </section>
          <div className="explorer-section">
            <div
              className="sidebar-section-title explorer-header"
              role="button"
              aria-expanded="true"
              data-preference-section="variables"
            >
              <span data-i18n="Variables">Variables</span>
              <button className="section-toggle" aria-label="Toggle Variables">
                ▾
              </button>
            </div>
            <div className="sidebar-section-content variables-editor">
              <textarea
                id="variablesInput"
                rows={6}
                data-i18n-placeholder={'k=2\nw=3.14'}
                placeholder={'k=2\nw=3.14'}
              />
              <div className="variables-actions">
                <button id="applyVariables" className="btn btn-sm" data-i18n="Apply">
                  Apply
                </button>
                <button id="resetVariables" className="btn btn-sm" data-i18n="Reset">
                  Reset
                </button>
              </div>
              <pre
                id="variablesPreview"
                className="variables-preview"
                data-i18n="No variables defined."
              >
                No variables defined.
              </pre>
            </div>
          </div>
          <section className="model-settings-section">
            <h3 data-i18n="Diagram">Diagram</h3>
            <div className="control-group-inline">
              <label htmlFor="diagramName" data-i18n="Diagram Name">
                Diagram Name
              </label>
              <input id="diagramName" type="text" defaultValue="nflow" className="input" />
            </div>
            <div className="control-group-inline">
              <label htmlFor="authorInput" data-i18n="Author">
                Author
              </label>
              <input id="authorInput" type="text" defaultValue="" className="input" />
            </div>
            <div className="control-group-inline">
              <label htmlFor="licenseInput" data-i18n="License">
                License
              </label>
              <input id="licenseInput" type="text" defaultValue="" className="input" />
            </div>
            <div className="control-group">
              <label htmlFor="commentsInput" data-i18n="Description">
                Description
              </label>
              <textarea
                id="commentsInput"
                rows={3}
                className="input"
                data-i18n-placeholder="Optional description"
                placeholder="Optional description"
              />
            </div>
          </section>
          <section className="model-settings-section">
            <h3 data-i18n="Model information">Model information</h3>
            <div className="control-group-inline">
              <label htmlFor="modelVersionInput" data-i18n="Model version">
                Model version
              </label>
              <input id="modelVersionInput" type="text" className="input" readOnly />
            </div>
            <div className="control-group-inline">
              <label htmlFor="createdByInput" data-i18n="Created by">
                Created by
              </label>
              <input id="createdByInput" type="text" className="input" readOnly />
            </div>
            <div className="control-group-inline">
              <label htmlFor="createdInput" data-i18n="Created on">
                Created on
              </label>
              <input id="createdInput" type="text" className="input" readOnly />
            </div>
            <div className="control-group-inline">
              <label htmlFor="lastSavedByInput" data-i18n="Last saved by">
                Last saved by
              </label>
              <input
                id="lastSavedByInput"
                type="text"
                className="input"
                readOnly
                data-i18n-placeholder="Not available"
                placeholder="Not available"
              />
            </div>
            <div className="control-group-inline">
              <label htmlFor="lastSavedAtInput" data-i18n="Last saved on">
                Last saved on
              </label>
              <input
                id="lastSavedAtInput"
                type="text"
                className="input"
                readOnly
                data-i18n-placeholder="Not available"
                placeholder="Not available"
              />
            </div>
          </section>
          <section className="model-settings-section">
            <h3 data-i18n="Editing">Editing</h3>
            <div className="control-group">
              <label className="checkbox-label">
                <input id="autoRouteInput" type="checkbox" defaultChecked />
                <span data-i18n="Auto-route wires">Auto-route wires</span>
              </label>
            </div>
            <div className="control-group">
              <label className="checkbox-label">
                <input id="lockedInput" type="checkbox" />
                <span data-i18n="Read-only diagram">Read-only diagram</span>
              </label>
            </div>
          </section>
        </div>
      </div>
    </div>
  );
}
//=============================================================================
function SidebarPanels(): JSX.Element {
  return (
    <>
      <div className="sidebar-resize-handle" />
      <LibraryPanel />
      <SettingsPanel />
    </>
  );
}
//=============================================================================
function InspectorDock(): JSX.Element {
  return (
    <div className="inspector-dock-content">
      <div className="inspector-dock-header">
        <span className="pane-heading-copy">
          <span className="pane-eyebrow" data-i18n="Inspector">
            Inspector
          </span>
          <strong id="inspectorContextTitle" data-i18n="Diagram">
            Diagram
          </strong>
        </span>
        <button
          id="inspectorCollapseBtn"
          type="button"
          className="sidebar-action"
          data-i18n-title="Hide inspector"
          title="Hide inspector"
          aria-label="Hide inspector"
        >
          ›
        </button>
      </div>
      <div className="inspector-tabs" role="tablist">
        <button
          type="button"
          className="inspector-tab active"
          data-inspector-tab="properties"
          role="tab"
          aria-selected="true"
          aria-controls="panel-properties"
          data-i18n="Block"
        >
          Block
        </button>
        <button
          type="button"
          className="inspector-tab"
          data-inspector-tab="simulation"
          role="tab"
          aria-selected="false"
          aria-controls="panel-simulation"
          data-i18n="Model"
        >
          Model
        </button>
      </div>
      <div className="inspector-panels">
        <PropertiesPanel />
        <SimulationPanel />
      </div>
    </div>
  );
}
//=============================================================================
function detectHaveToolchain(name: 'c' | 'rust'): boolean {
  try {
    const key = name === 'c' ? 'nf-have-c-toolchain' : 'nf-have-rust-toolchain';
    let v: string | null = null;
    try {
      v = sessionStorage.getItem(key);
    } catch (_e) {
      v = null;
    }
    if (!v) {
      try {
        v = localStorage.getItem(key);
      } catch (_e) {
        v = null;
      }
    }
    if (v) return v === '1' || v === 'true';
  } catch (_e) {}
  try {
    const s = sessionStorage.getItem('nf-bootstrap');
    if (s) {
      try {
        const b = JSON.parse(s);
        if (b) {
          if (name === 'c' && typeof b.haveCToolChain === 'boolean') return b.haveCToolChain;
          if (name === 'rust' && typeof b.haveRustToolChain === 'boolean')
            return b.haveRustToolChain;
          if (name === 'c' && (b.haveCToolChain === '1' || b.haveCToolChain === '0'))
            return String(b.haveCToolChain) === '1';
          if (name === 'rust' && (b.haveRustToolChain === '1' || b.haveRustToolChain === '0'))
            return String(b.haveRustToolChain) === '1';
        }
      } catch (_e) {}
    }
  } catch (_e) {}
  try {
    const bs = (
      window as unknown as {
        __nf_bootstrap?: {
          haveCToolChain?: boolean | string | number;
          haveRustToolChain?: boolean | string | number;
          [key: string]: unknown;
        };
      }
    ).__nf_bootstrap;
    if (bs && typeof bs === 'object') {
      if (name === 'c' && typeof bs.haveCToolChain === 'boolean') return bs.haveCToolChain;
      if (name === 'rust' && typeof bs.haveRustToolChain === 'boolean') return bs.haveRustToolChain;
      if (name === 'c' && (bs.haveCToolChain === '1' || bs.haveCToolChain === '0'))
        return String(bs.haveCToolChain) === '1';
      if (name === 'rust' && (bs.haveRustToolChain === '1' || bs.haveRustToolChain === '0'))
        return String(bs.haveRustToolChain) === '1';
    }
  } catch (_e) {}
  return false;
}
//=============================================================================
function bindToolchainAvailability(): void {
  const langSel = document.getElementById('codegenLang') as HTMLSelectElement | null;
  const buildChk = document.getElementById('codegenBuildAfterGen') as HTMLInputElement | null;
  if (!langSel || !buildChk) return;

  const updateBuildCheckbox = (): void => {
    const lang = (langSel.value || '').toLowerCase();
    let ok = true;
    if (lang === 'c') ok = detectHaveToolchain('c');
    else if (lang === 'rust') ok = detectHaveToolchain('rust');
    if (!ok) {
      const wasChecked = buildChk.checked;
      buildChk.checked = false;
      if (wasChecked) buildChk.dispatchEvent(new Event('change'));
      buildChk.disabled = true;
      buildChk.title = lang === 'c' ? 'C toolchain not available' : 'Rust toolchain not available';
    } else {
      buildChk.disabled = false;
      buildChk.title = '';
    }
  };

  updateBuildCheckbox();
  langSel.addEventListener('change', updateBuildCheckbox);
  window.addEventListener('storage', (e: StorageEvent) => {
    if (!e.key) return;
    if (
      e.key.includes('nf-have-c-toolchain') ||
      e.key.includes('nf-have-rust-toolchain') ||
      e.key === 'nf-bootstrap'
    ) {
      updateBuildCheckbox();
    }
  });
}
//=============================================================================
function bindLibrarySearch(): void {
  const input = document.getElementById('blockLibrarySearch') as HTMLInputElement | null;
  const container = document.getElementById('blockLibraryGroups');
  if (!input || !container || input.dataset.bound === '1') return;
  input.dataset.bound = '1';
  input.addEventListener('input', () => filterBlockLibraryView(container, input.value));
  if (document.body.dataset.librarySearchShortcut !== '1') {
    document.body.dataset.librarySearchShortcut = '1';
    document.addEventListener('keydown', (event) => {
      if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'k') {
        event.preventDefault();
        const search = document.getElementById('blockLibrarySearch') as HTMLInputElement | null;
        search?.focus();
        search?.select();
      }
    });
  }
}
//=============================================================================
export function mountSidebarPanels(): void {
  const sidebar = document.querySelector('.sidebar');
  if (!sidebar) return;

  if (sidebar !== mountedSidebar) {
    sidebarRoot = createRoot(sidebar);
    mountedSidebar = sidebar;
  }

  flushSync(() => {
    sidebarRoot?.render(<SidebarPanels />);
  });
  const inspector = document.getElementById('inspectorDock');
  if (inspector) {
    if (inspector !== mountedInspector) {
      inspectorRoot = createRoot(inspector);
      mountedInspector = inspector;
    }
    flushSync(() => {
      inspectorRoot?.render(<InspectorDock />);
    });
  }
  try {
    localizeDOM(document);
  } catch (e) {
    /* ignore localization errors */
  }
  try {
    bindSettingsPreferences();
    bindToolchainAvailability();
  } catch (_e) {}
  try {
    bindLibrarySearch();
  } catch (_e) {}
  bindScrollPreferences(sidebar);
  if (inspector) bindScrollPreferences(inspector);
}
//=============================================================================
