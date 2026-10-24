//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
function CodeGenerationSettings(): JSX.Element {
  return (
    <div className="settings-section">
      <h4 data-i18n="Code Generation">Code Generation</h4>
      <div className="control-group">
        <label htmlFor="codegenLang" data-i18n="Language">
          Language
        </label>
        <select id="codegenLang" className="input" defaultValue="rust">
          <option value="rust">Rust</option>
          <option value="c">C</option>
        </select>
      </div>
      <div className="control-group">
        <label className="checkbox-label">
          <input id="codegenIncludeMain" type="checkbox" defaultChecked />
          <span data-i18n="Include main()">Include main()</span>
        </label>
      </div>
      <div className="control-group">
        <label className="checkbox-label">
          <input id="codegenBuildAfterGen" type="checkbox" />
          <span data-i18n="Build after generation">Build after generation</span>
        </label>
      </div>
      <button id="codegenBtn" className="btn btn-secondary" data-i18n="Generate Code">
        Generate Code
      </button>
    </div>
  );
}
//=============================================================================
function ModelicaSettings(): JSX.Element {
  return (
    <div className="settings-section" id="modelicaSettingsSection">
      <h4 data-i18n="Modelica (OpenModelica)">Modelica (OpenModelica)</h4>
      <div className="control-group">
        <label htmlFor="modelicaPathInput" data-i18n="OpenModelica path">
          OpenModelica path
        </label>
        <input
          id="modelicaPathInput"
          className="input"
          type="text"
          placeholder="auto-detected, set to override"
        />
      </div>
      <button id="modelicaBrowseBtn" className="btn btn-secondary" data-i18n="Browse...">
        Browse...
      </button>
      <div
        id="modelicaPathStatus"
        className="settings-detection-status"
        data-state="checking"
        role="status"
        aria-live="polite"
      >
        <span className="settings-detection-indicator" aria-hidden="true" />
        <span className="settings-detection-copy">
          <strong id="modelicaPathStatusLabel">Checking OpenModelica...</strong>
          <small id="modelicaPathStatusDetails" />
        </span>
      </div>
      <div
        className="settings-note"
        data-i18n="Used to compile Modelica blocks to FMUs. Leave empty to auto-detect."
      >
        Used to compile Modelica blocks to FMUs. Leave empty to auto-detect.
      </div>
    </div>
  );
}
//=============================================================================
export function SettingsPanel(): JSX.Element {
  return (
    <div className="sidebar-panel" id="panel-settings">
      <div className="sidebar-header">
        <span data-i18n="Settings">Settings</span>
      </div>
      <div className="sidebar-content" data-preference-scroll="settings">
        <CodeGenerationSettings />
        <ModelicaSettings />
      </div>
    </div>
  );
}
//=============================================================================
