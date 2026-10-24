//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { _resetUiPrefsForTests } from './ui-prefs.js';
import { activateInspectorTab, initInspectorDock, setInspectorOpen } from './workspace-layout.js';
//=============================================================================
beforeEach(() => {
  _resetUiPrefsForTests();
  localStorage.clear();
  document.body.innerHTML = `
    <div class="sidebar"></div>
    <aside class="inspector-pane">
      <div class="inspector-resize-handle"></div>
      <button id="inspectorCollapseBtn"></button>
      <button data-inspector-tab="properties"></button>
      <button data-inspector-tab="simulation"></button>
      <div id="panel-properties" class="sidebar-panel"></div>
      <div id="panel-simulation" class="sidebar-panel"></div>
    </aside>`;
});
//=============================================================================
test('activates the requested inspector tab and persists it', () => {
  activateInspectorTab('simulation');
  expect(document.getElementById('panel-simulation')?.classList.contains('active')).toBe(true);
  expect(localStorage.getItem('nf-inspector-tab')).toBe('simulation');
});
//=============================================================================
test('opens and closes the inspector with persisted state', () => {
  initInspectorDock();
  setInspectorOpen(false);
  expect(document.querySelector('.inspector-pane')?.classList.contains('collapsed')).toBe(true);
  expect(localStorage.getItem('nf-inspector-open')).toBe('0');
  document.dispatchEvent(new CustomEvent('nf-inspector-show', { detail: { tab: 'properties' } }));
  expect(document.querySelector('.inspector-pane')?.classList.contains('collapsed')).toBe(false);
  expect(document.getElementById('panel-properties')?.classList.contains('active')).toBe(true);
});
//=============================================================================
