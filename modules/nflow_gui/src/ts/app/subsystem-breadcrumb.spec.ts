//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { updateSubsystemNavUi } from './diagram-handlers.js';
import { state } from './state.js';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = `<div class="subsystem-nav"></div>`;
  state.subsystemStack = [];
  state.diagramName = 'Root Diagram';
});
//=============================================================================
test('breadcrumb hidden at root level', () => {
  updateSubsystemNavUi();
  const bc = document.getElementById('subsystemBreadcrumb');
  expect(bc).not.toBeNull();
  expect(bc?.hidden).toBe(true);
});
//=============================================================================
test('breadcrumb shows the clickable path inside nested subsystems', () => {
  state.subsystemStack = [
    { parentDiagram: { name: 'Root Diagram' }, parentUiState: {}, hostBlockId: 'a' },
    { parentDiagram: { name: 'Motor' }, parentUiState: {}, hostBlockId: 'b' },
  ];
  state.diagramName = 'Controller';
  updateSubsystemNavUi();
  const bc = document.getElementById('subsystemBreadcrumb')!;
  expect(bc.hidden).toBe(false);
  const items = bc.querySelectorAll('.breadcrumb-item');
  expect(items.length).toBe(2);
  expect(items[0].textContent).toBe('Root Diagram');
  expect(items[1].textContent).toBe('Motor');
  expect(bc.querySelector('.breadcrumb-current')?.textContent).toBe('Controller');
});
//=============================================================================
test('breadcrumb refreshes when navigating back to root', () => {
  state.subsystemStack = [
    { parentDiagram: { name: 'Root Diagram' }, parentUiState: {}, hostBlockId: 'a' },
  ];
  updateSubsystemNavUi();
  expect(document.getElementById('subsystemBreadcrumb')?.hidden).toBe(false);
  state.subsystemStack = [];
  updateSubsystemNavUi();
  expect(document.getElementById('subsystemBreadcrumb')?.hidden).toBe(true);
});
//=============================================================================
