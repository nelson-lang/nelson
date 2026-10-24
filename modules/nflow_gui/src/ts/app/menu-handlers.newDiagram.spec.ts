//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import {
  newDiagram,
  setDiagramNameInput,
  setRuntimeInput,
  setSimDt,
  setRealTimeModeInput,
  setAutoRouteInput,
  setVariablesPreview,
  setStatusEl,
} from './menu-handlers.js';
import { state } from './state.js';
//=============================================================================
describe('menu-handlers newDiagram', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
  });

  test('newDiagram resets UI and state', () => {
    const diag = document.createElement('input');
    diag.id = 'diagramName';
    document.body.appendChild(diag);
    const runtime = document.createElement('input');
    runtime.id = 'runtimeInput';
    document.body.appendChild(runtime);
    const sim = document.createElement('input');
    sim.id = 'simDt';
    document.body.appendChild(sim);
    const realTimeMode = document.createElement('select');
    realTimeMode.id = 'realTimeMode';
    realTimeMode.innerHTML =
      '<option value="auto">Auto</option><option value="real-time">Real-time</option><option value="fast">Fast</option>';
    document.body.appendChild(realTimeMode);
    const auto = document.createElement('input');
    auto.type = 'checkbox';
    auto.id = 'autoRouteInput';
    document.body.appendChild(auto);
    const varsPreview = document.createElement('div');
    varsPreview.id = 'variablesPreview';
    document.body.appendChild(varsPreview);
    const status = document.createElement('div');
    status.id = 'status';
    document.body.appendChild(status);

    setDiagramNameInput(diag as any);
    setRuntimeInput(runtime as any);
    setSimDt(sim as any);
    setRealTimeModeInput(realTimeMode as any);
    setAutoRouteInput(auto as any);
    setVariablesPreview(varsPreview as any);
    setStatusEl(status as any);

    const locked = document.createElement('input');
    locked.type = 'checkbox';
    locked.id = 'lockedInput';
    document.body.appendChild(locked);
    state.locked = true;
    locked.checked = true;

    state.diagramName = 'old';
    state.modelVersion = '2.0.0';
    state.currentUser = 'Carol';
    state.author = 'Alice';
    state.createdBy = 'Bob';
    state.created = '2024-01-01';
    state.lastSavedBy = 'Bob';
    state.lastSavedAt = '2025-01-01';
    state.comments = 'Old description';
    state.variablesText = 'x=1';
    state.realTime = false;
    realTimeMode.value = 'fast';

    newDiagram();

    expect(state.diagramName).toBe('New Diagram');
    expect(state.modelVersion).toBe('1.0.0');
    expect(state.author).toBe('Carol');
    expect(state.createdBy).toBe('Carol');
    expect(state.created).not.toBe('2024-01-01');
    expect(state.lastSavedBy).toBe('');
    expect(state.lastSavedAt).toBe('');
    expect(state.comments).toBe('');
    expect(diag.value).toBe('New Diagram');
    expect(runtime.value).toBe('10');
    expect(sim.value).toBe('0.01');
    expect(state.realTime).toBeUndefined();
    expect(realTimeMode.value).toBe('auto');
    expect(auto.checked).toBe(true);
    expect(varsPreview.textContent).toBe('No variables defined.');
    expect(status.textContent).toMatch(/New diagram created/);
    expect(state.locked).toBe(false);
    expect(locked.checked).toBe(false);
    expect(document.body.classList.contains('diagram-locked')).toBe(false);
  });
});
//=============================================================================
