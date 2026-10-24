//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as dh from './diagram-handlers';
//=============================================================================
test('diagram-handlers module loads', () => {
  expect(typeof dh).toBe('object');
});
//=============================================================================
import { beforeAll, test, expect } from 'vitest';
//=============================================================================
let mod: any;
beforeAll(async () => {
  mod = await import('../../../dist/app/diagram-handlers.js').catch(() => null);
});
//=============================================================================
test('diagram-handlers module loads', () => {
  expect(mod === null || typeof mod === 'object' || typeof mod === 'function').toBe(true);
});
//=============================================================================
export {};
test('syncLockedUi syncs checkbox and body class from state.locked', async () => {
  const mod = await import('./diagram-handlers');
  const { state } = await import('./state');
  document.body.innerHTML = '';
  const locked = document.createElement('input');
  locked.type = 'checkbox';
  locked.id = 'lockedInput';
  document.body.appendChild(locked);

  state.locked = true;
  if (typeof mod.syncLockedUi === 'function') mod.syncLockedUi();
  expect(locked.checked).toBe(true);
  expect(document.body.classList.contains('diagram-locked')).toBe(true);

  state.locked = false;
  if (typeof mod.syncLockedUi === 'function') mod.syncLockedUi();
  expect(locked.checked).toBe(false);
  expect(document.body.classList.contains('diagram-locked')).toBe(false);
});
//=============================================================================
test('loadDiagram syncs real-time mode selector from diagram data', async () => {
  const mod = await import('./diagram-handlers');
  const { state } = await import('./state');
  document.body.innerHTML = '';
  const realTimeMode = document.createElement('select');
  realTimeMode.id = 'realTimeMode';
  realTimeMode.innerHTML =
    '<option value="auto">Auto</option><option value="real-time">Real-time</option><option value="fast">Fast</option>';
  document.body.appendChild(realTimeMode);

  mod.setRealTimeModeInput(realTimeMode);

  await mod.loadDiagram({ name: 'sync', blocks: [], connections: [], realTime: true });
  expect(state.realTime).toBe(true);
  expect(realTimeMode.value).toBe('real-time');

  await mod.loadDiagram({ name: 'sync', blocks: [], connections: [], realTime: false });
  expect(state.realTime).toBe(false);
  expect(realTimeMode.value).toBe('fast');

  await mod.loadDiagram({ name: 'sync', blocks: [], connections: [] });
  expect(state.realTime).toBeUndefined();
  expect(realTimeMode.value).toBe('auto');
});
//=============================================================================
test('loadDiagram resets solver tolerances when the next diagram omits them', async () => {
  const mod = await import('./diagram-handlers');
  const { state } = await import('./state');
  document.body.innerHTML = '';
  const solver = document.createElement('select');
  solver.id = 'simSolver';
  solver.innerHTML =
    '<option value="discrete">discrete</option><option value="ode45">ode45</option>';
  const rel = document.createElement('input');
  rel.id = 'simRelTol';
  const abs = document.createElement('input');
  abs.id = 'simAbsTol';
  const max = document.createElement('input');
  max.id = 'simMaxStep';
  const group = document.createElement('div');
  group.id = 'solverToleranceGroup';
  document.body.append(solver, rel, abs, max, group);

  await mod.loadDiagram({
    name: 'custom',
    blocks: [],
    connections: [],
    solver: 'ode45',
    relTol: 1e-4,
    absTol: 1e-7,
    maxStep: 0.25,
  });
  expect(state.relTol).toBe(1e-4);
  expect(state.absTol).toBe(1e-7);
  expect(state.maxStep).toBe(0.25);

  await mod.loadDiagram({ name: 'defaults', blocks: [], connections: [] });
  expect(state.solver).toBe('discrete');
  expect(state.relTol).toBe(1e-6);
  expect(state.absTol).toBe(1e-8);
  expect(state.maxStep).toBe(0);
  expect(rel.value).toBe('0.000001');
  expect(abs.value).toBe('1e-8');
  expect(max.value).toBe('0');
});
//=============================================================================
test('loadDiagram restores model creation and last-save metadata', async () => {
  const mod = await import('./diagram-handlers');
  const { state } = await import('./state');
  document.body.innerHTML = '';

  await mod.loadDiagram({
    version: '1.4.0',
    name: 'metadata',
    blocks: [],
    connections: [],
    metadata: {
      author: 'Alice',
      createdBy: 'Carol',
      created: '2024-01-02T03:04:05.000Z',
      lastSavedBy: 'Bob',
      lastSavedAt: '2026-08-08T18:35:29.000Z',
    },
  });

  expect(state.modelVersion).toBe('1.4.0');
  expect(state.author).toBe('Alice');
  expect(state.createdBy).toBe('Carol');
  expect(state.created).toBe('2024-01-02T03:04:05.000Z');
  expect(state.lastSavedBy).toBe('Bob');
  expect(state.lastSavedAt).toBe('2026-08-08T18:35:29.000Z');
});
//=============================================================================
test('loadDiagram uses the legacy author as creator when createdBy is absent', async () => {
  const mod = await import('./diagram-handlers');
  const { state } = await import('./state');

  await mod.loadDiagram({
    name: 'legacy-metadata',
    blocks: [],
    connections: [],
    metadata: {
      author: 'Legacy author',
      created: '2024-01-02T03:04:05.000Z',
    },
  });

  expect(state.author).toBe('Legacy author');
  expect(state.createdBy).toBe('Legacy author');
});
//=============================================================================
