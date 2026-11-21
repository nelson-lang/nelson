//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, expect, test } from 'vitest';
import { buildDiagramAndScopeMap } from '../sim/resolve-params.js';
import { state } from './state.js';
import { loadDiagram, setRendererRef } from './diagram-handlers.js';
import { serializeDiagram, serializeDiagramForSave, serializeNBlock } from './file-operations.js';
import { initHistory, undo, redo } from './history.js';
import { newDiagram } from './menu-handlers.js';
import { openSubsystemFromBlock, closeSubsystemView } from './subsystem-handlers.js';

const busTypes = [
  { name: 'EnvironmentBus', members: [{ name: 'temperature', type: 'double', dims: [1] }] },
  { name: 'VehicleBus', members: [{ name: 'environment', bus: 'EnvironmentBus' }] },
];
const diagram = (types?: unknown[]) => ({
  name: 'typed',
  blocks: [],
  connections: [],
  autoRoute: false,
  busTypes: types,
});

beforeEach(() => {
  document.body.innerHTML = '';
  state.blocks.clear();
  state.connections = [];
  state.busTypes = [];
  state.subsystemStack = [];
  setRendererRef({ current: { clearWorkspace: () => state.blocks.clear() } });
});

test('load, simulation serialization and save retain nested types without sharing snapshots', async () => {
  const input = structuredClone(busTypes);
  await loadDiagram(diagram(input));
  expect(state.busTypes).toEqual(busTypes);
  expect(state.busTypes).not.toBe(input);
  const run = buildDiagramAndScopeMap(state).diagram as Record<string, unknown>;
  const saved = serializeDiagramForSave(state);
  expect(run.busTypes).toEqual(busTypes);
  expect(saved.busTypes).toEqual(busTypes);
  expect(serializeNBlock(saved, [], []).busTypes).toEqual(busTypes);
  (state.busTypes[0] as { name: string }).name = 'Changed';
  expect(run.busTypes).toEqual(busTypes);
  expect(saved.busTypes).toEqual(busTypes);
  expect(input).toEqual(busTypes);
  await loadDiagram(saved);
  expect(state.busTypes).toEqual(busTypes);
});

test('opening an untyped model or creating a new one clears previous declarations', async () => {
  await loadDiagram(diagram(busTypes));
  await loadDiagram(diagram());
  expect(state.busTypes).toEqual([]);
  expect(serializeDiagram(state)).not.toHaveProperty('busTypes');
  expect(buildDiagramAndScopeMap(state).diagram).not.toHaveProperty('busTypes');
  await loadDiagram(diagram(busTypes));
  newDiagram();
  expect(state.busTypes).toEqual([]);
  expect(serializeDiagram(state)).not.toHaveProperty('busTypes');
  expect(buildDiagramAndScopeMap(state).diagram).not.toHaveProperty('busTypes');
});

test('undo and redo restore independent bus type snapshots', async () => {
  await loadDiagram(diagram(busTypes));
  initHistory({ renderer: null });
  (state.busTypes[0] as { name: string }).name = 'Changed';
  window.dispatchEvent(new Event('diagramCommitted'));
  expect(await undo()).toBe(true);
  expect(state.busTypes).toEqual(busTypes);
  expect(await redo()).toBe(true);
  expect((state.busTypes[0] as { name: string }).name).toBe('Changed');
});

test('subsystem navigation inherits model types and restores the parent declarations', async () => {
  await loadDiagram(diagram(busTypes));
  const host = {
    id: 1,
    type: 'subsystem',
    params: {
      subsystem: {
        name: 'inner',
        blocks: [],
        connections: [],
      },
    },
  };
  state.blocks.set(1, host);
  await openSubsystemFromBlock(host);
  expect(state.busTypes).toEqual(busTypes);
  expect(serializeDiagram(state).busTypes).toEqual(busTypes);
  await closeSubsystemView();
  expect(state.busTypes).toEqual(busTypes);
  await loadDiagram(diagram());
  expect(state.busTypes).toEqual([]);
});
