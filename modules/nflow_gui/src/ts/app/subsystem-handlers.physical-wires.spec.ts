//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// An acausal sub-circuit stored inside a subsystem must show (and keep) its
// physical wires: openSubsystemFromBlock forwards the inner diagram's
// `physicalWires` to loadDiagram (which reconstructs the drawable pin-to-pin
// connections), and buildSubsystemSpec keeps them so the nets are not dropped on
// close. Without them an acausal subsystem opens with its blocks but no wires,
// which gives a floating (singular) island at run time.
//=============================================================================
import { vi, beforeEach, test, expect } from 'vitest';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
// Spy on loadDiagram so the forwarding contract can be checked without the full
// block registry (loadDiagram aborts on unregistered types in the unit env).
const { loadDiagramMock } = vi.hoisted(() => ({
  loadDiagramMock: vi.fn(async () => {}),
}));
//=============================================================================
vi.mock('./diagram-handlers.js', async (importActual) => {
  const actual = await importActual<typeof import('./diagram-handlers.js')>();
  return { ...actual, loadDiagram: loadDiagramMock };
});
//=============================================================================
import { openSubsystemFromBlock } from './subsystem-handlers';
import { buildSubsystemSpec } from './event-context';
import { setStatusEl } from './diagram-handlers';
import { state } from './state';
//=============================================================================
beforeEach(() => {
  state.blocks.clear();
  state.connections = [];
  state.subsystemStack = [];
  state.diagramName = 'root';
  document.body.innerHTML = '';
  const status = document.createElement('div');
  status.setAttribute('id', 'status');
  document.body.appendChild(status);
  setStatusEl(status);
  loadDiagramMock.mockClear();
});
//=============================================================================
test('openSubsystemFromBlock forwards the inner physicalWires to loadDiagram', async () => {
  const host = {
    id: 900,
    type: FieldNames.kSubsystem,
    params: {
      name: 'RC',
      subsystem: {
        name: 'RC',
        blocks: [
          { id: 'R1', type: 'Resistor', params: { R: 1 } },
          { id: 'C1', type: 'Capacitor', params: { C: 1 } },
          { id: 'yo', type: FieldNames.kLabelSink, params: { isExternalPort: true, name: 'vc' } },
        ],
        connections: [],
        physicalWires: [{ pins: ['R1.n', 'C1.p'] }, { pins: ['R1.p', 'C1.n'] }],
        externalOutputs: [{ id: 'yo', name: 'vc' }],
      },
    },
  };
  await openSubsystemFromBlock(host as any);
  expect(loadDiagramMock).toHaveBeenCalled();
  const arg = loadDiagramMock.mock.calls[0][0] as Record<string, unknown>;
  expect(Array.isArray(arg.physicalWires)).toBe(true);
  expect((arg.physicalWires as unknown[]).length).toBe(2);
});
//=============================================================================
test('buildSubsystemSpec preserves inner physical wires', () => {
  const data = {
    name: 'RC',
    blocks: [
      { id: 'R1', type: 'Resistor', params: {} },
      { id: 'yo', type: FieldNames.kLabelSink, params: { isExternalPort: true, name: 'vc' } },
    ],
    connections: [],
    physicalWires: [{ pins: ['R1.p', 'R1.n'] }],
  };
  const spec = buildSubsystemSpec(data, 'RC') as Record<string, unknown>;
  expect(Array.isArray(spec.physicalWires)).toBe(true);
  expect((spec.physicalWires as unknown[]).length).toBe(1);
});
//=============================================================================
export {};
//=============================================================================
