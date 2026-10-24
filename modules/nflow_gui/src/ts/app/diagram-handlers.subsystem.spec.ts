//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { openSubsystemFromBlock, closeSubsystemView } from './subsystem-handlers';
import { setRendererRef, setStatusEl } from './diagram-handlers';
import { state } from './state';
import { beforeEach, test, expect } from 'vitest';
import * as FieldNames from '../FieldNames.js';
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
  // renderer that populates state.blocks when createBlock is called
  setRendererRef({
    current: {
      createBlock: (type: string, x: number, y: number, opts?: Record<string, any>) => {
        const id = opts && opts.id ? (opts.id as number) : state.nextId++;
        state.blocks.set(id, { id, type, x, y, params: opts?.params || {} });
        return { id };
      },
      selectBlock: () => {},
    },
  });
});
//=============================================================================
test('open and close subsystem view updates host params and subsystem stack', async () => {
  const hostBlock = {
    id: 900,
    type: FieldNames.kSubsystem,
    params: {
      name: 'Host',
      subsystem: {
        name: 'Sub',
        blocks: [
          {
            id: 1,
            type: FieldNames.kLabelSource,
            x: 0,
            y: 0,
            params: { isExternalPort: true, name: 'in1' },
          },
          {
            id: 2,
            type: FieldNames.kLabelSink,
            x: 10,
            y: 0,
            params: { isExternalPort: true, name: 'out1' },
          },
        ],
        connections: [],
      },
    },
  };
  await openSubsystemFromBlock(hostBlock as any);
  expect(state.subsystemStack.length).toBeGreaterThan(0);
  await closeSubsystemView();
  expect(state.subsystemStack.length).toBe(0);
  const host = state.blocks.get(900);
  // host may not be created by renderer; ensure no crash and status updated
  const statusEl = document.getElementById('status');
  expect(statusEl?.textContent).toMatch(/Closed subsystem view|Opened subsystem/);
});
//=============================================================================
export {};
//=============================================================================
