//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { openSubsystemFromBlock } from './subsystem-handlers';
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
test('openSubsystemFromBlock creates and opens empty subsystem when missing', async () => {
  const hostBlock: any = {
    id: 901,
    type: FieldNames.kSubsystem,
    params: {
      name: 'HostWithoutSubsys',
      subsystem: null,
    },
  };

  await openSubsystemFromBlock(hostBlock as any);

  expect(state.subsystemStack.length).toBeGreaterThan(0);

  expect(hostBlock.params).toBeDefined();
  expect(hostBlock.params.subsystem).toBeDefined();
  expect(Array.isArray(hostBlock.params.subsystem.blocks)).toBe(true);

  const statusEl = document.getElementById('status');
  expect(statusEl?.textContent).toMatch(/Opened|empty subsystem/);
});
//=============================================================================
export {};
//=============================================================================
