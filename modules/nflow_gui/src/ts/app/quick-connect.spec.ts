//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { firstFreeInput, quickConnectTo, setRendererRef } from './quick-connect.js';
import { state } from './state.js';
import { _resetForTests, subscribeStatus } from './status-service.js';
//=============================================================================
beforeEach(() => {
  _resetForTests();
  state.blocks.clear();
  state.connections.length = 0;
  (state as any).selectedId = null;
  state.locked = false;
});
//=============================================================================
function addBlock(id: string, type: string, inputs: number, outputs: number): void {
  state.blocks.set(id as never, { id, type, inputs, outputs } as never);
}
//=============================================================================
test('firstFreeInput skips occupied inputs', () => {
  addBlock('t', 'sum', 3, 1);
  (state.connections as any[]).push({ from: 's', to: 't', toIndex: 0 });
  expect(firstFreeInput('t', 3)).toBe(1);
  (state.connections as any[]).push({ from: 's2', to: 't', toIndex: 1 });
  expect(firstFreeInput('t', 3)).toBe(2);
});
//=============================================================================
test('quickConnectTo wires selection to the first free input', () => {
  addBlock('src', 'gain', 1, 1);
  addBlock('dst', 'scope', 1, 0);
  (state as any).selectedId = 'src';
  const createConnection = vi.fn(() => ({}));
  const updateConnections = vi.fn();
  setRendererRef({ current: { createConnection, updateConnections } });

  expect(quickConnectTo('dst')).toBe(true);
  expect(createConnection).toHaveBeenCalledWith('src', 'dst', 0, 0);
  expect(updateConnections).toHaveBeenCalled();
});
//=============================================================================
test('quickConnectTo refuses source without output and full targets', () => {
  addBlock('sink1', 'scope', 1, 0);
  addBlock('sink2', 'scope', 1, 0);
  (state as any).selectedId = 'sink1';
  const createConnection = vi.fn(() => ({}));
  setRendererRef({ current: { createConnection } });
  const seen: any[] = [];
  const unsub = subscribeStatus((item) => item && seen.push(item));

  // Source has no outputs.
  expect(quickConnectTo('sink2')).toBe(false);
  // Target with all inputs occupied.
  addBlock('src', 'gain', 1, 1);
  (state as any).selectedId = 'src';
  (state.connections as any[]).push({ from: 'x', to: 'sink2', toIndex: 0 });
  expect(quickConnectTo('sink2')).toBe(false);
  unsub();
  expect(createConnection).not.toHaveBeenCalled();
  expect(seen.some((s) => s.level === 'warn')).toBe(true);
});
//=============================================================================
test('quickConnectTo ignores self-connections and missing selection', () => {
  addBlock('a', 'gain', 1, 1);
  const createConnection = vi.fn(() => ({}));
  setRendererRef({ current: { createConnection } });
  expect(quickConnectTo('a')).toBe(false);
  (state as any).selectedId = 'a';
  expect(quickConnectTo('a')).toBe(false);
  expect(createConnection).not.toHaveBeenCalled();
});
//=============================================================================
