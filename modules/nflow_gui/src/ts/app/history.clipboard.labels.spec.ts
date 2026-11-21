//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { afterEach, beforeEach, expect, test, vi } from 'vitest';
import {
  duplicateSelection,
  initHistory,
  copySelection,
  cutSelection,
  pasteSelection,
} from './history.js';
import { state } from './state.js';
import { buildDiagramAndScopeMap } from '../sim/resolve-params.js';

let clipboard = '';
beforeEach(() => {
  state.blocks.clear();
  state.connections = [];
  state.selectedIds = new Set(['a', 'b', 'bus', 'sel']);
  state.selectedId = null;
  for (const [id, type, params] of [
    ['a', 'constant', { Value: 4 }],
    ['b', 'constant', { Value: 7 }],
    ['bus', 'busCreator', { MemberNames: [] }],
    ['sel', 'busSelector', { SelectedSignals: ['altitude', 'speed'] }],
  ] as const)
    state.blocks.set(id, { id, type, params });
  state.connections = [
    { from: 'a', to: 'bus', toIndex: 0, label: 'altitude', labelOffset: { x: 10, y: -5 } },
    { from: 'b', to: 'bus', toIndex: 1, label: 'speed' },
    { from: 'bus', to: 'sel', toIndex: 0 },
  ];
  clipboard = '';
  vi.stubGlobal('navigator', {
    clipboard: {
      writeText: async (text: string) => {
        clipboard = text;
      },
      readText: async () => clipboard,
    },
  });
  vi.stubGlobal('blockTemplates', {});
  Object.defineProperty(window, 'blockTemplates', {
    configurable: true,
    value: { constant: {}, busCreator: {}, busSelector: {} },
  });
  let nextId = 0;
  const renderer = {
    createBlock: (type: string, x: number, y: number, options: any) => {
      const block = { id: `new${nextId++}`, type, x, y, params: options.params };
      state.blocks.set(block.id, block);
      return block;
    },
    createConnection: (from: string, to: string, toIndex: number, fromIndex: number) => {
      const wire = { from, to, toIndex, fromIndex };
      state.connections.push(wire);
      return wire;
    },
    deleteBlock: (id: string) => {
      state.blocks.delete(id);
      state.connections = (state.connections as any[]).filter((c) => c.from !== id && c.to !== id);
    },
  };
  initHistory({ renderer: { current: renderer } });
});
afterEach(() => vi.unstubAllGlobals());

test.each(['copy', 'cut'])(
  '%s/paste preserves names consumed by bus member inference',
  async (operation) => {
    expect(await (operation === 'copy' ? copySelection() : cutSelection())).toBe(true);
    const payload = JSON.parse(clipboard);
    expect(payload.connections[0]).toMatchObject({
      label: 'altitude',
      labelOffset: { x: 10, y: -5 },
    });
    expect(await pasteSelection()).toBe(true);
    const pasted = (state.connections as any[]).filter((c) => c.to === 'new2');
    expect(pasted.map((c) => c.label)).toEqual(['altitude', 'speed']);
    expect(pasted[0].labelOffset).toEqual({ x: 10, y: -5 });
    const run = buildDiagramAndScopeMap(state).diagram as any;
    expect(run.connections.filter((c: any) => c.to === 'new2').map((c: any) => c.label)).toEqual([
      'altitude',
      'speed',
    ]);
    expect((state.blocks.get('new3') as any).params.SelectedSignals).toEqual(['altitude', 'speed']);
  }
);

test('paste accepts an older clipboard payload without labels', async () => {
  await copySelection();
  const payload = JSON.parse(clipboard);
  payload.connections.forEach((c: any) => {
    delete c.label;
    delete c.labelOffset;
  });
  clipboard = JSON.stringify(payload);
  expect(await pasteSelection()).toBe(true);
  const pasted = (state.connections as any[]).filter((c) => c.to === 'new2');
  expect(pasted).toHaveLength(2);
  expect(pasted[0]).not.toHaveProperty('label');
  expect(pasted[0]).not.toHaveProperty('labelOffset');
});

test('duplicate preserves names, offsets and bus selector paths', () => {
  expect(duplicateSelection()).toBe(true);
  const wires = (state.connections as any[]).filter((c) => c.to === 'new2');
  expect(wires.map((c) => c.label)).toEqual(['altitude', 'speed']);
  expect(wires[0].labelOffset).toEqual({ x: 10, y: -5 });
  expect(wires[0].labelOffset).not.toBe((state.connections[0] as any).labelOffset);
  expect((state.blocks.get('new3') as any).params.SelectedSignals).toEqual(['altitude', 'speed']);
  const run = buildDiagramAndScopeMap(state).diagram as any;
  expect(run.connections.filter((c: any) => c.to === 'new2').map((c: any) => c.label)).toEqual([
    'altitude',
    'speed',
  ]);
});
