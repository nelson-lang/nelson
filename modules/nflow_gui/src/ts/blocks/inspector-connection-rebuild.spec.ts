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
import { createInspector } from './inspector.js';
import { state } from '../app/state.js';

test('changing mux inputs preserves the labels and routing of surviving wires', () => {
  const body = document.createElement('div');
  document.body.appendChild(body);
  const ports = (count: number) => [
    ...Array.from({ length: count }, (_, index) => ({ type: 'in', index })),
    { type: 'out', index: 0 },
  ];
  const block: any = { id: 'mux', type: 'mux', x: 0, y: 0, params: { Inputs: 2 }, ports: ports(2) };
  const originalConnections = state.connections;
  const originalBlocks = state.blocks;
  try {
    state.blocks = new Map([['mux', block]]);
    state.connections = [
      {
        from: 'source',
        to: 'mux',
        fromIndex: 0,
        toIndex: 0,
        label: 'velocity',
        points: [
          { x: 10, y: 20, pin: true },
          { x: 30, y: 20 },
        ],
      },
    ] as any;
    const renderer: any = {
      updateBlockLabel: () => {},
      deleteBlock: () => {
        state.connections = [];
        state.blocks.delete('mux');
      },
      createBlock: (_type: string, _x: number, _y: number, options: any) => {
        state.blocks.set('mux', {
          ...block,
          params: options.params,
          ports: ports(options.params.Inputs),
        });
      },
      createConnection: (from: string, to: string, toIndex: number, fromIndex: number) => {
        const connection = { from, to, toIndex, fromIndex, points: [] };
        (state.connections as any[]).push(connection);
        return connection;
      },
    };
    const inspector = createInspector({ inspectorBody: body, renderer });
    inspector.renderInspector(block);
    const input = body.querySelector<HTMLInputElement>("input[data-edit='Inputs']")!;
    input.value = '3';
    input.dispatchEvent(new Event('input', { bubbles: true }));
    expect(state.connections).toHaveLength(1);
    expect((state.connections as any[])[0]).toMatchObject({
      label: 'velocity',
      points: [
        { x: 10, y: 20, pin: true },
        { x: 30, y: 20 },
      ],
    });
  } finally {
    state.connections = originalConnections;
    state.blocks = originalBlocks;
    body.remove();
  }
});

test('rejecting removal of a connected input restores its parameter and wire', () => {
  const body = document.createElement('div');
  document.body.appendChild(body);
  const block: any = {
    id: 'mux',
    type: 'mux',
    x: 0,
    y: 0,
    params: { Inputs: 3 },
    ports: [
      { type: 'in', index: 0 },
      { type: 'in', index: 1 },
      { type: 'in', index: 2 },
      { type: 'out', index: 0 },
    ],
  };
  const originalConnections = state.connections;
  const originalBlocks = state.blocks;
  const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false);
  try {
    state.blocks = new Map([['mux', block]]);
    const connection = { from: 'source', to: 'mux', fromIndex: 0, toIndex: 2, label: 'speed' };
    state.connections = [connection] as any;
    const renderer: any = { updateBlockLabel: vi.fn(), deleteBlock: vi.fn(), createBlock: vi.fn() };
    const inspector = createInspector({ inspectorBody: body, renderer });
    inspector.renderInspector(block);
    const input = body.querySelector<HTMLInputElement>("input[data-edit='Inputs']")!;
    input.value = '2';
    input.dispatchEvent(new Event('input', { bubbles: true }));
    expect(confirm).toHaveBeenCalledOnce();
    expect(renderer.deleteBlock).not.toHaveBeenCalled();
    expect(block.params.Inputs).toBe(3);
    expect(state.connections).toEqual([connection]);
  } finally {
    confirm.mockRestore();
    state.connections = originalConnections;
    state.blocks = originalBlocks;
    body.remove();
  }
});

test('reordering selected bus members keeps wires attached to their members', () => {
  const body = document.createElement('div');
  document.body.appendChild(body);
  const ports = [
    { type: 'in', index: 0 },
    { type: 'out', index: 0 },
    { type: 'out', index: 1 },
  ];
  const block: any = {
    id: 'sel',
    type: 'busSelector',
    x: 0,
    y: 0,
    params: { SelectedSignals: ['a', 'b'] },
    ports,
  };
  const originalConnections = state.connections;
  const originalBlocks = state.blocks;
  try {
    state.blocks = new Map([['sel', block]]);
    state.connections = [
      { from: 'sel', to: 'a_sink', fromIndex: 0, toIndex: 0 },
      { from: 'sel', to: 'b_sink', fromIndex: 1, toIndex: 0 },
    ] as any;
    const renderer: any = {
      updateBlockLabel: vi.fn(),
      deleteBlock: () => {
        state.connections = [];
        state.blocks.delete('sel');
      },
      createBlock: (_t: string, _x: number, _y: number, options: any) => {
        state.blocks.set('sel', { ...block, params: options.params, ports });
      },
      createConnection: (from: string, to: string, toIndex: number, fromIndex: number) => {
        const conn = { from, to, fromIndex, toIndex };
        (state.connections as any[]).push(conn);
        return conn;
      },
    };
    const inspector = createInspector({ inspectorBody: body, renderer });
    inspector.renderInspector(block);
    const input = body.querySelector<HTMLTextAreaElement>("textarea[data-edit='SelectedSignals']")!;
    input.value = 'b\na';
    input.dispatchEvent(new Event('change', { bubbles: true }));
    expect((state.connections as any[]).find((c) => c.to === 'a_sink').fromIndex).toBe(1);
    expect((state.connections as any[]).find((c) => c.to === 'b_sink').fromIndex).toBe(0);
  } finally {
    state.connections = originalConnections;
    state.blocks = originalBlocks;
    body.remove();
  }
});
