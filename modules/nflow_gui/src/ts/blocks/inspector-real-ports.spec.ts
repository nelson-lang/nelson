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
import { createInspector } from './inspector.js';
import { state } from '../app/state.js';
import { createRenderer } from '../render/create-renderer.js';
import { blockTemplates } from '../render/block/templates.js';
import { sumResizer, multResizer } from '../libraries/math/blocks.js';
import { busCreatorResizer } from '../libraries/utility/blocks.js';
import { buildDiagramAndScopeMap } from '../sim/resolve-params.js';

const previousTemplates = new Map<string, unknown>();
const ns = 'http://www.w3.org/2000/svg';
beforeEach(() => {
  document.body.innerHTML =
    '<div id="inspector"></div><svg><g id="blocks"></g><g id="wires"></g><g id="overlay"></g></svg>';
  state.blocks.clear();
  state.connections = [];
  state.selectedIds.clear();
  state.selectedConnections.clear();
  state.loadingDiagram = false;
  state.locked = false;
  state.autoRoute = false;
  for (const [type, resize] of Object.entries({
    sum: sumResizer,
    mult: multResizer,
    busCreator: busCreatorResizer,
  })) {
    previousTemplates.set(type, (blockTemplates as any)[type]);
    (blockTemplates as any)[type] = {
      width: 30,
      height: 60,
      inputs: [],
      outputs: [],
      defaultParams: {},
      resize,
      render: (block: any) => {
        const rect = document.createElementNS(ns, 'rect');
        rect.classList.add('nf-block');
        block.group.appendChild(rect);
      },
    };
  }
});
afterEach(() => {
  for (const [type, template] of previousTemplates) {
    if (template) (blockTemplates as any)[type] = template;
    else delete (blockTemplates as any)[type];
  }
  previousTemplates.clear();
  vi.restoreAllMocks();
});

function setup(type: string, input: number) {
  const renderer = createRenderer({
    svg: document.querySelector('svg'),
    blockLayer: document.getElementById('blocks'),
    wireLayer: document.getElementById('wires'),
    overlayLayer: document.getElementById('overlay'),
    state,
  });
  const block = renderer.createBlock(type, 0, 0, {
    id: 'block',
    params: { Inputs: type === 'sum' ? '+'.repeat(input) : input },
  });
  const body = document.getElementById('inspector')!;
  createInspector({ inspectorBody: body, renderer }).renderInspector(block);
  return { body, block, renderer };
}
function editInputs(body: HTMLElement, type: string, count: number) {
  const input = body.querySelector<HTMLInputElement>('input[data-edit="Inputs"]')!;
  input.value = type === 'sum' ? '+'.repeat(count) : String(count);
  input.dispatchEvent(new Event(type === 'sum' ? 'input' : 'change', { bubbles: true }));
}

test.each(['sum', 'mult', 'busCreator'])(
  '%s confirms removal before the real renderer mutates ports',
  (type) => {
    const { body, block } = setup(type, 3);
    const wire = {
      from: 'source',
      to: 'block',
      fromIndex: 0,
      toIndex: 2,
      path: document.createElementNS(ns, 'path'),
    };
    state.connections = [wire];
    const confirm = vi.spyOn(window, 'confirm').mockReturnValue(false);
    editInputs(body, type, 2);
    expect(confirm).toHaveBeenCalledOnce();
    expect(state.blocks.get('block')).toBe(block);
    expect(block.inputs).toBe(3);
    expect(block.ports.filter((p: any) => p.type === 'in')).toHaveLength(3);
    expect(state.connections).toEqual([wire]);
    expect(block.params.Inputs).toBe(type === 'sum' ? '+++' : 3);
  }
);

test.each(['sum', 'mult', 'busCreator'])(
  '%s updates both rendered and simulation arity after accepting removal',
  (type) => {
    const { body } = setup(type, 3);
    state.connections = [
      {
        from: 'source',
        to: 'block',
        fromIndex: 0,
        toIndex: 2,
        path: document.createElementNS(ns, 'path'),
      },
    ];
    vi.spyOn(window, 'confirm').mockReturnValue(true);
    editInputs(body, type, 2);
    const rebuilt = state.blocks.get('block') as any;
    expect(rebuilt.inputs).toBe(2);
    expect(rebuilt.ports.filter((p: any) => p.type === 'in')).toHaveLength(2);
    expect(state.connections).toEqual([]);
    expect((buildDiagramAndScopeMap(state).diagram as any).blocks[0].inputs).toBe(2);
  }
);

test.each(['sum', 'mult', 'busCreator'])(
  '%s adds inputs to the real block and simulation payload',
  (type) => {
    const { body } = setup(type, 2);
    editInputs(body, type, 4);
    const rebuilt = state.blocks.get('block') as any;
    expect(rebuilt.inputs).toBe(4);
    expect(rebuilt.ports.filter((p: any) => p.type === 'in')).toHaveLength(4);
    expect((buildDiagramAndScopeMap(state).diagram as any).blocks[0].inputs).toBe(4);
  }
);

test('typing successive sum signs preserves focus and rebuilds once per input', () => {
  const { body, renderer } = setup('sum', 2);
  const create = vi.spyOn(renderer, 'createBlock');
  let input = body.querySelector<HTMLInputElement>('input[data-edit="Inputs"]')!;
  input.focus();
  for (const count of [3, 4, 5]) {
    input.value = '+'.repeat(count);
    input.setSelectionRange(count, count);
    input.dispatchEvent(new Event('input', { bubbles: true }));
    const replacement = body.querySelector<HTMLInputElement>('input[data-edit="Inputs"]')!;
    expect(document.activeElement).toBe(replacement);
    expect(replacement.selectionStart).toBe(count);
    expect(replacement.selectionEnd).toBe(count);
    expect((state.blocks.get('block') as any).inputs).toBe(count);
    expect(create).toHaveBeenCalledTimes(count - 2);
    input = replacement;
  }
});

test('a rejected port removal restores focus and the caret selection', () => {
  const { body, renderer } = setup('sum', 3);
  state.connections = [
    { from: 'source', to: 'block', toIndex: 2, path: document.createElementNS(ns, 'path') },
  ];
  vi.spyOn(window, 'confirm').mockReturnValue(false);
  const create = vi.spyOn(renderer, 'createBlock');
  const input = body.querySelector<HTMLInputElement>('input[data-edit="Inputs"]')!;
  input.focus();
  input.value = '++';
  input.setSelectionRange(1, 2, 'backward');
  input.dispatchEvent(new Event('input', { bubbles: true }));
  const restored = body.querySelector<HTMLInputElement>('input[data-edit="Inputs"]')!;
  expect(document.activeElement).toBe(restored);
  expect(restored.value).toBe('+++');
  expect(restored.selectionStart).toBe(1);
  expect(restored.selectionEnd).toBe(2);
  expect(restored.selectionDirection).toBe('backward');
  expect(create).not.toHaveBeenCalled();
});

test('matrix product exposes its mode and keeps a single inverse input', () => {
  const { body, block } = setup('mult', 3);
  const mode = body.querySelector<HTMLSelectElement>('select[data-edit="Multiplication"]')!;
  expect(mode).not.toBeNull();
  mode.value = 'Matrix(*)';
  mode.dispatchEvent(new Event('change', { bubbles: true }));
  expect(block.params.Multiplication).toBe('Matrix(*)');
  const input = body.querySelector<HTMLInputElement>('input[data-edit="Inputs"]')!;
  input.value = '/';
  input.dispatchEvent(new Event('change', { bubbles: true }));
  const current: any = state.blocks.get('block');
  expect(current.params.Inputs).toBe('/');
  expect(current.ports.filter((p: any) => p.type === 'in')).toHaveLength(1);
});
