//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { renderUtility } from './utility.js';
import { buildBusMemberTree } from './bus-member-tree.js';
import { state } from '../../app/state.js';
import { setSignalValues, clearSignalValues } from '../../app/signal-badges.js';
import * as FieldNames from '../../FieldNames.js';
import type { BusInfo } from '../../sim/parse-results.js';
//=============================================================================
const makeDeps = () => {
  const inspectorBody = document.createElement('div');
  document.body.appendChild(inspectorBody);
  return {
    inspectorBody,
    callUpdate: vi.fn(),
    callResize: vi.fn(),
    renderInspector: vi.fn(),
    signalDiagramChanged: vi.fn(),
  } as never as Parameters<typeof renderUtility>[1];
};
//=============================================================================
const nestedBus: BusInfo = {
  name: 'Pose',
  nonvirtual: true,
  members: [
    { name: 'pos', offset: 0, width: 2, type: 'double', dims: [2, 1] },
    { name: 'count', offset: 2, width: 1, type: 'int64' },
    {
      name: 'sub',
      offset: 3,
      width: 1,
      bus: {
        name: 'Inner',
        nonvirtual: false,
        members: [{ name: 'a', offset: 0, width: 1, type: 'double' }],
      },
    },
  ],
};
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
  clearSignalValues();
  (state.connections as unknown[]).length = 0;
  (state.blocks as Map<unknown, unknown>).clear();
});
//=============================================================================
describe('buildBusMemberTree', () => {
  it('builds dotted paths from a nested BusInfo', () => {
    const nodes = buildBusMemberTree(nestedBus, null);
    expect(nodes.map((n) => n.path)).toEqual(['pos', 'count', 'sub']);
    expect(nodes[0].meta).toBe('double [2]');
    expect(nodes[1].meta).toBe('int64');
    expect(nodes[2].isBus).toBe(true);
    expect(nodes[2].meta).toBe('bus Inner');
    expect(nodes[2].children.map((n) => n.path)).toEqual(['sub.a']);
  });

  it('falls back to flat member names', () => {
    const nodes = buildBusMemberTree(null, ['x', ' y ', '']);
    expect(nodes.map((n) => n.path)).toEqual(['x', 'y']);
    expect(nodes[0].children).toEqual([]);
  });

  it('returns empty without any source', () => {
    expect(buildBusMemberTree(null, null)).toEqual([]);
  });
});
//=============================================================================
describe('busSelector inspector member tree', () => {
  const wire = (selId: string, srcId: string) => {
    (state.connections as unknown[]).push({ from: srcId, to: selId, fromIndex: 0, toIndex: 0 });
  };

  it('renders the tree from the last-sim signalBus and toggles SelectedSignals', () => {
    const deps = makeDeps();
    const block = { id: 'sel1', type: 'busSelector', params: { SelectedSignals: ['count'] } };
    wire('sel1', 'bc1');
    setSignalValues({ bc1: [0] }, null, null, null, { bc1: [nestedBus as never] });

    expect(renderUtility(block as never, deps)).toBe(true);
    const body = (deps as never as { inspectorBody: HTMLElement }).inspectorBody;
    const boxes = body.querySelectorAll<HTMLInputElement>('input[data-bus-path]');
    expect(Array.from(boxes).map((b) => b.getAttribute('data-bus-path'))).toEqual([
      'pos',
      'count',
      'sub',
      'sub.a',
    ]);
    expect(boxes[1].checked).toBe(true);

    boxes[3].checked = true;
    boxes[3].dispatchEvent(new Event('change', { bubbles: true }));
    expect(block.params.SelectedSignals).toEqual(['count', 'sub.a']);
    const ta = body.querySelector<HTMLTextAreaElement>(
      `textarea[data-edit='${FieldNames.kSelectedSignals}']`
    );
    expect(ta?.value).toBe('count\nsub.a');
    expect(
      (deps as never as { callUpdate: ReturnType<typeof vi.fn> }).callUpdate
    ).toHaveBeenCalled();

    boxes[1].checked = false;
    boxes[1].dispatchEvent(new Event('change', { bubbles: true }));
    expect(block.params.SelectedSignals).toEqual(['sub.a']);
  });

  it('falls back to the upstream busCreator MemberNames before any sim', () => {
    const deps = makeDeps();
    const block = { id: 'sel1', type: 'busSelector', params: {} };
    wire('sel1', 'bc1');
    (state.blocks as Map<unknown, unknown>).set('bc1', {
      id: 'bc1',
      type: 'busCreator',
      params: { MemberNames: ['u', 'v'] },
    });

    renderUtility(block as never, deps);
    const body = (deps as never as { inspectorBody: HTMLElement }).inspectorBody;
    const boxes = body.querySelectorAll<HTMLInputElement>('input[data-bus-path]');
    expect(Array.from(boxes).map((b) => b.getAttribute('data-bus-path'))).toEqual(['u', 'v']);
  });

  it('shows a hint when the input is not connected', () => {
    const deps = makeDeps();
    const block = { id: 'sel1', type: 'busSelector', params: {} };
    renderUtility(block as never, deps);
    const body = (deps as never as { inspectorBody: HTMLElement }).inspectorBody;
    expect(body.querySelector('.bus-tree-hint')?.textContent).toMatch(/Connect a bus/);
  });

  it('refreshes the tree when a simulation completes', () => {
    const deps = makeDeps();
    const block = { id: 'sel1', type: 'busSelector', params: {} };
    wire('sel1', 'bc1');
    renderUtility(block as never, deps);
    const body = (deps as never as { inspectorBody: HTMLElement }).inspectorBody;
    expect(body.querySelector('.bus-tree-hint')?.textContent).toMatch(/Run a simulation/);

    setSignalValues({ bc1: [0] }, null, null, null, { bc1: [nestedBus as never] });
    document.dispatchEvent(new CustomEvent('nf-sim-time', { detail: { done: true } }));
    const boxes = body.querySelectorAll<HTMLInputElement>('input[data-bus-path]');
    expect(boxes.length).toBe(4);
  });
});
//=============================================================================
describe('multi-line textarea params (regression: coerce received the element)', () => {
  it('busSelector textarea input stores one trimmed path per line', () => {
    const deps = makeDeps();
    const block = { id: 'sel1', type: 'busSelector', params: {} };
    renderUtility(block as never, deps);
    const body = (deps as never as { inspectorBody: HTMLElement }).inspectorBody;
    const ta = body.querySelector<HTMLTextAreaElement>(
      `textarea[data-edit='${FieldNames.kSelectedSignals}']`
    );
    expect(ta).toBeTruthy();
    ta!.value = ' pos \nsub.a\n\n';
    ta!.dispatchEvent(new Event('input', { bubbles: true }));
    expect(block.params.SelectedSignals).toEqual(['pos', 'sub.a']);
  });

  it('busCreator textarea input stores MemberNames and displays one per line', () => {
    const deps = makeDeps();
    const block = {
      id: 'bc1',
      type: 'busCreator',
      params: { MemberNames: ['a', 'b'] },
    };
    renderUtility(block as never, deps);
    const body = (deps as never as { inspectorBody: HTMLElement }).inspectorBody;
    const ta = body.querySelector<HTMLTextAreaElement>(
      `textarea[data-edit='${FieldNames.kMemberNames}']`
    );
    expect(ta?.value).toBe('a\nb');
    ta!.value = 'x\ny\nz';
    ta!.dispatchEvent(new Event('input', { bubbles: true }));
    expect(block.params.MemberNames).toEqual(['x', 'y', 'z']);
  });
});
//=============================================================================
