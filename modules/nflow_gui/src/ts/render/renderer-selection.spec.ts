//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { makeSelectionHandlers } from './interaction/renderer-selection';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
const makeSpy = () => {
  const s: any = (...a: any[]) => s.calls.push(a);
  s.calls = [];
  return s;
};
//=============================================================================
test('selectBlock toggles class and calls onSelectBlock', () => {
  const onSelect = makeSpy();
  const block: any = { id: 'b1', group: document.createElement('div') };
  const S: any = {
    blocks: new Map([['b1', block]]),
    connections: [],
    selectedIds: new Set(),
    selectedConnections: new Set(),
    suppressNextCanvasClick: false,
  };
  const ctx: any = {
    state: S,
    onSelectBlock: onSelect,
    updatePortVisibility: makeSpy(),
    updateSelectionBox: makeSpy(),
    updateWireCornerHandles: makeSpy(),
  };
  const h = makeSelectionHandlers(ctx as any);
  h.selectBlock('b1');
  expect(S.selectedId).toBe('b1');
  expect(block.group.classList.contains('selected')).toBe(true);
  expect(onSelect.calls.length).toBe(1);
  expect(onSelect.calls[0][0]).toBe(block);
  h.selectBlock(null);
  expect(S.selectedId).toBeNull();
  expect(block.group.classList.contains('selected')).toBe(false);
});
//=============================================================================
test('selectConnection selects connection and reports types', () => {
  const onSelectConn = makeSpy();
  const from: any = { id: 'f', type: 'A', group: document.createElement('div') };
  const to: any = { id: 't', type: 'B', group: document.createElement('div') };
  const conn: any = {
    from: 'f',
    to: 't',
    toIndex: 0,
    path: document.createElement(FieldNames.kPath),
    points: [
      { x: 0, y: 0 },
      { x: 10, y: 0 },
    ],
  };
  const S: any = {
    blocks: new Map([
      ['f', from],
      ['t', to],
    ]),
    connections: [conn],
    selectedIds: new Set(),
    selectedConnections: new Set(),
  };
  const ctx: any = {
    state: S,
    onSelectConnection: onSelectConn,
    updatePortVisibility: makeSpy(),
    updateSelectionBox: makeSpy(),
    updateWireCornerHandles: makeSpy(),
  };
  const h = makeSelectionHandlers(ctx as any);
  h.selectConnection(conn);
  expect(S.selectedConnection).toBe(conn);
  expect(conn.path.classList.contains('selected')).toBe(true);
  expect(onSelectConn.calls.length).toBe(1);
  const reported = onSelectConn.calls[0][0];
  expect(reported.fromId).toBe('f');
  expect(reported.toId).toBe('t');
  expect(reported.fromType).toBe('A');
  expect(reported.toType).toBe('B');
  h.selectConnection(null);
  expect(S.selectedConnection).toBeNull();
  expect(onSelectConn.calls.length).toBe(2);
});
//=============================================================================
test('setMultiSelection behaviors for single/multi/empty', () => {
  const onSelect = makeSpy();
  const onSelectConn = makeSpy();
  const b1: any = { id: 'b1', group: document.createElement('div') };
  const c1: any = {
    from: 'b1',
    to: 'b2',
    toIndex: 0,
    path: document.createElement(FieldNames.kPath),
  };
  const S: any = {
    blocks: new Map([['b1', b1]]),
    connections: [c1],
    selectedIds: new Set(),
    selectedConnections: new Set(),
  };
  const ctx: any = {
    state: S,
    onSelectBlock: onSelect,
    onSelectConnection: onSelectConn,
    updatePortVisibility: makeSpy(),
    updateSelectionBox: makeSpy(),
    updateWireCornerHandles: makeSpy(),
  };
  const h = makeSelectionHandlers(ctx as any);
  h.setMultiSelection(new Set(['b1']), new Set());
  expect(onSelect.calls.length).toBe(1);
  h.setMultiSelection(new Set(), new Set([c1]));
  expect(onSelectConn.calls.length).toBe(1);
  h.setMultiSelection(new Set(['b1']), new Set([c1]));
  expect(onSelect.calls.length).toBe(2);
  h.setMultiSelection(new Set(), new Set());
  expect(onSelect.calls.length).toBe(3);
});
//=============================================================================
test('updateMarqueeRect sets attributes and returns rect', () => {
  const S: any = { blocks: new Map(), connections: [] };
  const marquee = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const ctx: any = { state: S, marqueeRect: marquee };
  const h = makeSelectionHandlers(ctx as any);
  const out = h.updateMarqueeRect({ x: 0, y: 0 }, { x: 10, y: 5 });
  expect(out.left).toBe(0);
  expect(out.right).toBe(10);
  expect(marquee.getAttribute('display')).toBe('block');
  expect(marquee.getAttribute('width')).toBe('10');
});
//=============================================================================
test('startMarqueeSelection and finishMarqueeSelection select blocks inside rect', () => {
  const spy = makeSpy();
  const b1: any = {
    id: 'b1',
    x: 0,
    y: 0,
    width: 10,
    height: 10,
    group: document.createElement('div'),
  };
  const S: any = {
    blocks: new Map([['b1', b1]]),
    connections: [],
    selectedIds: new Set(),
    selectedConnections: new Set(),
    suppressNextCanvasClick: false,
  };
  const marquee = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const ctx: any = {
    state: S,
    marqueeRect: marquee,
    clientToSvg: (x: number, y: number) => ({ x, y }),
    marqueeState: {},
  };
  ctx.onSelectBlock = spy;
  const h = makeSelectionHandlers(ctx as any);
  const down: any = { clientX: 0, clientY: 0, pointerId: 5, button: 0 };
  h.startMarqueeSelection(down as any);
  expect(ctx.marqueeState.active).toBe(true);
  const up: any = { clientX: 5, clientY: 5, pointerId: 5 };
  h.finishMarqueeSelection(up as any);
  expect(S.selectedIds.has('b1')).toBe(true);
  expect(spy.calls.length).toBeGreaterThan(0);
});
//=============================================================================
