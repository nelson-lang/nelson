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
import { vi } from 'vitest';
import { makeScopeCommentLayout } from './core/renderer-scope-layout';
//=============================================================================
const makeCtx = (ext: any = {}) => ({
  state: { connections: [] },
  updateBlockTransform: () => {},
  updateSelectionBox: () => {},
  ...ext,
});
//=============================================================================
describe('clampScopeSize / clampCommentSize', () => {
  test('clampScopeSize enforces minimum 160x120', () => {
    const { clampScopeSize } = makeScopeCommentLayout(makeCtx());
    const result = clampScopeSize(10, 20);
    expect(result.width).toBeGreaterThanOrEqual(160);
    expect(result.height).toBeGreaterThanOrEqual(120);
  });

  test('clampCommentSize enforces minimum 80x60', () => {
    const { clampCommentSize } = makeScopeCommentLayout(makeCtx());
    const result = clampCommentSize(1, 1);
    expect(result.width).toBeGreaterThanOrEqual(80);
    expect(result.height).toBeGreaterThanOrEqual(60);
  });

  test('clampScopeSize snaps to grid and keeps large values', () => {
    const { clampScopeSize } = makeScopeCommentLayout(makeCtx());
    const result = clampScopeSize(400, 300);
    expect(result.width).toBe(400);
    expect(result.height).toBe(300);
  });
});
//=============================================================================
describe('updatePortElement', () => {
  const ns = 'http://www.w3.org/2000/svg';

  test('updates hit circle and rect shape positions', () => {
    const { updatePortElement } = makeScopeCommentLayout(makeCtx());
    const g = document.createElementNS(ns, 'g');
    const hit = document.createElementNS(ns, 'circle');
    hit.classList.add('port-hit');
    const rect = document.createElementNS(ns, 'rect');
    rect.classList.add('port');
    g.appendChild(hit);
    g.appendChild(rect);
    updatePortElement({ el: g, x: 30, y: 40 });
    expect(hit.getAttribute('cx')).toBe('30');
    expect(hit.getAttribute('cy')).toBe('40');
    expect(rect.getAttribute('x')).toBe(String(30 - 6));
    expect(rect.getAttribute('y')).toBe(String(40 - 6));
  });

  test('updates hit circle and circle shape positions', () => {
    const { updatePortElement } = makeScopeCommentLayout(makeCtx());
    const g = document.createElementNS(ns, 'g');
    const hit = document.createElementNS(ns, 'circle');
    hit.classList.add('port-hit');
    const circ = document.createElementNS(ns, 'circle');
    circ.classList.add('port');
    g.appendChild(hit);
    g.appendChild(circ);
    updatePortElement({ el: g, x: 5, y: 6 });
    expect(circ.getAttribute('cx')).toBe('5');
    expect(circ.getAttribute('cy')).toBe('6');
  });

  test('no-ops when port.el is null', () => {
    const { updatePortElement } = makeScopeCommentLayout(makeCtx());
    expect(() => updatePortElement({ el: null, x: 0, y: 0 })).not.toThrow();
  });
});
//=============================================================================
describe('updateScopeLayout', () => {
  const ns = 'http://www.w3.org/2000/svg';

  test('clamps block size, updates bodyRect and calls transforms', () => {
    const updateBlockTransform = vi.fn();
    const updateSelectionBox = vi.fn();
    const { updateScopeLayout } = makeScopeCommentLayout(
      makeCtx({ updateBlockTransform, updateSelectionBox })
    );
    const bodyRect = document.createElementNS(ns, 'rect');
    const scopePlot = document.createElementNS(ns, 'svg');
    scopePlot.setAttribute('x', '0');
    const port = {
      el: (() => {
        const g = document.createElementNS(ns, 'g');
        const ph = document.createElementNS(ns, 'circle');
        ph.classList.add('port-hit');
        const ps = document.createElementNS(ns, 'circle');
        ps.classList.add('port');
        g.appendChild(ph);
        g.appendChild(ps);
        return g;
      })(),
      type: 'in',
      index: 0,
    };
    const block: any = {
      type: 'scope',
      width: 10,
      height: 10,
      bodyRect,
      scopePlot,
      scopeClipRect: document.createElementNS(ns, 'rect'),
      dragRect: document.createElementNS(ns, 'rect'),
      resizeHandle: document.createElementNS(ns, 'rect'),
      scopeInputHints: [document.createElementNS(ns, 'circle')],
      ports: [port],
    };
    updateScopeLayout(block);
    expect(block.width).toBeGreaterThanOrEqual(160);
    expect(bodyRect.getAttribute('width')).toBe(String(block.width));
    expect(updateBlockTransform).toHaveBeenCalledWith(block);
    expect(updateSelectionBox).toHaveBeenCalled();
  });

  test('no-ops for non-scope block type', () => {
    const updateBlockTransform = vi.fn();
    const { updateScopeLayout } = makeScopeCommentLayout(makeCtx({ updateBlockTransform }));
    const block: any = { type: FieldNames.kGain, width: 80, height: 60 };
    updateScopeLayout(block);
    expect(updateBlockTransform).not.toHaveBeenCalled();
  });
});
//=============================================================================
describe('updateCommentLayout', () => {
  const ns = 'http://www.w3.org/2000/svg';

  test('resizes comment, updates text content and calls transforms', () => {
    const updateBlockTransform = vi.fn();
    const updateSelectionBox = vi.fn();
    const { updateCommentLayout } = makeScopeCommentLayout(
      makeCtx({ updateBlockTransform, updateSelectionBox })
    );
    const group = document.createElementNS(ns, 'g');
    const body = document.createElementNS(ns, 'rect');
    body.classList.add('comment-body');
    const foreign = document.createElementNS(ns, 'foreignObject');
    foreign.classList.add('comment-foreign');
    const textEl = document.createElementNS(ns, 'text');
    textEl.classList.add('comment-text');
    const fallback = document.createElementNS(ns, 'text');
    fallback.classList.add('comment-text-fallback');
    group.appendChild(body);
    group.appendChild(foreign);
    group.appendChild(textEl);
    group.appendChild(fallback);
    const block: any = {
      type: 'comment',
      width: 10,
      height: 10,
      group,
      dragRect: document.createElementNS(ns, 'rect'),
      resizeHandle: document.createElementNS(ns, 'rect'),
      params: { CommentText: 'hello', ShowBorder: true },
    };
    updateCommentLayout(block);
    expect(block.width).toBeGreaterThanOrEqual(80);
    expect(textEl.textContent).toBe('hello');
    expect(fallback.textContent).toBe('hello');
    expect(body.style.display).toBe('');
    expect(updateBlockTransform).toHaveBeenCalledWith(block);
  });

  test('no-ops for non-comment block type', () => {
    const updateBlockTransform = vi.fn();
    const { updateCommentLayout } = makeScopeCommentLayout(makeCtx({ updateBlockTransform }));
    updateCommentLayout({ type: 'scope' } as any);
    expect(updateBlockTransform).not.toHaveBeenCalled();
  });
});
//=============================================================================
test('updatePortElement updates hit and rect/circle shapes', () => {
  const handlers = makeScopeCommentLayout({} as any);
  const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const hit = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
  hit.classList.add('port-hit');
  const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  rect.classList.add('port');
  g.appendChild(hit);
  g.appendChild(rect);
  const port = { el: g, x: 30, y: 40 };
  handlers.updatePortElement(port);
  expect(hit.getAttribute('cx')).toBe('30');
  expect(hit.getAttribute('cy')).toBe('40');
  expect(rect.getAttribute('x')).toBe(String(30 - 6));
  expect(rect.getAttribute('y')).toBe(String(40 - 6));

  const g2 = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const hit2 = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
  hit2.classList.add('port-hit');
  const circ = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
  circ.classList.add('port');
  g2.appendChild(hit2);
  g2.appendChild(circ);
  const port2 = { el: g2, x: 5, y: 6 };
  handlers.updatePortElement(port2);
  expect(circ.getAttribute('cx')).toBe('5');
  expect(circ.getAttribute('cy')).toBe('6');
});
//=============================================================================
test('updateScopeLayout sets sizes, plot and ports and calls transforms', () => {
  const updateBlockTransform = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const updateSelectionBox = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const ctx: any = { updateBlockTransform, updateSelectionBox };
  const handlers = makeScopeCommentLayout(ctx);

  const bodyRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const scopePlot = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  scopePlot.setAttribute('x', '0');
  const scopeClip = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const dragRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const resizeHandle = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const hint = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
  const inPortEl = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const pHit = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
  pHit.classList.add('port-hit');
  const pShape = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
  pShape.classList.add('port');
  inPortEl.appendChild(pHit);
  inPortEl.appendChild(pShape);

  const port = { el: inPortEl, type: 'in', index: 0 };
  const block: any = {
    type: 'scope',
    width: 10,
    height: 10,
    bodyRect,
    scopePlot,
    scopeClipRect: scopeClip,
    dragRect,
    resizeHandle,
    scopeInputHints: [hint],
    ports: [port],
  };
  handlers.updateScopeLayout(block);
  expect(block.width).toBeGreaterThanOrEqual(160);
  expect(block.height).toBeGreaterThanOrEqual(120);
  expect(bodyRect.getAttribute('width')).toBe(String(block.width));
  expect(scopePlot.getAttribute('width')).not.toBeNull();
  expect(pHit.getAttribute('cx')).toBeDefined();
  expect(updateBlockTransform.calls.length).toBeGreaterThan(0);
  expect(updateSelectionBox.calls.length).toBeGreaterThan(0);
});
//=============================================================================
test('updateCommentLayout resizes comment, updates text and dragRect', () => {
  const updateBlockTransform = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const updateSelectionBox = (() => {
    const s: any = (...a: any[]) => s.calls.push(a);
    s.calls = [];
    return s;
  })();
  const ctx: any = { updateBlockTransform, updateSelectionBox };
  const handlers = makeScopeCommentLayout(ctx);

  const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const body = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  body.classList.add('comment-body');
  const foreign = document.createElementNS('http://www.w3.org/2000/svg', 'foreignObject');
  foreign.classList.add('comment-foreign');
  const textEl = document.createElementNS('http://www.w3.org/2000/svg', 'text');
  textEl.classList.add('comment-text');
  const fallback = document.createElementNS('http://www.w3.org/2000/svg', 'text');
  fallback.classList.add('comment-text-fallback');
  group.appendChild(body);
  group.appendChild(foreign);
  group.appendChild(textEl);
  group.appendChild(fallback);
  const dragRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const resizeHandle = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
  const block: any = {
    type: 'comment',
    width: 10,
    height: 10,
    group,
    dragRect,
    resizeHandle,
    params: { CommentText: 'hello', ShowBorder: true },
  };
  handlers.updateCommentLayout(block);
  expect(block.width).toBeGreaterThanOrEqual(80);
  expect(block.height).toBeGreaterThanOrEqual(60);
  expect(textEl.textContent).toBe('hello');
  expect(fallback.textContent).toBe('hello');
  expect(body.style.display).toBe('');
  expect(dragRect.getAttribute('width')).toBeDefined();
  expect(updateBlockTransform.calls.length).toBeGreaterThan(0);
  expect(updateSelectionBox.calls.length).toBeGreaterThan(0);
});
//=============================================================================
