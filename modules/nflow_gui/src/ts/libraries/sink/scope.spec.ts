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
import fs from 'fs';
import path from 'path';
//=============================================================================
let scope: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  scope = br.getDefinitions()['scope']?.render as any;
});
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeHelpers = () => ({
  svgRect: (x: number, y: number, w: number, h: number, cls?: string) => {
    const r = document.createElementNS(ns, 'rect');
    r.setAttribute('x', String(x));
    r.setAttribute('y', String(y));
    r.setAttribute('width', String(w));
    r.setAttribute('height', String(h));
    if (cls) r.setAttribute('class', cls);
    return r;
  },
  svgText: (x: number, y: number, text: string, cls?: string) => {
    const t = document.createElementNS(ns, 'text');
    t.setAttribute('x', String(x));
    t.setAttribute('y', String(y));
    if (cls) t.setAttribute('class', cls);
    t.textContent = text;
    return t;
  },
  createSvgElement: (tag: string, attrs?: any, content?: string) => {
    const el = document.createElementNS(ns, tag) as any;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    if (content) el.textContent = content;
    Object.defineProperty(el, 'dataset', { value: {}, writable: true, configurable: true });
    return el;
  },
  renderSourcePlot: () => {},
  renderCenteredAxesPlot: () => {},
  renderTeXMath: () => {},
});
//=============================================================================
const makeBlock = (overrides: any = {}) => ({
  id: 'b1',
  group: document.createElementNS(ns, 'g'),
  width: 200,
  height: 160,
  params: {},
  ...overrides,
});
//=============================================================================
describe('scope renderer', () => {
  test('appends nf-block rect to group', () => {
    const block = makeBlock();

    scope(block as any, makeHelpers() as any);

    expect(block.group.querySelector('rect.nf-block')).not.toBeNull();
  });

  test('sets block.bodyRect to the body rect', () => {
    const block = makeBlock();

    scope(block as any, makeHelpers() as any);

    expect((block as any).bodyRect).toBeDefined();
  });

  test('creates scopePaths array with 3 paths', () => {
    const block = makeBlock();

    scope(block as any, makeHelpers() as any);

    expect((block as any).scopePaths).toHaveLength(3);
  });

  test('sets block.scopePlot', () => {
    const block = makeBlock();

    scope(block as any, makeHelpers() as any);

    expect((block as any).scopePlot).toBeDefined();
  });

  test('sets block.scopeAxes with xAxis, yAxis, xTicks, yTicks', () => {
    const block = makeBlock();

    scope(block as any, makeHelpers() as any);

    const axes = (block as any).scopeAxes;
    expect(axes.xAxis).toBeDefined();
    expect(axes.yAxis).toBeDefined();
    expect(axes.xTicks).toHaveLength(9);
    expect(axes.yTicks).toHaveLength(9);
  });

  test('creates 3 scopeInputHints circles', () => {
    const block = makeBlock();

    scope(block as any, makeHelpers() as any);

    expect((block as any).scopeInputHints).toHaveLength(3);
  });

  test('creates a clipPath def with block id', () => {
    const block = makeBlock({ id: 'myScope' });

    scope(block as any, makeHelpers() as any);

    expect(block.group.querySelector(`clipPath#scope-clip-myScope`)).not.toBeNull();
  });
});
//=============================================================================
