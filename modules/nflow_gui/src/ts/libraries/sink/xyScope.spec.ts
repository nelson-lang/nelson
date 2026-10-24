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
let xyScope: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  xyScope = br.getDefinitions()['xyScope']?.render as any;
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
  svgText: (x: number, y: number, text: string) => {
    const t = document.createElementNS(ns, 'text');
    t.textContent = text;
    return t;
  },
  createSvgElement: (tag: string, attrs?: any) => {
    const el = document.createElementNS(ns, tag) as any;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    Object.defineProperty(el, 'dataset', { value: {}, writable: true, configurable: true });
    return el;
  },
  renderSourcePlot: () => {},
  renderCenteredAxesPlot: () => {},
  renderTeXMath: () => {},
});
//=============================================================================
const makeBlock = (overrides: any = {}) => ({
  id: 'xy1',
  group: document.createElementNS(ns, 'g'),
  width: 200,
  height: 160,
  params: {},
  ...overrides,
});
//=============================================================================
describe('xyScope renderer', () => {
  test('appends nf-block rect to group', () => {
    const block = makeBlock();

    xyScope(block as any, makeHelpers() as any);

    expect(block.group.querySelector('rect.nf-block')).not.toBeNull();
  });

  test('sets block.bodyRect', () => {
    const block = makeBlock();

    xyScope(block as any, makeHelpers() as any);

    expect((block as any).bodyRect).toBeDefined();
  });

  test('creates scopePaths array with 1 path (x-y plot)', () => {
    const block = makeBlock();

    xyScope(block as any, makeHelpers() as any);

    expect((block as any).scopePaths).toHaveLength(1);
  });

  test('sets block.scopeAxes with 9 x-ticks and 9 y-ticks', () => {
    const block = makeBlock();

    xyScope(block as any, makeHelpers() as any);

    const axes = (block as any).scopeAxes;
    expect(axes.xTicks).toHaveLength(9);
    expect(axes.yTicks).toHaveLength(9);
  });

  test('scopeInputHints is empty array', () => {
    const block = makeBlock();

    xyScope(block as any, makeHelpers() as any);

    expect((block as any).scopeInputHints).toHaveLength(0);
  });

  test('creates clipPath def with block id', () => {
    const block = makeBlock({ id: 'xyTest' });

    xyScope(block as any, makeHelpers() as any);

    expect(block.group.querySelector('clipPath#scope-clip-xyTest')).not.toBeNull();
  });
});
//=============================================================================
