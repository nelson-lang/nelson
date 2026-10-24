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
let display: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  display = br.getDefinitions()['display']?.render as any;
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
  createSvgElement: (tag: string, attrs?: any, content?: string) => {
    const el = document.createElementNS(ns, tag) as any;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    if (content) el.textContent = content;
    return el;
  },
});
//=============================================================================
const makeBlock = (overrides: any = {}) => ({
  id: 'd1',
  group: document.createElementNS(ns, 'g') as any,
  width: 120,
  height: 60,
  params: {},
  ...overrides,
});
//=============================================================================
describe('display renderer', () => {
  test('appends nf-block rect to group', () => {
    const block = makeBlock();

    display(block as any, makeHelpers() as any);

    expect(block.group.querySelector('rect.nf-block')).not.toBeNull();
  });

  test('sets block.bodyRect', () => {
    const block = makeBlock();

    display(block as any, makeHelpers() as any);

    expect((block as any).bodyRect).toBeDefined();
  });

  test('appends display-title-bar rect', () => {
    const block = makeBlock();

    display(block as any, makeHelpers() as any);

    expect(block.group.querySelector('rect.display-title-bar')).not.toBeNull();
  });

  test('appends display-value-area rect', () => {
    const block = makeBlock();

    display(block as any, makeHelpers() as any);

    expect(block.group.querySelector('rect.display-value-area')).not.toBeNull();
  });

  test('sets block.displayValueEl', () => {
    const block = makeBlock();

    display(block as any, makeHelpers() as any);

    expect((block as any).displayValueEl).toBeDefined();
  });

  test('initial display value is "--"', () => {
    const block = makeBlock();

    display(block as any, makeHelpers() as any);

    expect((block as any).displayValueEl?.textContent).toBe('--');
  });

  test('display-value text has middle text-anchor', () => {
    const block = makeBlock();

    display(block as any, makeHelpers() as any);

    expect((block as any).displayValueEl?.getAttribute('text-anchor')).toBe('middle');
  });

  test('display-value text x is centered to block width', () => {
    const block = makeBlock({ width: 120 });

    display(block as any, makeHelpers() as any);

    expect((block as any).displayValueEl?.getAttribute('x')).toBe('60');
  });

  test('title bar width matches block width', () => {
    const block = makeBlock({ width: 120 });

    display(block as any, makeHelpers() as any);

    const titleBar = block.group.querySelector('rect.display-title-bar');
    expect(titleBar?.getAttribute('width')).toBe('120');
  });

  test('value area is inset from block edges', () => {
    const block = makeBlock({ width: 120, height: 60 });

    display(block as any, makeHelpers() as any);

    const valueArea = block.group.querySelector('rect.display-value-area');
    expect(Number(valueArea?.getAttribute('x'))).toBeGreaterThan(0);
    expect(Number(valueArea?.getAttribute('y'))).toBeGreaterThan(0);
    expect(Number(valueArea?.getAttribute('width'))).toBeLessThan(120);
  });
});
//=============================================================================
