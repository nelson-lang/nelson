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
import { subsystem } from './subsystem';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeHelpers = () => ({
  createSvgElement: (tag: string, attrs?: any, content?: string) => {
    const el = document.createElementNS(ns, tag) as any;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    if (content) el.textContent = content;
    Object.defineProperty(el, 'dataset', { value: {}, writable: true, configurable: true });
    return el;
  },
  renderTeXMath: vi.fn(),
  formatLabelTeX: (s: string) => s,
  svgRect: (x: number, y: number, w: number, h: number, cls?: string) => {
    const r = document.createElementNS(ns, 'rect');
    if (cls) r.setAttribute('class', cls);
    return r;
  },
  svgText: () => document.createElementNS(ns, 'text'),
  renderSourcePlot: () => {},
  renderCenteredAxesPlot: () => {},
});
//=============================================================================
const makeBlock = (overrides: any = {}) => ({
  id: 'sub1',
  group: document.createElementNS(ns, 'g'),
  width: 120,
  height: 80,
  params: { name: 'MySubsystem' },
  dynamicInputs: [{ y: 20 }],
  dynamicOutputs: [{ y: 20 }],
  ...overrides,
});
//=============================================================================
describe('subsystem renderer', () => {
  test('appends nf-block rect to group', () => {
    const block = makeBlock();

    subsystem(block as any, makeHelpers() as any);

    expect(block.group.querySelector('rect.nf-block')).not.toBeNull();
  });

  test('creates subsystem-port-labels group', () => {
    const block = makeBlock();

    subsystem(block as any, makeHelpers() as any);

    expect(block.group.querySelector('g.subsystem-port-labels')).not.toBeNull();
  });

  test('title text uses params.name', () => {
    const block = makeBlock({ params: { name: 'CtrlSys' } });

    subsystem(block as any, makeHelpers() as any);

    const textEl = block.group.querySelector('text.block-text');
    expect(textEl).not.toBeNull();
    expect(textEl!.textContent).toBe('CtrlSys');
  });

  test('title falls back to Subsystem when params.name is absent', () => {
    const block = makeBlock({ params: {} });

    subsystem(block as any, makeHelpers() as any);

    const textEl = block.group.querySelector('text.block-text');
    expect(textEl!.textContent).toBe('Subsystem');
  });

  test('calls renderTeXMath for each input port', () => {
    const helpers = makeHelpers();
    const block = makeBlock({ dynamicInputs: [{ y: 20 }, { y: 40 }], dynamicOutputs: [] });

    subsystem(block as any, helpers as any);

    expect(helpers.renderTeXMath).toHaveBeenCalledTimes(2);
  });

  test('renders masked-indicator when params.masked is true', () => {
    const block = makeBlock({ params: { name: 'MaskedSys', masked: true } });
    subsystem(block as any, makeHelpers() as any);
    const maskEl = block.group.querySelector('g.masked-indicator');
    expect(maskEl).not.toBeNull();
    expect(maskEl!.getAttribute('data-visible')).toBe('true');
  });

  test('masked-indicator present but hidden when params.masked is false', () => {
    const block = makeBlock({ params: { name: 'UnmaskedSys', masked: false } });
    subsystem(block as any, makeHelpers() as any);
    const maskEl = block.group.querySelector('g.masked-indicator');
    expect(maskEl).not.toBeNull();
    expect(maskEl!.getAttribute('data-visible')).toBe('false');
  });
});
//=============================================================================
