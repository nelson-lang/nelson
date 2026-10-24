//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { muxRenderer, muxResizer } from './mux';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeHelpers = () => ({
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  createSvgElement: (tag: string, attrs?: any) => {
    const el = document.createElementNS(ns, tag) as Element;
    if (attrs) Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v)));
    return el;
  },
});
//=============================================================================
describe('muxResizer', () => {
  test('mutates blockDef with default 2 inputs', () => {
    const probe: any = { params: { Inputs: 2 } };
    muxResizer(probe);
    expect(probe.width).toBe(44);
    expect(probe.height).toBe(40);
    expect(Array.isArray(probe.dynamicInputs)).toBe(true);
    expect(probe.dynamicInputs).toHaveLength(2);
    expect(Array.isArray(probe.dynamicOutputs)).toBe(true);
    expect(probe.dynamicOutputs).toHaveLength(1);
  });

  test('computes correct port y-positions for 2 inputs', () => {
    const probe: any = { params: { Inputs: 2 } };
    muxResizer(probe);
    expect(probe.dynamicInputs[0].y).toBeCloseTo(10);
    expect(probe.dynamicInputs[1].y).toBeCloseTo(30);
    expect(probe.dynamicOutputs[0].y).toBeCloseTo(20);
  });

  test('scales height for more inputs', () => {
    const probe: any = { params: { Inputs: 4 } };
    muxResizer(probe);
    expect(probe.height).toBe(80);
    expect(probe.dynamicInputs).toHaveLength(4);
  });

  test('all input ports have side: left, output side: right', () => {
    const probe: any = { params: { Inputs: 3 } };
    muxResizer(probe);
    probe.dynamicInputs.forEach((p: any) => expect(p.side).toBe('left'));
    probe.dynamicOutputs.forEach((p: any) => expect(p.side).toBe('right'));
  });

  test('clamps to minimum 2 inputs', () => {
    const probe: any = { params: { Inputs: 1 } };
    muxResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(2);
  });

  test('falls back to 2 inputs when params absent', () => {
    const probe: any = {};
    muxResizer(probe);
    expect(probe.dynamicInputs).toHaveLength(2);
  });

  test('output port x equals block width', () => {
    const probe: any = { params: { Inputs: 3 } };
    muxResizer(probe);
    expect(probe.dynamicOutputs[0].x).toBe(44);
  });

  test('input ports have x: 0', () => {
    const probe: any = { params: { Inputs: 3 } };
    muxResizer(probe);
    probe.dynamicInputs.forEach((p: any) => expect(p.x).toBe(0));
  });
});
//=============================================================================
describe('muxRenderer', () => {
  test('renders a rect.nf-block', () => {
    const group = document.createElementNS(ns, 'g');
    const probe: any = { params: { Inputs: 2 } };
    muxResizer(probe);
    const block = {
      id: 'b1',
      type: 'mux',
      group,
      width: probe.width,
      height: probe.height,
      params: { Inputs: 2 },
      dynamicInputs: probe.dynamicInputs,
      dynamicOutputs: probe.dynamicOutputs,
    };
    muxRenderer(block, makeHelpers());
    const rect = group.querySelector('rect.nf-block');
    expect(rect).not.toBeNull();
    expect(rect!.getAttribute('fill')).toBe('#000000');
  });

  test('renders stub lines for each input port', () => {
    const group = document.createElementNS(ns, 'g');
    const probe: any = { params: { Inputs: 3 } };
    muxResizer(probe);
    const block = {
      id: 'b2',
      type: 'mux',
      group,
      width: probe.width,
      height: probe.height,
      params: { Inputs: 3 },
      dynamicInputs: probe.dynamicInputs,
      dynamicOutputs: probe.dynamicOutputs,
    };
    muxRenderer(block, makeHelpers());
    const stubs = group.querySelectorAll('line.mux-stub');
    // 3 input stubs + 1 output stub
    expect(stubs.length).toBe(4);
  });

  test('renders without crashing when dynamicInputs absent', () => {
    const group = document.createElementNS(ns, 'g');
    const block = {
      id: 'b3',
      type: 'mux',
      group,
      width: 44,
      height: 40,
      params: { Inputs: 2 },
    };
    expect(() => muxRenderer(block as any, makeHelpers())).not.toThrow();
  });
});
//=============================================================================
