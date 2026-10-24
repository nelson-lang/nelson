//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { demuxRenderer, demuxResizer } from './demux';
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
describe('demuxResizer', () => {
  test('mutates blockDef with default 2 outputs', () => {
    const probe: any = { params: { Outputs: 2 } };
    demuxResizer(probe);
    expect(probe.width).toBe(44);
    expect(probe.height).toBe(40);
    expect(Array.isArray(probe.dynamicInputs)).toBe(true);
    expect(probe.dynamicInputs).toHaveLength(1);
    expect(Array.isArray(probe.dynamicOutputs)).toBe(true);
    expect(probe.dynamicOutputs).toHaveLength(2);
  });

  test('computes correct port y-positions for 2 outputs', () => {
    const probe: any = { params: { Outputs: 2 } };
    demuxResizer(probe);
    expect(probe.dynamicInputs[0].y).toBeCloseTo(20);
    expect(probe.dynamicOutputs[0].y).toBeCloseTo(10);
    expect(probe.dynamicOutputs[1].y).toBeCloseTo(30);
  });

  test('scales height for more outputs', () => {
    const probe: any = { params: { Outputs: 4 } };
    demuxResizer(probe);
    expect(probe.height).toBe(80);
    expect(probe.dynamicOutputs).toHaveLength(4);
  });

  test('all output ports have side: right, input side: left', () => {
    const probe: any = { params: { Outputs: 3 } };
    demuxResizer(probe);
    probe.dynamicOutputs.forEach((p: any) => expect(p.side).toBe('right'));
    probe.dynamicInputs.forEach((p: any) => expect(p.side).toBe('left'));
  });

  test('clamps to minimum 2 outputs', () => {
    const probe: any = { params: { Outputs: 1 } };
    demuxResizer(probe);
    expect(probe.dynamicOutputs).toHaveLength(2);
  });

  test('falls back to 2 outputs when params absent', () => {
    const probe: any = {};
    demuxResizer(probe);
    expect(probe.dynamicOutputs).toHaveLength(2);
  });

  test('output ports have x equal to block width', () => {
    const probe: any = { params: { Outputs: 3 } };
    demuxResizer(probe);
    probe.dynamicOutputs.forEach((p: any) => expect(p.x).toBe(44));
  });

  test('input port has x: 0', () => {
    const probe: any = { params: { Outputs: 3 } };
    demuxResizer(probe);
    expect(probe.dynamicInputs[0].x).toBe(0);
  });
});
//=============================================================================
describe('demuxRenderer', () => {
  test('renders a rect.nf-block', () => {
    const group = document.createElementNS(ns, 'g');
    const probe: any = { params: { Outputs: 2 } };
    demuxResizer(probe);
    const block = {
      id: 'b1',
      type: 'demux',
      group,
      width: probe.width,
      height: probe.height,
      params: { Outputs: 2 },
      dynamicInputs: probe.dynamicInputs,
      dynamicOutputs: probe.dynamicOutputs,
    };
    demuxRenderer(block, makeHelpers());
    const rect = group.querySelector('rect.nf-block');
    expect(rect).not.toBeNull();
    expect(rect!.getAttribute('fill')).toBe('#000000');
  });

  test('renders stub lines for each output port', () => {
    const group = document.createElementNS(ns, 'g');
    const probe: any = { params: { Outputs: 3 } };
    demuxResizer(probe);
    const block = {
      id: 'b2',
      type: 'demux',
      group,
      width: probe.width,
      height: probe.height,
      params: { Outputs: 3 },
      dynamicInputs: probe.dynamicInputs,
      dynamicOutputs: probe.dynamicOutputs,
    };
    demuxRenderer(block, makeHelpers());
    const stubs = group.querySelectorAll('line.demux-stub');
    // 1 input stub + 3 output stubs
    expect(stubs.length).toBe(4);
  });

  test('renders without crashing when dynamicOutputs absent', () => {
    const group = document.createElementNS(ns, 'g');
    const block = {
      id: 'b3',
      type: 'demux',
      group,
      width: 44,
      height: 40,
      params: { Outputs: 2 },
    };
    expect(() => demuxRenderer(block as any, makeHelpers())).not.toThrow();
  });
});
//=============================================================================
