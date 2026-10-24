//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { buildDiagramAndScopeMap, flattenMuxDemux } from './resolve-params.js';
//=============================================================================
describe('buildDiagramAndScopeMap', () => {
  it('lifts physical-pin connections into physicalWires so the Run lowers acausal blocks', () => {
    // Two acausal blocks joined at their flange pins. The engine only lowers a model
    // that carries physicalWires, so Run must lift the edge. Otherwise Inertia / Fixed
    // are rejected (unsupported_block).
    const blocks = new Map<string, unknown>([
      [
        'M1',
        { id: 'M1', type: 'Mass', acausal: true, physicalPins: [{ name: 'flange' }], params: {} },
      ],
      [
        'W',
        { id: 'W', type: 'Fixed', acausal: true, physicalPins: [{ name: 'flange' }], params: {} },
      ],
    ]);
    const state = {
      blocks,
      connections: [{ from: 'M1', to: 'W', fromIndex: 0, toIndex: 0 }],
      variables: {},
      sampleTime: 0.01,
    };

    const { diagram } = buildDiagramAndScopeMap(state as any, { value: '10' } as any) as any;

    expect(Array.isArray(diagram.physicalWires)).toBe(true);
    expect(diagram.physicalWires.length).toBe(1);
    const pins = diagram.physicalWires[0].pins.slice().sort();
    expect(pins).toEqual(['M1.flange', 'W.flange']);
    expect(diagram.connections.length).toBe(0);
  });

  it('leaves a purely causal diagram without a physicalWires field', () => {
    const blocks = new Map<string, unknown>([
      ['g', { id: 'g', type: 'gain', params: { Gain: 1 } }],
      ['s', { id: 's', type: 'scope', params: {} }],
    ]);
    const state = {
      blocks,
      connections: [{ from: 'g', to: 's', fromIndex: 0, toIndex: 0 }],
      variables: {},
      sampleTime: 0.01,
    };

    const { diagram } = buildDiagramAndScopeMap(state as any, { value: '10' } as any) as any;

    expect(diagram).not.toHaveProperty('physicalWires');
    expect(diagram.connections.length).toBe(1);
  });

  it('propagates explicit realTime mode to the simulation payload', () => {
    const state = {
      blocks: new Map(),
      connections: [],
      variables: {},
      sampleTime: 0.01,
      realTime: false,
    };

    const { diagram } = buildDiagramAndScopeMap(state as any, { value: '10' } as any);

    expect((diagram as any).realTime).toBe(false);
  });

  it('omits realTime from the simulation payload when unspecified', () => {
    const state = {
      blocks: new Map(),
      connections: [],
      variables: {},
      sampleTime: 0.01,
    };

    const { diagram } = buildDiagramAndScopeMap(state as any, { value: '10' } as any);

    expect(diagram).not.toHaveProperty('realTime');
  });

  it('forwards solver tolerances to the payload for a variable-step solver (A4.1)', () => {
    const state = {
      blocks: new Map(),
      connections: [],
      variables: {},
      sampleTime: 0.01,
      solver: 'variableStiff',
      relTol: 1e-5,
      absTol: 1e-7,
      maxStep: 0.02,
    };

    const { diagram } = buildDiagramAndScopeMap(state as any, { value: '10' } as any) as any;

    expect(diagram.solver).toBe('variableStiff');
    expect(diagram.relTol).toBe(1e-5);
    expect(diagram.absTol).toBe(1e-7);
    expect(diagram.maxStep).toBe(0.02);
  });

  it('omits solver tolerances when no solver is selected (discrete)', () => {
    const state = {
      blocks: new Map(),
      connections: [],
      variables: {},
      sampleTime: 0.01,
      solver: 'discrete',
      relTol: 1e-5,
      absTol: 1e-7,
      maxStep: 0.02,
    };

    const { diagram } = buildDiagramAndScopeMap(state as any, { value: '10' } as any) as any;

    expect(diagram).not.toHaveProperty('solver');
    expect(diagram).not.toHaveProperty('relTol');
    expect(diagram).not.toHaveProperty('absTol');
    expect(diagram).not.toHaveProperty('maxStep');
  });
});
//=============================================================================
describe('flattenMuxDemux', () => {
  it('rewires mux->demux chains into direct connections', () => {
    const diagram = {
      blocks: [
        { id: 's1', type: 'const' },
        { id: 's2', type: 'const' },
        { id: 'mux1', type: 'mux' },
        { id: 'demux1', type: 'demux' },
        { id: 't1', type: 'display' },
        { id: 't2', type: 'display' },
      ],
      connections: [
        { from: 's1', to: 'mux1', fromIndex: 0, toIndex: 0 },
        { from: 's2', to: 'mux1', fromIndex: 0, toIndex: 1 },
        { from: 'mux1', to: 'demux1', fromIndex: 0, toIndex: 0 },
        { from: 'demux1', to: 't1', fromIndex: 0, toIndex: 0 },
        { from: 'demux1', to: 't2', fromIndex: 1, toIndex: 0 },
      ],
    };

    const out = flattenMuxDemux(diagram as any) as any;
    const keys = (out.connections || []).map(
      (c: any) => `${c.from}:${c.fromIndex ?? 0}->${c.to}:${c.toIndex ?? 0}`
    );

    expect(keys).toContain('s1:0->t1:0');
    expect(keys).toContain('s2:0->t2:0');

    expect(keys).not.toContain('mux1:0->demux1:0');
    expect(keys).not.toContain('demux1:0->t1:0');
    expect(keys).not.toContain('demux1:1->t2:0');

    expect(keys).toContain('s1:0->mux1:0');
    expect(keys).toContain('s2:0->mux1:1');
  });

  it('keeps a demux fed by a non-mux source wired to its consumers', () => {
    // A standalone demux (no mux feeding it) must not have its output edges
    // dropped: the consumers would otherwise be stranded with no input.
    const diagram = {
      blocks: [
        { id: 'c', type: 'const' },
        { id: 'demux1', type: 'demux' },
        { id: 't1', type: 'display' },
        { id: 't2', type: 'display' },
      ],
      connections: [
        { from: 'c', to: 'demux1', fromIndex: 0, toIndex: 0 },
        { from: 'demux1', to: 't1', fromIndex: 0, toIndex: 0 },
        { from: 'demux1', to: 't2', fromIndex: 1, toIndex: 0 },
      ],
    };
    const out = flattenMuxDemux(diagram as any) as any;
    const keys = (out.connections || []).map(
      (c: any) => `${c.from}:${c.fromIndex ?? 0}->${c.to}:${c.toIndex ?? 0}`
    );
    expect(keys).toContain('c:0->demux1:0');
    expect(keys).toContain('demux1:0->t1:0');
    expect(keys).toContain('demux1:1->t2:0');
  });

  it('keeps non-mux/demux connections intact', () => {
    const diagram = {
      blocks: [
        { id: 'a', type: 'const' },
        { id: 'b', type: 'display' },
      ],
      connections: [{ from: 'a', to: 'b', fromIndex: 0, toIndex: 0 }],
    };
    const out = flattenMuxDemux(diagram as any) as any;
    expect(out.connections).toEqual(diagram.connections);
  });
});
//=============================================================================
