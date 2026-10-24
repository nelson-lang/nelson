//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// End-to-end tests for mux and demux blocks through the TypeScript simulation
// pipeline (the native RPC simulation backend is mocked).
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import { vi } from 'vitest';
import { buildDiagramAndScopeMap } from '../../sim/resolve-params';
import { muxResizer } from './mux';
import { demuxResizer } from './demux';
import { simulate } from '../../sim/sim';
//=============================================================================
function installSimRpcBackend(handler: (diagramJson: string) => unknown): ReturnType<typeof vi.fn> {
  const backend = vi.fn(handler);
  (global as any).window.webui = {
    call: async (name: string, payload: string) => {
      if (name !== 'rpc') throw new Error(`unexpected call: ${name}`);
      const envelope = JSON.parse(payload);
      if (envelope.method !== 'nflow.sim.run') {
        throw new Error(`unexpected method: ${envelope.method}`);
      }
      try {
        return JSON.stringify({
          id: envelope.id,
          result: await backend(String(envelope.params?.diagramJson ?? '')),
        });
      } catch (err) {
        return JSON.stringify({
          id: envelope.id,
          error: { message: (err as Error)?.message || String(err) },
        });
      }
    },
  };
  return backend;
}
//=============================================================================
function makeMuxBlock(id: string, numInputs: number): any {
  const probe: any = { params: { inputs: numInputs } };
  muxResizer(probe);
  return {
    id,
    type: 'mux',
    inputs: numInputs,
    outputs: 1,
    params: { Inputs: numInputs },
    width: probe.width,
    height: probe.height,
    dynamicInputs: probe.dynamicInputs,
    dynamicOutputs: probe.dynamicOutputs,
  };
}
//=============================================================================
function makeDemuxBlock(id: string, numOutputs: number): any {
  const probe: any = { params: { outputs: numOutputs } };
  demuxResizer(probe);
  return {
    id,
    type: 'demux',
    inputs: 1,
    outputs: numOutputs,
    params: { Outputs: numOutputs },
    width: probe.width,
    height: probe.height,
    dynamicInputs: probe.dynamicInputs,
    dynamicOutputs: probe.dynamicOutputs,
  };
}
//=============================================================================
function makeState(blockList: any[], connections: any[]): any {
  const blocks = new Map<string, any>();
  for (const b of blockList) blocks.set(b.id, b);
  return { blocks, connections };
}
//=============================================================================
describe('buildDiagramAndScopeMap - mux block', () => {
  test('serializes mux with correct type and inputs param', () => {
    const state = makeState([makeMuxBlock('m1', 3)], []);
    const { diagram } = buildDiagramAndScopeMap(state);
    const diag = diagram as any;
    const b = diag.blocks.find((x: any) => x.id === 'm1');
    expect(b).toBeDefined();
    expect(b.type).toBe('mux');
    expect(b.params.Inputs).toBe(3);
  });

  test('preserves inputs port count on the block entry', () => {
    const state = makeState([makeMuxBlock('m2', 4)], []);
    const { diagram } = buildDiagramAndScopeMap(state);
    const b = (diagram as any).blocks.find((x: any) => x.id === 'm2');
    expect(b.inputs).toBe(4);
  });

  test('outputs field is 1 for mux', () => {
    const state = makeState([makeMuxBlock('m3', 2)], []);
    const { diagram } = buildDiagramAndScopeMap(state);
    const b = (diagram as any).blocks.find((x: any) => x.id === 'm3');
    expect(b.outputs).toBe(1);
  });
});
//=============================================================================
describe('buildDiagramAndScopeMap - demux block', () => {
  test('serializes demux with correct type and outputs param', () => {
    const state = makeState([makeDemuxBlock('d1', 3)], []);
    const { diagram } = buildDiagramAndScopeMap(state);
    const b = (diagram as any).blocks.find((x: any) => x.id === 'd1');
    expect(b).toBeDefined();
    expect(b.type).toBe('demux');
    expect(b.params.Outputs).toBe(3);
  });

  test('preserves outputs count on the block entry', () => {
    const state = makeState([makeDemuxBlock('d2', 4)], []);
    const { diagram } = buildDiagramAndScopeMap(state);
    const b = (diagram as any).blocks.find((x: any) => x.id === 'd2');
    expect(b.outputs).toBe(4);
  });

  test('inputs field is 1 for demux', () => {
    const state = makeState([makeDemuxBlock('d3', 2)], []);
    const { diagram } = buildDiagramAndScopeMap(state);
    const b = (diagram as any).blocks.find((x: any) => x.id === 'd3');
    expect(b.inputs).toBe(1);
  });
});
//=============================================================================
describe('buildDiagramAndScopeMap - mux→demux chain', () => {
  function buildChainState(n: number) {
    const src = { id: 'const', type: 'constant', inputs: 0, outputs: 1, params: { Value: 7 } };
    const mux = makeMuxBlock('mux', n);
    const dmx = makeDemuxBlock('dmx', n);
    const scope = { id: 'sc', type: 'scope', inputs: 1, outputs: 0, params: {} };

    const connections: any[] = [];
    for (let i = 0; i < n; i++) {
      connections.push({ from: 'const', to: 'mux', fromIndex: 0, toIndex: i });
    }
    connections.push({ from: 'mux', to: 'dmx', fromIndex: 0, toIndex: 0 });
    connections.push({ from: 'dmx', to: 'sc', fromIndex: 0, toIndex: 0 });

    return makeState([src, mux, dmx, scope], connections);
  }

  test('diagram contains mux and demux blocks for a 2-wide chain', () => {
    const { diagram } = buildDiagramAndScopeMap(buildChainState(2));
    const diag = diagram as any;
    expect(diag.blocks.some((b: any) => b.type === 'mux')).toBe(true);
    expect(diag.blocks.some((b: any) => b.type === 'demux')).toBe(true);
  });

  test('connections include mux→demux link', () => {
    const { diagram } = buildDiagramAndScopeMap(buildChainState(3));
    const conns = (diagram as any).connections as any[];
    const link = conns.find((c) => c.from === 'mux' && c.to === 'dmx');
    expect(link).toBeDefined();
    expect(link.fromIndex).toBe(0);
    expect(link.toIndex).toBe(0);
  });

  test('connections include all n constant→mux fan-in wires', () => {
    const n = 4;
    const { diagram } = buildDiagramAndScopeMap(buildChainState(n));
    const conns = (diagram as any).connections as any[];
    const fanIn = conns.filter((c) => c.from === 'const' && c.to === 'mux');
    expect(fanIn).toHaveLength(n);
    const toIndexes = fanIn.map((c: any) => c.toIndex).sort((a: number, b: number) => a - b);
    expect(toIndexes).toEqual(Array.from({ length: n }, (_, i) => i));
  });

  test('scope block appears in scopeBlockMap', () => {
    const { scopeBlockMap } = buildDiagramAndScopeMap(buildChainState(2));
    expect(scopeBlockMap.has('sc')).toBe(true);
  });
});
//=============================================================================
describe('simulate() - mux/demux in diagram JSON', () => {
  beforeEach(() => {
    (global as any).requestAnimationFrame = (cb: any) => {
      cb();
      return 1;
    };
    (global as any).cancelAnimationFrame = () => {};
  });
  afterEach(() => {
    delete (global as any).requestAnimationFrame;
    delete (global as any).cancelAnimationFrame;
    delete (global as any).window.webui;
  });

  test('nflow.sim.run is called with JSON containing mux block', async () => {
    const capturedArgs: string[] = [];
    installSimRpcBackend((payload: string) => {
      capturedArgs.push(payload);
      return JSON.stringify({ status: 'done', scopes: [] });
    });

    const state = makeState([makeMuxBlock('mux', 2)], []);
    const statusEl: any = { textContent: '' };
    await simulate({ state, statusEl });

    expect(capturedArgs.length).toBeGreaterThan(0);
    const diag = JSON.parse(capturedArgs[0]);
    const muxBlock = diag.blocks.find((b: any) => b.type === 'mux');
    expect(muxBlock).toBeDefined();
    expect(muxBlock.params.Inputs).toBe(2);
  });

  test('nflow.sim.run is called with JSON containing demux block', async () => {
    const capturedArgs: string[] = [];
    installSimRpcBackend((payload: string) => {
      capturedArgs.push(payload);
      return JSON.stringify({ status: 'done', scopes: [] });
    });

    const state = makeState([makeDemuxBlock('dmx', 3)], []);
    const statusEl: any = { textContent: '' };
    await simulate({ state, statusEl });

    const diag = JSON.parse(capturedArgs[0]);
    const demuxBlock = diag.blocks.find((b: any) => b.type === 'demux');
    expect(demuxBlock).toBeDefined();
    expect(demuxBlock.params.Outputs).toBe(3);
  });

  test('returns done status when nflow.sim.run succeeds', async () => {
    installSimRpcBackend(() => JSON.stringify({ status: 'done', scopes: [] }));

    const src = { id: 'c', type: 'constant', inputs: 0, outputs: 1, params: { Value: 1 } };
    const mux = makeMuxBlock('mux', 2);
    const dmx = makeDemuxBlock('dmx', 2);
    const scope = { id: 'sc', type: 'scope', inputs: 1, outputs: 0, params: {} };
    const connections = [
      { from: 'c', to: 'mux', fromIndex: 0, toIndex: 0 },
      { from: 'c', to: 'mux', fromIndex: 0, toIndex: 1 },
      { from: 'mux', to: 'dmx', fromIndex: 0, toIndex: 0 },
      { from: 'dmx', to: 'sc', fromIndex: 0, toIndex: 0 },
    ];
    const state = makeState([src, mux, dmx, scope], connections);

    const statusEl: any = { textContent: '' };
    const res = (await simulate({ state, statusEl })) as any;

    expect(res.status).toBe('done');
    expect(statusEl.textContent).toBe('Done');
  });
});
//=============================================================================
describe('simulate() - scope data routed after mux/demux chain', () => {
  beforeEach(() => {
    (global as any).requestAnimationFrame = (cb: any) => {
      cb();
      return 1;
    };
    (global as any).cancelAnimationFrame = () => {};
  });
  afterEach(() => {
    delete (global as any).requestAnimationFrame;
    delete (global as any).cancelAnimationFrame;
    delete (global as any).window.webui;
  });

  test('scope block receives scopeData from simulation result', async () => {
    const scopeTimes = [0, 0.01, 0.02];
    const scopeSeries = [[7, 7, 7]];
    installSimRpcBackend(() =>
      JSON.stringify({
        status: 'done',
        scopes: [
          { id: 'sc', type: 'scope', times: scopeTimes, series: scopeSeries, connected: [] },
        ],
      })
    );

    const src = { id: 'c', type: 'constant', inputs: 0, outputs: 1, params: { Value: 7 } };
    const mux = makeMuxBlock('mux', 2);
    const dmx = makeDemuxBlock('dmx', 2);
    const scope = { id: 'sc', type: 'scope', inputs: 1, outputs: 0, params: {} };
    const connections = [
      { from: 'c', to: 'mux', fromIndex: 0, toIndex: 0 },
      { from: 'c', to: 'mux', fromIndex: 0, toIndex: 1 },
      { from: 'mux', to: 'dmx', fromIndex: 0, toIndex: 0 },
      { from: 'dmx', to: 'sc', fromIndex: 0, toIndex: 0 },
    ];
    const state = makeState([src, mux, dmx, scope], connections);

    const statusEl: any = { textContent: '' };
    await simulate({ state, statusEl });

    expect(scope.scopeData).toBeDefined();
    expect((scope as any).scopeData.time).toEqual(scopeTimes);
    expect((scope as any).scopeData.series).toEqual(scopeSeries);
  });

  test('simulation error propagates through mux/demux diagram', async () => {
    installSimRpcBackend(() => {
      throw new Error('sim-crash');
    });

    const state = makeState(
      [makeMuxBlock('mux', 2), makeDemuxBlock('dmx', 2)],
      [{ from: 'mux', to: 'dmx', fromIndex: 0, toIndex: 0 }]
    );
    const statusEl: any = { textContent: '' };
    const res = (await simulate({ state, statusEl })) as any;

    expect(res.status).toBe('error');
    expect(statusEl.textContent).toMatch(/Error/);
  });
});
//=============================================================================
describe('resizer → diagram serialization integration', () => {
  test('mux with 5 inputs: diagram has inputs=5 and 5 connections land on mux', () => {
    const n = 5;
    const mux = makeMuxBlock('mux', n);
    const cons = Array.from({ length: n }, (_, i) => ({
      id: `c${i}`,
      type: 'constant',
      inputs: 0,
      outputs: 1,
      params: { Value: i },
    }));
    const connections = cons.map((c, i) => ({
      from: c.id,
      to: 'mux',
      fromIndex: 0,
      toIndex: i,
    }));
    const state = makeState([...cons, mux], connections);
    const { diagram } = buildDiagramAndScopeMap(state);

    const b = (diagram as any).blocks.find((x: any) => x.id === 'mux');
    expect(b.inputs).toBe(n);
    const toMux = (diagram as any).connections.filter((c: any) => c.to === 'mux');
    expect(toMux).toHaveLength(n);
  });

  test('demux with 5 outputs: diagram has outputs=5 and 5 connections come from demux', () => {
    const n = 5;
    const dmx = makeDemuxBlock('dmx', n);
    const src = { id: 'src', type: 'constant', inputs: 0, outputs: 1, params: { Value: 1 } };
    const sinks = Array.from({ length: n }, (_, i) => ({
      id: `sc${i}`,
      type: 'scope',
      inputs: 1,
      outputs: 0,
      params: {},
    }));
    const connections = [
      { from: 'src', to: 'dmx', fromIndex: 0, toIndex: 0 },
      ...sinks.map((s, i) => ({ from: 'dmx', to: s.id, fromIndex: i, toIndex: 0 })),
    ];
    const state = makeState([src, dmx, ...sinks], connections);
    const { diagram } = buildDiagramAndScopeMap(state);

    const b = (diagram as any).blocks.find((x: any) => x.id === 'dmx');
    expect(b.outputs).toBe(n);
    const fromDmx = (diagram as any).connections.filter((c: any) => c.from === 'dmx');
    expect(fromDmx).toHaveLength(n);
  });
});
//=============================================================================
