//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect, vi } from 'vitest';
import {
  parseFmuInspection,
  fmuOutputPortCount,
  applyFmuInspection,
  browseAndInspectFmu,
  recomputeFmuPortWidths,
} from './fmu-inspect.js';
import { fmuResizer } from './fmu.js';
//=============================================================================
const VANDERPOL = JSON.stringify({
  success: true,
  modelIdentifier: 'VanDerPol',
  fmiVersion: '3.0',
  coSimulation: true,
  variables: [
    { name: 'time', valueReference: 0, causality: 'independent', dataType: 'Float64' },
    {
      name: 'x0',
      valueReference: 1,
      causality: 'output',
      dataType: 'Float64',
      description: 'position',
    },
    { name: 'der(x0)', valueReference: 2, causality: 'local', dataType: 'Float64' },
    { name: 'x1', valueReference: 3, causality: 'output', dataType: 'Float64' },
    { name: 'mu', valueReference: 5, causality: 'parameter', dataType: 'Float64' },
    { name: 'flag', valueReference: 6, causality: 'output', dataType: 'Boolean' },
  ],
});
//=============================================================================
const FEEDTHROUGH = JSON.stringify({
  success: true,
  modelIdentifier: 'Feedthrough',
  fmiVersion: '3.0',
  coSimulation: true,
  variables: [
    { name: 'u', valueReference: 7, causality: 'input', dataType: 'Float64' },
    { name: 'y', valueReference: 8, causality: 'output', dataType: 'Float64' },
    { name: 'flag', valueReference: 9, causality: 'input', dataType: 'Boolean' },
  ],
});
//=============================================================================
// StateSpace-like FMU: structural parameters m/r with array dimensions
// bound to their value references.
const STATESPACE = JSON.stringify({
  success: true,
  modelIdentifier: 'StateSpace',
  fmiVersion: '3.0',
  coSimulation: true,
  variables: [
    {
      name: 'm',
      valueReference: 1,
      causality: 'structuralParameter',
      dataType: 'UInt64',
      startValue: 3,
    },
    {
      name: 'r',
      valueReference: 3,
      causality: 'structuralParameter',
      dataType: 'UInt64',
      startValue: 3,
    },
    {
      name: 'u',
      valueReference: 9,
      causality: 'input',
      dataType: 'Float64',
      arraySize: 3,
      dimensions: [{ vr: 1, start: 3 }],
    },
    {
      name: 'y',
      valueReference: 10,
      causality: 'output',
      dataType: 'Float64',
      arraySize: 3,
      dimensions: [{ vr: 3, start: 3 }],
    },
  ],
});
//=============================================================================
describe('structural parameters and bound dimensions', () => {
  it('parses structural parameters and per-dimension descriptors', () => {
    const info = parseFmuInspection(STATESPACE);
    expect(info.ok).toBe(true);
    expect(info.parameters.map((p) => p.name)).toEqual(['m', 'r']);
    expect(info.parameters[0].structural).toBe(true);
    expect(info.parameters[0].startValue).toBe(3);
    expect(info.outputs[0].dimensions).toEqual([{ vr: 3, start: 3 }]);
    expect(info.inputs[0].dimensions).toEqual([{ vr: 1, start: 3 }]);
  });

  it('applyFmuInspection stores dimension metadata and seeds structural values', async () => {
    const block: { params?: Record<string, unknown> } = {};
    await applyFmuInspection(block, 'StateSpace.fmu', vi.fn().mockResolvedValue(STATESPACE));
    expect(block.params?.outputWidths).toEqual([3]);
    expect(block.params?.inputWidths).toEqual([3]);
    expect(block.params?.parameterStructural).toEqual([true, true]);
    expect(block.params?.parameterVrs).toEqual([1, 3]);
    expect(block.params?.parameterValues).toEqual({ m: 3, r: 3 });
    expect(block.params?.outputDimensions).toEqual([[{ vr: 3, start: 3 }]]);
  });

  it('keeps the FMU defaults for array parameters', async () => {
    const description = JSON.parse(STATESPACE);
    description.variables.push({
      name: 'A',
      valueReference: 4,
      causality: 'parameter',
      dataType: 'Float64',
      arraySize: 9,
    });
    const block: { params?: Record<string, unknown> } = {};
    await applyFmuInspection(
      block,
      'StateSpace.fmu',
      vi.fn().mockResolvedValue(JSON.stringify(description))
    );
    expect(block.params?.parameterValues).toEqual({ m: 3, r: 3 });
  });

  it('recomputeFmuPortWidths resizes ports bound to an overridden structural parameter', async () => {
    const block: { params?: Record<string, unknown> } = {};
    await applyFmuInspection(block, 'StateSpace.fmu', vi.fn().mockResolvedValue(STATESPACE));
    (block.params!.parameterValues as Record<string, number>).r = 2;
    expect(recomputeFmuPortWidths(block)).toBe(true);
    expect(block.params?.outputWidths).toEqual([2]);
    expect(block.params?.inputWidths).toEqual([3]);
    expect(recomputeFmuPortWidths(block)).toBe(false);
  });

  it('recomputeFmuPortWidths is a no-op without dimension metadata', () => {
    const block = { params: { outputWidths: [1] } };
    expect(recomputeFmuPortWidths(block)).toBe(false);
    expect(block.params.outputWidths).toEqual([1]);
  });
});
//=============================================================================
describe('parseFmuInspection', () => {
  it('keeps every portable output (numeric + boolean)', () => {
    const info = parseFmuInspection(VANDERPOL);
    expect(info.ok).toBe(true);
    expect(info.fmiVersion).toBe('3.0');
    expect(info.coSimulation).toBe(true);
    expect(info.inputs).toHaveLength(0);
    expect(info.outputs.map((o) => o.name)).toEqual(['x0', 'x1', 'flag']);
    expect(info.outputs[0].valueReference).toBe(1);
    expect(info.outputs[0].description).toBe('position');
  });

  it('keeps portable inputs (numeric + boolean)', () => {
    const info = parseFmuInspection(FEEDTHROUGH);
    expect(info.inputs.map((i) => i.name)).toEqual(['u', 'flag']);
    expect(info.outputs.map((o) => o.name)).toEqual(['y']);
  });

  it('reports a failed inspection', () => {
    const info = parseFmuInspection('{"success":false,"error":"nope"}');
    expect(info.ok).toBe(false);
    expect(info.error).toBe('nope');
    expect(info.outputs).toHaveLength(0);
  });

  it('handles invalid JSON', () => {
    const info = parseFmuInspection('not json');
    expect(info.ok).toBe(false);
    expect(info.outputs).toHaveLength(0);
  });

  it('reads an embedded icon data URI', () => {
    const withIcon = JSON.stringify({
      success: true,
      fmiVersion: '3.0',
      coSimulation: true,
      icon: 'data:image/png;base64,AAAA',
      variables: [{ name: 'y', valueReference: 1, causality: 'output', dataType: 'Float64' }],
    });
    expect(parseFmuInspection(withIcon).iconUri).toBe('data:image/png;base64,AAAA');
    expect(parseFmuInspection(VANDERPOL).iconUri).toBeUndefined();
  });
});
//=============================================================================
const BOUNCINGBALL = JSON.stringify({
  success: true,
  fmiVersion: '3.0',
  coSimulation: true,
  variables: [
    { name: 'h', valueReference: 1, causality: 'output', dataType: 'Float64', startValue: 1 },
    {
      name: 'g',
      valueReference: 5,
      causality: 'parameter',
      dataType: 'Float64',
      startValue: -9.81,
    },
    { name: 'e', valueReference: 6, causality: 'parameter', dataType: 'Float64', startValue: 0.7 },
  ],
});
//=============================================================================
describe('parseFmuInspection parameters', () => {
  it('collects Float64 parameters with their start values', () => {
    const info = parseFmuInspection(BOUNCINGBALL);
    expect(info.parameters.map((p) => p.name)).toEqual(['g', 'e']);
    expect(info.parameters[0].startValue).toBe(-9.81);
    expect(info.outputs[0].startValue).toBe(1);
  });
});
//=============================================================================
describe('applyFmuInspection parameters', () => {
  it('seeds parameterValues with the FMU start values', async () => {
    const rpc = vi.fn().mockResolvedValue(BOUNCINGBALL);
    const block: { params?: Record<string, unknown> } = {};
    await applyFmuInspection(block, 'BouncingBall.fmu', rpc);
    expect(block.params?.parameterNames).toEqual(['g', 'e']);
    expect(block.params?.parameterValues).toMatchObject({ g: -9.81, e: 0.7 });
  });
});
//=============================================================================
const VANDERPOL_ME = JSON.stringify({
  success: true,
  modelIdentifier: 'VanDerPol',
  fmiVersion: '3.0',
  coSimulation: false,
  modelExchange: true,
  numberOfContinuousStates: 2,
  numberOfEventIndicators: 0,
  variables: [
    { name: 'x0', valueReference: 1, causality: 'output', dataType: 'Float64' },
    { name: 'x1', valueReference: 3, causality: 'output', dataType: 'Float64' },
  ],
});
//=============================================================================
describe('parseFmuInspection Model Exchange', () => {
  it('reads the ME interface flag and state/event sizing', () => {
    const info = parseFmuInspection(VANDERPOL_ME);
    expect(info.modelExchange).toBe(true);
    expect(info.coSimulation).toBe(false);
    expect(info.numContinuousStates).toBe(2);
    expect(info.numEventIndicators).toBe(0);
  });

  it('leaves the ME sizing undefined for a Co-Simulation-only FMU', () => {
    const info = parseFmuInspection(VANDERPOL);
    expect(info.modelExchange).toBe(false);
    expect(info.numContinuousStates).toBeUndefined();
    expect(info.numEventIndicators).toBeUndefined();
  });
});
//=============================================================================
describe('applyFmuInspection Model Exchange sizing', () => {
  it('writes numContinuousStates / numEventIndicators for an ME FMU', async () => {
    const rpc = vi.fn().mockResolvedValue(VANDERPOL_ME);
    const block: { params?: Record<string, unknown> } = {};
    await applyFmuInspection(block, 'VanDerPol.fmu', rpc);
    expect(block.params?.numContinuousStates).toBe(2);
    expect(block.params?.numEventIndicators).toBe(0);
  });

  it('clears the ME sizing when re-inspecting with a CS-only FMU', async () => {
    const rpc = vi.fn().mockResolvedValue(VANDERPOL);
    const block: { params?: Record<string, unknown> } = {
      params: { numContinuousStates: 2, numEventIndicators: 1 },
    };
    await applyFmuInspection(block, 'VanDerPol.fmu', rpc);
    expect(block.params?.numContinuousStates).toBeUndefined();
    expect(block.params?.numEventIndicators).toBeUndefined();
  });
});
//=============================================================================
describe('fmuOutputPortCount', () => {
  it('is the number of outputs, at least one', () => {
    expect(fmuOutputPortCount(parseFmuInspection(VANDERPOL))).toBe(3);
    expect(fmuOutputPortCount({ ok: true, inputs: [], outputs: [], parameters: [] })).toBe(1);
  });
});
//=============================================================================
describe('applyFmuInspection', () => {
  it('writes path, port count and output names into the block params', async () => {
    const rpc = vi.fn().mockResolvedValue(VANDERPOL);
    const block: { params?: Record<string, unknown> } = {};
    const info = await applyFmuInspection(block, 'C:/fmus/VanDerPol.fmu', rpc);
    expect(rpc).toHaveBeenCalledWith('nflow.fmi3.inspect', { path: 'C:/fmus/VanDerPol.fmu' });
    expect(info.ok).toBe(true);
    expect(block.params?.path).toBe('C:/fmus/VanDerPol.fmu');
    expect(block.params?.numInputs).toBe(0);
    expect(block.params?.numOutputs).toBe(3);
    expect(block.params?.outputNames).toEqual(['x0', 'x1', 'flag']);
    expect(block.params?.outputDescriptions).toEqual(['position', '', '']);
  });

  it('writes input ports for an FMU with inputs', async () => {
    const rpc = vi.fn().mockResolvedValue(FEEDTHROUGH);
    const block: { params?: Record<string, unknown> } = {};
    await applyFmuInspection(block, 'Feedthrough.fmu', rpc);
    expect(block.params?.numInputs).toBe(2);
    expect(block.params?.inputNames).toEqual(['u', 'flag']);
    expect(block.params?.numOutputs).toBe(1);
  });

  it('leaves the block unchanged on a failed inspection', async () => {
    const rpc = vi.fn().mockResolvedValue('{"success":false,"error":"bad fmu"}');
    const block: { params?: Record<string, unknown> } = { params: { path: 'old' } };
    const info = await applyFmuInspection(block, 'X.fmu', rpc);
    expect(info.ok).toBe(false);
    expect(block.params?.path).toBe('old');
    expect(block.params?.numOutputs).toBeUndefined();
  });
});
//=============================================================================
describe('browseAndInspectFmu', () => {
  it('picks a file, inspects it and updates the block', async () => {
    const rpc = vi.fn().mockResolvedValue(VANDERPOL);
    const pickFmuFile = vi.fn().mockResolvedValue('D:/fmus/VanDerPol.fmu');
    const block: { params?: Record<string, unknown> } = {};
    const res = await browseAndInspectFmu(block, { pickFmuFile, rpc });
    expect(res.canceled).toBe(false);
    expect(res.path).toBe('D:/fmus/VanDerPol.fmu');
    expect(block.params?.numOutputs).toBe(3);
  });

  it('does nothing when the picker is canceled', async () => {
    const rpc = vi.fn();
    const pickFmuFile = vi.fn().mockResolvedValue(null);
    const block: { params?: Record<string, unknown> } = {};
    const res = await browseAndInspectFmu(block, { pickFmuFile, rpc });
    expect(res.canceled).toBe(true);
    expect(rpc).not.toHaveBeenCalled();
    expect(block.params).toBeUndefined();
  });
});
//=============================================================================
describe('fmuResizer', () => {
  it('lays out one output port per FMU output and no inputs by default', () => {
    const def: any = { params: { numOutputs: 2 } };
    fmuResizer(def);
    expect(def.dynamicInputs).toHaveLength(0);
    expect(def.dynamicOutputs).toHaveLength(2);
    expect(def.dynamicOutputs.every((p: any) => p.side === 'right')).toBe(true);
  });

  it('lays out input ports on the left when the FMU has inputs', () => {
    const def: any = { params: { numInputs: 1, numOutputs: 1 } };
    fmuResizer(def);
    expect(def.dynamicInputs).toHaveLength(1);
    expect(def.dynamicInputs[0].side).toBe('left');
    expect(def.dynamicOutputs).toHaveLength(1);
  });

  it('defaults to a single output port', () => {
    const def: any = { params: {} };
    fmuResizer(def);
    expect(def.dynamicOutputs).toHaveLength(1);
  });
});
//=============================================================================
