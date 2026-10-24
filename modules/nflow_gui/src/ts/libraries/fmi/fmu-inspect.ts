//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
//
// Editor-side logic for the FMU block: turn the nflow.fmi3.inspect RPC payload
// into the block's output ports. Kept free of DOM / RPC-transport details so it
// is unit-testable; the transport is injected as a callback.
//=============================================================================
/** One <Dimension> of an FMI3 array variable: a fixed size, or a size bound
 *  to a structural parameter (vr set; start = the parameter's default). */
export interface FmuDimension {
  start: number;
  vr?: number;
}
//=============================================================================
export interface FmuOutputVariable {
  name: string;
  valueReference: number;
  description?: string;
  startValue?: number;
  /** Array element count (FMI3 array variable); 1 for a scalar. */
  arraySize?: number;
  /** Per-dimension descriptors (absent for scalars). */
  dimensions?: FmuDimension[];
  /** True for a structural parameter (settable in Configuration Mode;
   *  array dimensions may be bound to its value). */
  structural?: boolean;
}
//=============================================================================
export interface FmuInspection {
  ok: boolean;
  error?: string;
  modelIdentifier?: string;
  fmiVersion?: string;
  coSimulation?: boolean;
  modelExchange?: boolean;
  /** Model Exchange sizing (present only for ME FMUs). */
  numContinuousStates?: number;
  numEventIndicators?: number;
  iconUri?: string;
  inputs: FmuOutputVariable[];
  outputs: FmuOutputVariable[];
  parameters: FmuOutputVariable[];
}
//=============================================================================
interface FmuBlockLike {
  params?: Record<string, unknown>;
}
//=============================================================================
// Transport: resolves to the raw JSON string returned by nflow.fmi3.inspect.
export type FmuInspectRpc = (method: string, params: Record<string, unknown>) => Promise<string>;
// Numeric and boolean variables become ports (the engine bridge uses the typed FMI
// getters); String/Binary/Clock variables cannot map onto a signal and are skipped.
const PORTABLE_FMI_TYPES = new Set([
  'Float64',
  'Float32',
  'Int8',
  'UInt8',
  'Int16',
  'UInt16',
  'Int32',
  'UInt32',
  'Int64',
  'UInt64',
  'Boolean',
  'Enumeration',
]);
//=============================================================================
export function parseFmuInspection(json: string): FmuInspection {
  let obj: Record<string, unknown>;
  try {
    obj = JSON.parse(json) as Record<string, unknown>;
  } catch {
    return {
      ok: false,
      error: 'Invalid FMU inspection response.',
      inputs: [],
      outputs: [],
      parameters: [],
    };
  }
  if (!obj || obj.success !== true) {
    const error = typeof obj?.error === 'string' ? obj.error : 'FMU inspection failed.';
    return { ok: false, error, inputs: [], outputs: [], parameters: [] };
  }
  const variables = Array.isArray(obj.variables) ? obj.variables : [];
  const inputs: FmuOutputVariable[] = [];
  const outputs: FmuOutputVariable[] = [];
  const parameters: FmuOutputVariable[] = [];
  for (const v of variables as Array<Record<string, unknown>>) {
    if (!v || !PORTABLE_FMI_TYPES.has(String(v.dataType))) {
      continue;
    }
    const entry: FmuOutputVariable = {
      name: String(v.name ?? ''),
      valueReference: Number(v.valueReference ?? 0),
      description: typeof v.description === 'string' ? v.description : '',
    };
    if (typeof v.startValue === 'number') {
      entry.startValue = v.startValue;
    }
    if (typeof v.arraySize === 'number' && v.arraySize > 1) {
      entry.arraySize = v.arraySize;
    }
    if (Array.isArray(v.dimensions)) {
      entry.dimensions = (v.dimensions as Array<Record<string, unknown>>).map((d) => {
        const dim: FmuDimension = { start: Math.max(1, Number(d?.start ?? 1)) };
        if (typeof d?.vr === 'number') dim.vr = d.vr;
        return dim;
      });
    }
    if (v.causality === 'output') {
      outputs.push(entry);
    } else if (v.causality === 'input') {
      inputs.push(entry);
    } else if (v.causality === 'parameter') {
      parameters.push(entry);
    } else if (v.causality === 'structuralParameter') {
      entry.structural = true;
      parameters.push(entry);
    }
  }
  return {
    ok: true,
    modelIdentifier: typeof obj.modelIdentifier === 'string' ? obj.modelIdentifier : undefined,
    fmiVersion: typeof obj.fmiVersion === 'string' ? obj.fmiVersion : undefined,
    coSimulation: obj.coSimulation === true,
    modelExchange: obj.modelExchange === true,
    numContinuousStates:
      typeof obj.numberOfContinuousStates === 'number' ? obj.numberOfContinuousStates : undefined,
    numEventIndicators:
      typeof obj.numberOfEventIndicators === 'number' ? obj.numberOfEventIndicators : undefined,
    iconUri: typeof obj.icon === 'string' && obj.icon ? obj.icon : undefined,
    inputs,
    outputs,
    parameters,
  };
}
//=============================================================================
// At least one output port, so the block stays connectable.
export function fmuOutputPortCount(info: FmuInspection): number {
  return info.outputs.length > 0 ? info.outputs.length : 1;
}
//=============================================================================
// Writes the inspection into the block params so the resizer can lay out one
// output port per FMU output. On failure the block is left unchanged.
export async function applyFmuInspection(
  block: FmuBlockLike,
  fmuPath: string,
  rpc: FmuInspectRpc
): Promise<FmuInspection> {
  const raw = await rpc('nflow.fmi3.inspect', { path: fmuPath });
  const info = parseFmuInspection(raw);
  if (!info.ok) {
    return info;
  }
  const params = block.params ?? (block.params = {});
  params.path = fmuPath;
  params.numInputs = info.inputs.length;
  params.inputNames = info.inputs.map((o) => o.name);
  params.inputDescriptions = info.inputs.map((o) => o.description ?? '');
  // Per-port element counts (FMI3 array variables); the engine's FMU block
  // reads outputWidths in its dimension rule to size vector output ports.
  params.inputWidths = info.inputs.map((o) => o.arraySize ?? 1);
  params.numOutputs = fmuOutputPortCount(info);
  params.outputNames = info.outputs.map((o) => o.name);
  params.outputDescriptions = info.outputs.map((o) => o.description ?? '');
  params.outputWidths = info.outputs.map((o) => o.arraySize ?? 1);
  // Per-dimension descriptors: a dimension bound to a structural parameter
  // carries the parameter's VR, so an override can resize the port
  // (recomputeFmuPortWidths). Absent for all-scalar FMUs.
  const anyDims = info.inputs.some((o) => o.dimensions) || info.outputs.some((o) => o.dimensions);
  if (anyDims) {
    params.inputDimensions = info.inputs.map((o) => o.dimensions ?? null);
    params.outputDimensions = info.outputs.map((o) => o.dimensions ?? null);
  } else {
    delete params.inputDimensions;
    delete params.outputDimensions;
  }
  // Tunable parameters (regular + structural), seeded with the FMU start values;
  // regular ones apply in initialization mode, structural ones in Configuration Mode.
  params.parameterNames = info.parameters.map((p) => p.name);
  params.parameterDescriptions = info.parameters.map((p) => p.description ?? '');
  params.parameterVrs = info.parameters.map((p) => p.valueReference);
  params.parameterStructural = info.parameters.map((p) => p.structural === true);
  const paramValues: Record<string, number> = {};
  for (const p of info.parameters) {
    if ((p.arraySize ?? 1) === 1) paramValues[p.name] = p.startValue ?? 0;
  }
  params.parameterValues = paramValues;
  // Model Exchange sizing: the fmuMe block reads these at state-layout time to
  // size its slice of the global continuous-state / zero-crossing vectors. Left
  // absent (and cleared) for a Co-Simulation FMU, which owns its own solver.
  if (typeof info.numContinuousStates === 'number') {
    params.numContinuousStates = info.numContinuousStates;
  } else {
    delete params.numContinuousStates;
  }
  if (typeof info.numEventIndicators === 'number') {
    params.numEventIndicators = info.numEventIndicators;
  } else {
    delete params.numEventIndicators;
  }
  if (info.iconUri) {
    params.iconUri = info.iconUri;
  } else {
    delete params.iconUri;
  }
  return info;
}
//=============================================================================
// A dimension bound to a structural parameter (vr) takes the overridden value; a fixed
// dimension keeps its start size. Returns true when any width changed (callers re-run
// the signal layout). No-op for FMUs without dimension data.
export function recomputeFmuPortWidths(block: FmuBlockLike): boolean {
  const params = block.params;
  if (!params) return false;
  const names = Array.isArray(params.parameterNames) ? (params.parameterNames as string[]) : [];
  const vrs = Array.isArray(params.parameterVrs) ? (params.parameterVrs as number[]) : [];
  const structural = Array.isArray(params.parameterStructural)
    ? (params.parameterStructural as boolean[])
    : [];
  const values = (params.parameterValues ?? {}) as Record<string, number>;
  const overrideByVr = new Map<number, number>();
  for (let i = 0; i < names.length; i++) {
    if (structural[i] === true && typeof vrs[i] === 'number') {
      const v = Number(values[names[i]]);
      if (Number.isFinite(v) && v >= 0) overrideByVr.set(vrs[i], Math.round(v));
    }
  }
  const widthOf = (dims: FmuDimension[] | null): number => {
    if (!Array.isArray(dims) || !dims.length) return 1;
    let w = 1;
    for (const d of dims) {
      const ext =
        typeof d.vr === 'number' && overrideByVr.has(d.vr)
          ? (overrideByVr.get(d.vr) as number)
          : d.start;
      w *= Math.max(1, ext);
    }
    return Math.max(1, w);
  };
  let changed = false;
  const apply = (dimsKey: string, widthsKey: string): void => {
    const dims = params[dimsKey];
    if (!Array.isArray(dims)) return;
    const widths = (dims as Array<FmuDimension[] | null>).map(widthOf);
    const prev = Array.isArray(params[widthsKey]) ? (params[widthsKey] as number[]) : [];
    if (widths.length !== prev.length || widths.some((w, i) => w !== prev[i])) {
      params[widthsKey] = widths;
      changed = true;
    }
  };
  apply('inputDimensions', 'inputWidths');
  apply('outputDimensions', 'outputWidths');
  return changed;
}
//=============================================================================
export interface FmuBrowseDeps {
  pickFmuFile: () => Promise<string | null>;
  rpc: FmuInspectRpc;
}
//=============================================================================
export interface FmuBrowseResult {
  canceled: boolean;
  path?: string;
  info?: FmuInspection;
}
//=============================================================================
// Picks a .fmu, inspects it and writes its ports into the block. Returns
// { canceled: true } when the picker is dismissed.
export async function browseAndInspectFmu(
  block: FmuBlockLike,
  deps: FmuBrowseDeps
): Promise<FmuBrowseResult> {
  const path = await deps.pickFmuFile();
  if (!path) {
    return { canceled: true };
  }
  const info = await applyFmuInspection(block, path, deps.rpc);
  return { canceled: false, path, info };
}
//=============================================================================
