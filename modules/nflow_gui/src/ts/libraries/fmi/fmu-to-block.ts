//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// FMU import assistant (editor side): turns an nflow.fmi3.inspect result into a
// one-block library manifest that wraps the FMU as a palette block. It targets
// the engine's `fmu` / `fmuMe` handler, so no new runtime is needed. Frontend
// twin of the Nelson `fmuToBlock` function.
//=============================================================================
import type { FmuInspection } from './fmu-inspect.js';
import { fmuOutputPortCount } from './fmu-inspect.js';
//=============================================================================
export interface ManifestPort {
  x: number;
  y: number;
  side: 'left' | 'right';
  name?: string;
}
//=============================================================================
export interface ManifestBlockEntry {
  type: 'fmu' | 'fmuMe';
  label: string;
  width: number;
  height: number;
  inputs: ManifestPort[];
  outputs: ManifestPort[];
  defaultParams: Record<string, unknown>;
  render: { type: 'image'; src: string };
}
//=============================================================================
export interface GeneratedLibrary {
  id: string;
  title: string;
  version: string;
  format: string;
  comment: string;
  builtin: false;
  blocks: ManifestBlockEntry[];
}
//=============================================================================
export interface FmuBlockManifestOptions {
  /** Absolute path to the .fmu (baked into the block's default params). */
  fmuPath: string;
  /** Block label / library title (default: the FMU model identifier). */
  name?: string;
  /** Library id (default: user.<sanitized name>). */
  libraryId?: string;
  /** Wrap as a Model Exchange block (fmuMe) instead of Co-Simulation (fmu). */
  modelExchange?: boolean;
}
//=============================================================================
const FMU_WIDTH = 96;
// Seeds every parameter the fmu/fmuMe renderer and engine handler read, so the
// block draws its ports and simulates without a new inspection.
export function fmuInspectionToParams(
  info: FmuInspection,
  fmuPath: string
): Record<string, unknown> {
  const params: Record<string, unknown> = {};
  params.path = fmuPath;
  params.numInputs = info.inputs.length;
  params.inputNames = info.inputs.map((o) => o.name);
  params.inputDescriptions = info.inputs.map((o) => o.description ?? '');
  params.inputWidths = info.inputs.map((o) => o.arraySize ?? 1);
  params.numOutputs = fmuOutputPortCount(info);
  params.outputNames = info.outputs.map((o) => o.name);
  params.outputDescriptions = info.outputs.map((o) => o.description ?? '');
  params.outputWidths = info.outputs.map((o) => o.arraySize ?? 1);
  params.parameterNames = info.parameters.map((p) => p.name);
  params.parameterDescriptions = info.parameters.map((p) => p.description ?? '');
  params.parameterVrs = info.parameters.map((p) => p.valueReference);
  params.parameterStructural = info.parameters.map((p) => p.structural === true);
  const paramValues: Record<string, number> = {};
  for (const p of info.parameters) {
    paramValues[p.name] = p.startValue ?? 0;
  }
  params.parameterValues = paramValues;
  if (typeof info.numContinuousStates === 'number') {
    params.numContinuousStates = info.numContinuousStates;
  }
  if (typeof info.numEventIndicators === 'number') {
    params.numEventIndicators = info.numEventIndicators;
  }
  if (info.iconUri) {
    params.iconUri = info.iconUri;
  }
  return params;
}
//=============================================================================
export function sanitizeLibraryName(name: string): string {
  return (
    name
      .trim()
      .toLowerCase()
      .replace(/[^a-z0-9_]+/g, '_')
      .replace(/^_+|_+$/g, '') || 'fmu'
  );
}
export function buildFmuBlockManifest(
  info: FmuInspection,
  opts: FmuBlockManifestOptions
): GeneratedLibrary {
  const name = (opts.name || info.modelIdentifier || 'FMU').trim() || 'FMU';
  const type: 'fmu' | 'fmuMe' = opts.modelExchange ? 'fmuMe' : 'fmu';

  const numIn = info.inputs.length;
  const numOut = fmuOutputPortCount(info);
  const rows = Math.max(numIn, numOut, 1);
  const height = Math.max(64, rows * 24);

  const inputs: ManifestPort[] = info.inputs.map((v, i) => ({
    x: 0,
    y: Math.round((i + 0.5) * (height / Math.max(numIn, 1))),
    side: 'left',
    name: v.name,
  }));
  const outputs: ManifestPort[] = info.outputs.map((v, i) => ({
    x: FMU_WIDTH,
    y: Math.round((i + 0.5) * (height / Math.max(numOut, 1))),
    side: 'right',
    name: v.name,
  }));

  const block: ManifestBlockEntry = {
    type,
    label: name,
    width: FMU_WIDTH,
    height,
    inputs,
    outputs,
    defaultParams: fmuInspectionToParams(info, opts.fmuPath),
    render: { type: 'image', src: info.iconUri ? info.iconUri : 'fmu.svg' },
  };

  const libId = opts.libraryId || `user.${sanitizeLibraryName(name)}`;
  return {
    id: libId,
    title: name,
    version: '1.0.0',
    format: 'nflow-2',
    comment: `Imported FMU block: ${name}.`,
    builtin: false,
    blocks: [block],
  };
}
//=============================================================================
