//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, it, expect } from 'vitest';
import type { FmuInspection } from './fmu-inspect.js';
import {
  buildFmuBlockManifest,
  fmuInspectionToParams,
  sanitizeLibraryName,
} from './fmu-to-block.js';
//=============================================================================
function inspection(overrides: Partial<FmuInspection> = {}): FmuInspection {
  return {
    ok: true,
    modelIdentifier: 'VanDerPol',
    fmiVersion: '2.0',
    coSimulation: true,
    modelExchange: false,
    inputs: [],
    outputs: [
      { name: 'x0', valueReference: 1 },
      { name: 'x1', valueReference: 2 },
    ],
    parameters: [{ name: 'mu', valueReference: 3, startValue: 1 }],
    ...overrides,
  };
}
//=============================================================================
describe('buildFmuBlockManifest', () => {
  it('wraps an FMU as a one-block library with named ports', () => {
    const lib = buildFmuBlockManifest(inspection(), { fmuPath: 'C:/x/VanDerPol.fmu' });
    expect(lib.blocks).toHaveLength(1);
    const b = lib.blocks[0];
    expect(b.type).toBe('fmu');
    expect(b.label).toBe('VanDerPol');
    expect(b.inputs).toHaveLength(0);
    expect(b.outputs).toHaveLength(2);
    expect(b.outputs.map((o) => o.name)).toEqual(['x0', 'x1']);
    expect(b.outputs.every((o) => o.side === 'right')).toBe(true);
    expect(b.defaultParams.path).toBe('C:/x/VanDerPol.fmu');
    expect(lib.id).toBe('user.vanderpol');
    expect(lib.builtin).toBe(false);
  });

  it('seeds the params the fmu block needs to render and simulate', () => {
    const p = fmuInspectionToParams(inspection(), 'C:/x/VanDerPol.fmu');
    expect(p.numOutputs).toBe(2);
    expect(p.outputNames).toEqual(['x0', 'x1']);
    expect(p.numInputs).toBe(0);
    expect(p.parameterNames).toEqual(['mu']);
    expect((p.parameterValues as Record<string, number>).mu).toBe(1);
  });

  it('honours a custom name and model-exchange flag', () => {
    const lib = buildFmuBlockManifest(inspection(), {
      fmuPath: '/p/m.fmu',
      name: 'My Motor',
      modelExchange: true,
    });
    expect(lib.blocks[0].type).toBe('fmuMe');
    expect(lib.blocks[0].label).toBe('My Motor');
    expect(lib.id).toBe('user.my_motor');
  });

  it('uses the FMU icon when present, else the generic badge', () => {
    const withIcon = buildFmuBlockManifest(inspection({ iconUri: 'data:image/png;base64,AAAA' }), {
      fmuPath: '/p/m.fmu',
    });
    expect(withIcon.blocks[0].render.src).toBe('data:image/png;base64,AAAA');
    const noIcon = buildFmuBlockManifest(inspection(), { fmuPath: '/p/m.fmu' });
    expect(noIcon.blocks[0].render.src).toBe('fmu.svg');
  });

  it('inputs get left-side ports', () => {
    const lib = buildFmuBlockManifest(inspection({ inputs: [{ name: 'u', valueReference: 9 }] }), {
      fmuPath: '/p/m.fmu',
    });
    expect(lib.blocks[0].inputs).toHaveLength(1);
    expect(lib.blocks[0].inputs[0].side).toBe('left');
    expect(lib.blocks[0].inputs[0].name).toBe('u');
  });
});
//=============================================================================
describe('sanitizeLibraryName', () => {
  it('lowercases and replaces non-alphanumerics', () => {
    expect(sanitizeLibraryName('My FMU 2.0!')).toBe('my_fmu_2_0');
    expect(sanitizeLibraryName('   ')).toBe('fmu');
  });
});
//=============================================================================
