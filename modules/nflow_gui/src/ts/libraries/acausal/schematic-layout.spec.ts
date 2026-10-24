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
import {
  computeSchematicLayout,
  type SchematicBlock,
  type SchematicModel,
  type PinSide,
} from './schematic-layout';
//=============================================================================
const twoTerm = (id: string, type: string, domain = 'electrical'): SchematicBlock => ({
  id,
  type,
  acausal: true,
  width: 100,
  height: 80,
  pins: [
    { name: 'p', side: 'left' as PinSide, domain },
    { name: 'n', side: 'right' as PinSide, domain },
  ],
  hasSignalOut: false,
  hasSignalIn: false,
});
//=============================================================================
const oneTerm = (id: string, type: string, name = 'p', domain = 'electrical'): SchematicBlock => ({
  id,
  type,
  acausal: true,
  width: 100,
  height: 80,
  pins: [{ name, side: 'left' as PinSide, domain }],
  hasSignalOut: false,
  hasSignalIn: false,
});
//=============================================================================
// Series RLC loop: V1 -- R1 -- L1 -- C1 -- back to ground.
const rlc = (): SchematicModel => ({
  blocks: [
    twoTerm('V1', 'ConstantVoltage'),
    twoTerm('R1', 'Resistor'),
    twoTerm('L1', 'Inductor'),
    twoTerm('C1', 'Capacitor'),
    oneTerm('G', 'Ground'),
  ],
  physicalWires: [
    { pins: ['V1.p', 'R1.p'] },
    { pins: ['R1.n', 'L1.p'] },
    { pins: ['L1.n', 'C1.p'] },
    { pins: ['C1.n', 'V1.n', 'G.p'] },
  ],
  connections: [],
});
//=============================================================================
describe('computeSchematicLayout', () => {
  it('places every block', () => {
    const pos = computeSchematicLayout(rlc());
    for (const id of ['V1', 'R1', 'L1', 'C1', 'G']) expect(pos.has(id)).toBe(true);
  });

  it('stands the voltage source vertical on the far left', () => {
    const pos = computeSchematicLayout(rlc());
    const v1 = pos.get('V1')!;
    expect(v1.rotation).toBe(90);
    const xs = ['R1', 'L1', 'C1'].map((id) => pos.get(id)!.x);
    expect(v1.x).toBeLessThan(Math.min(...xs));
  });

  it('lays the series R and L horizontally on a common rail', () => {
    const pos = computeSchematicLayout(rlc());
    const r1 = pos.get('R1')!;
    const l1 = pos.get('L1')!;
    expect(r1.rotation).toBe(0);
    expect(l1.rotation).toBe(0);
    expect(r1.y).toBe(l1.y); // same rail row
    expect(l1.x).toBeGreaterThan(r1.x); // L after R
  });

  it('drops the shunt capacitor vertically', () => {
    const pos = computeSchematicLayout(rlc());
    expect(pos.get('C1')!.rotation).toBe(90);
  });

  it('puts ground at the bottom, below the rail', () => {
    const pos = computeSchematicLayout(rlc());
    const g = pos.get('G')!;
    const rail = pos.get('R1')!.y;
    expect(g.y).toBeGreaterThan(rail);
  });

  it('snaps all coordinates to the grid', () => {
    const pos = computeSchematicLayout(rlc());
    for (const p of pos.values()) {
      expect(p.x % 10).toBe(0);
      expect(p.y % 10).toBe(0);
    }
  });

  it('never stacks two blocks on the exact same point', () => {
    const pos = computeSchematicLayout(rlc());
    const seen = new Set<string>();
    for (const p of pos.values()) {
      const key = `${p.x},${p.y}`;
      expect(seen.has(key)).toBe(false);
      seen.add(key);
    }
  });

  it('keeps a signal sink (scope) off to the right of the circuit', () => {
    const model = rlc();
    model.blocks.push({
      id: 'sc',
      type: 'scope',
      acausal: false,
      width: 320,
      height: 220,
      pins: [],
      hasSignalOut: false,
      hasSignalIn: true,
    });
    const pos = computeSchematicLayout(model);
    const sc = pos.get('sc')!;
    const circuitRight = Math.max(...['R1', 'L1', 'C1'].map((id) => pos.get(id)!.x));
    expect(sc.x).toBeGreaterThan(circuitRight);
  });

  it('orients an EMF as a vertical electromechanical boundary', () => {
    const model: SchematicModel = {
      blocks: [
        twoTerm('V1', 'ConstantVoltage'),
        twoTerm('R1', 'Resistor'),
        {
          id: 'M',
          type: 'EMF',
          acausal: true,
          width: 100,
          height: 80,
          pins: [
            { name: 'p', side: 'left', domain: 'electrical' },
            { name: 'n', side: 'right', domain: 'electrical' },
            { name: 'flange', side: 'top', domain: 'rotational' },
          ],
          hasSignalOut: false,
          hasSignalIn: false,
        },
        oneTerm('J1', 'Inertia', 'flange', 'rotational'),
        oneTerm('G', 'Ground'),
      ],
      physicalWires: [
        { pins: ['V1.p', 'R1.p'] },
        { pins: ['R1.n', 'M.p'] },
        { pins: ['M.n', 'V1.n', 'G.p'] },
        { pins: ['M.flange', 'J1.flange'] },
      ],
      connections: [],
    };
    const pos = computeSchematicLayout(model);
    expect(pos.get('M')!.rotation).toBe(90);
  });
});
//=============================================================================
