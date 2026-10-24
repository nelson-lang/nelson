//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Block } from '../inspector.js';
import * as FieldNames from '../../FieldNames.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import { registerRenderer } from '../inspector-registry.js';
import type { FieldDef } from '../inspector-utils.js';
import {
  renderGenericFieldsPanel,
  renderStaticInspectorPanel,
} from '../components/inspector-fields.js';
//=============================================================================
type BlockDef = {
  fields: FieldDef[];
  values?: (block: Block) => Record<string, unknown>;
};
//=============================================================================
const BLOCK_DEFS: Record<string, BlockDef> = {
  integrator: {
    fields: [
      { key: FieldNames.kInitial, label: 'Initial state', inputType: 'text', event: 'input' },
      { key: FieldNames.kMin, label: 'Integrator min', inputType: 'text', event: 'input' },
      { key: FieldNames.kMax, label: 'Integrator max', inputType: 'text', event: 'input' },
    ],
    values: (block) => ({
      [FieldNames.kInitial]: block.params?.[FieldNames.kInitial] ?? 0,
      [FieldNames.kMin]: block.params?.[FieldNames.kMin] ?? '-inf',
      [FieldNames.kMax]: block.params?.[FieldNames.kMax] ?? 'inf',
    }),
  },
  toggleSwitch: {
    fields: [
      {
        key: FieldNames.kState,
        label: 'State',
        inputType: 'checkbox',
        event: 'change',
        coerce: (el) => ((el as HTMLInputElement).checked ? 1 : 0),
        callUpdate: true,
      },
      {
        key: FieldNames.kOnLabel,
        label: 'On Label',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
      {
        key: FieldNames.kOffLabel,
        label: 'Off Label',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
      {
        key: FieldNames.kOnValue,
        label: 'On Value',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
      {
        key: FieldNames.kOffValue,
        label: 'Off Value',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kState]: block.params[FieldNames.kState] ? 1 : 0,
      [FieldNames.kOnLabel]: block.params[FieldNames.kOnLabel] ?? 'ON',
      [FieldNames.kOffLabel]: block.params[FieldNames.kOffLabel] ?? 'OFF',
      [FieldNames.kOnValue]: block.params[FieldNames.kOnValue] ?? 1,
      [FieldNames.kOffValue]: block.params[FieldNames.kOffValue] ?? 0,
    }),
  },
  saturation: {
    fields: [
      {
        key: FieldNames.kLowerLimit,
        label: 'Min',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
      {
        key: FieldNames.kUpperLimit,
        label: 'Max',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
  },
  rate: {
    fields: [
      {
        key: FieldNames.kRise,
        label: 'Max rise',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
      {
        key: FieldNames.kFall,
        label: 'Max fall',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
  },
  backlash: {
    fields: [
      {
        key: FieldNames.kBacklashWidth,
        label: 'Width',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
  },
  deadZone: {
    fields: [
      {
        key: FieldNames.kLowerValue,
        label: 'Lower limit',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
      {
        key: FieldNames.kUpperValue,
        label: 'Upper limit',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kLowerValue]: block.params?.[FieldNames.kLowerValue] ?? -1,
      [FieldNames.kUpperValue]: block.params?.[FieldNames.kUpperValue] ?? 1,
    }),
  },
  quantizer: {
    fields: [
      {
        key: FieldNames.kQuantizationInterval,
        label: 'Interval',
        inputType: 'text',
        event: 'input',
        attrs: { step: '0.1' },
        callUpdate: true,
      },
    ],
    values: () => ({ [FieldNames.kQuantizationInterval]: 1 }),
  },
  lpf: {
    fields: [
      {
        key: FieldNames.kCutoff,
        label: 'Cutoff (Hz)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  hpf: {
    fields: [
      {
        key: FieldNames.kCutoff,
        label: 'Cutoff (Hz)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  pid: {
    fields: [
      {
        key: FieldNames.kKp,
        label: 'Kp',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kKi,
        label: 'Ki',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kKd,
        label: 'Kd',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      { key: FieldNames.kMin, label: 'Integrator min', inputType: 'text', event: 'input' },
      { key: FieldNames.kMax, label: 'Integrator max', inputType: 'text', event: 'input' },
    ],
    values: (block) => ({
      [FieldNames.kMin]: block.params[FieldNames.kMin] ?? '-inf',
      [FieldNames.kMax]: block.params[FieldNames.kMax] ?? 'inf',
    }),
  },
};
//=============================================================================
export function renderControls(block: Block, deps: BlockPanelDeps): boolean {
  const def = BLOCK_DEFS[String(block.type)];
  if (!def) {
    if (block.type === 'derivative') {
      renderStaticInspectorPanel(deps.inspectorBody, 'd/dt');
      return true;
    }
    return false;
  }

  renderGenericFieldsPanel(deps.inspectorBody, block, deps, def.fields, def.values?.(block));
  return true;
}
//=============================================================================
try {
  registerRenderer(renderControls);
} catch (e) {}
//=============================================================================
