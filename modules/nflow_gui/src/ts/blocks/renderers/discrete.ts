//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import * as FieldNames from '../../FieldNames.js';
import { renderGenericFieldsPanel } from '../components/inspector-fields.js';
import { registerRenderer } from '../inspector-registry.js';
import { parseList, type FieldDef } from '../inspector-utils.js';
//=============================================================================
type BlockDef = {
  fields: FieldDef[];
  values?: (block: Block) => Record<string, unknown>;
  notifyKeys?: string[];
};
//=============================================================================
function listValue(value: unknown): string {
  return Array.isArray(value) ? (value as (number | string)[]).join(', ') : '';
}
//=============================================================================
const BLOCK_DEFS: Record<string, BlockDef> = {
  dtf: {
    fields: [
      {
        key: FieldNames.kNum,
        label: 'Numerator (comma separated)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        coerce: (el) => parseList((el as HTMLInputElement).value),
      },
      {
        key: FieldNames.kDen,
        label: 'Denominator (comma separated)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        coerce: (el) => parseList((el as HTMLInputElement).value),
      },
      {
        key: FieldNames.kTs,
        label: 'Sample time (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kNum]: listValue(block.params?.[FieldNames.kNum]),
      [FieldNames.kDen]: listValue(block.params?.[FieldNames.kDen]),
      [FieldNames.kTs]: block.params[FieldNames.kTs] ?? '',
    }),
    notifyKeys: [FieldNames.kNum, FieldNames.kDen, FieldNames.kTs],
  },
  ddelay: {
    fields: [
      {
        key: FieldNames.kSteps,
        label: 'Steps',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
      {
        key: FieldNames.kTs,
        label: 'Sample time (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kSteps]: block.params[FieldNames.kSteps] ?? '',
      [FieldNames.kTs]: block.params[FieldNames.kTs] ?? '',
    }),
    notifyKeys: [FieldNames.kSteps, FieldNames.kTs],
  },
  unitDelay: {
    fields: [
      {
        key: FieldNames.kInitial,
        label: 'Initial value',
        inputType: 'text',
        event: 'input',
        attrs: { step: '0.1' },
        callUpdate: true,
      },
    ],
    values: () => ({ [FieldNames.kInitial]: 0 }),
  },
  difference: {
    fields: [
      {
        key: FieldNames.kICPrevInput,
        label: 'Initial value',
        inputType: 'text',
        event: 'input',
        attrs: { step: '0.1' },
        callUpdate: true,
      },
    ],
    values: () => ({ [FieldNames.kICPrevInput]: 0 }),
  },
  zoh: {
    fields: [
      {
        key: FieldNames.kTs,
        label: 'Sample time (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
    values: (block) => ({ [FieldNames.kTs]: block.params[FieldNames.kTs] ?? '' }),
    notifyKeys: [FieldNames.kTs],
  },
  foh: {
    fields: [
      {
        key: FieldNames.kTs,
        label: 'Sample time (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
    values: (block) => ({ [FieldNames.kTs]: block.params[FieldNames.kTs] ?? '' }),
    notifyKeys: [FieldNames.kTs],
  },
  dstateSpace: {
    fields: [
      { key: 'A', label: 'A', inputType: 'text', event: 'input' },
      { key: 'B', label: 'B', inputType: 'text', event: 'input' },
      { key: 'C', label: 'C', inputType: 'text', event: 'input' },
      { key: 'D', label: 'D', inputType: 'text', event: 'input' },
      { key: FieldNames.kTs, label: 'Sample time (s)', inputType: 'text', event: 'input' },
    ],
    values: (block) => ({
      A: block.params.A ?? '',
      B: block.params.B ?? '',
      C: block.params.C ?? '',
      D: block.params.D ?? '',
      [FieldNames.kTs]: block.params[FieldNames.kTs] ?? '',
    }),
  },
};
//=============================================================================
export function renderDiscrete(block: Block, deps: BlockPanelDeps): boolean {
  const def = BLOCK_DEFS[String(block.type)];
  if (!def) return false;

  renderGenericFieldsPanel(deps.inspectorBody, block, deps, def.fields, def.values?.(block));

  if (def.notifyKeys?.length && deps.inspectorBody) {
    const selector = def.notifyKeys.map((key) => `[data-edit='${key}']`).join(',');
    deps.inspectorBody.querySelectorAll<HTMLInputElement>(selector).forEach((el) => {
      el.addEventListener('input', () => deps.signalDiagramChanged?.());
    });
  }

  return true;
}
//=============================================================================
try {
  registerRenderer(renderDiscrete);
} catch (_e) {}
//=============================================================================
