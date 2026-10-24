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
import type { FieldDef } from '../inspector-utils.js';
import { registerRenderer } from '../inspector-registry.js';
import { renderGenericFieldsPanel } from '../components/inspector-fields.js';
import { appendBusMemberTree } from './bus-member-tree.js';
//=============================================================================
type BlockDef = {
  fields: FieldDef[];
  values?: (block: Block) => Record<string, unknown>;
};
//=============================================================================
const minCount = (el: HTMLInputElement | HTMLSelectElement) => {
  const v = parseInt((el as HTMLInputElement).value, 10);
  return Number.isFinite(v) && v >= 2 ? v : 2;
};
//=============================================================================
// One trimmed non-empty entry per line of a textarea element.
const linesOf = (el: HTMLInputElement | HTMLSelectElement) =>
  String((el as unknown as HTMLTextAreaElement).value ?? '')
    .split('\n')
    .map((s) => s.trim())
    .filter(Boolean);
const BLOCK_DEFS: Record<string, BlockDef> = {
  mux: {
    fields: [
      {
        key: FieldNames.kSumInputs,
        label: 'Number of inputs',
        inputType: 'number',
        event: 'input',
        attrs: { min: '2', step: '1' },
        callUpdate: true,
        coerce: minCount,
      },
    ],
    values: () => ({ [FieldNames.kSumInputs]: 2 }),
  },
  demux: {
    fields: [
      {
        key: FieldNames.kMuxOutputs,
        label: 'Number of outputs',
        inputType: 'number',
        event: 'input',
        attrs: { min: '2', step: '1' },
        callUpdate: true,
        coerce: minCount,
      },
    ],
    values: () => ({ [FieldNames.kMuxOutputs]: 2 }),
  },
  multiportSwitch: {
    fields: [
      {
        key: 'DataPortCount',
        label: 'Number of data ports',
        inputType: 'number',
        event: 'input',
        attrs: { min: '1', step: '1' },
        callUpdate: true,
        coerce: (el) => {
          const v = parseInt((el as HTMLInputElement).value, 10);
          return Number.isFinite(v) && v >= 1 ? v : 3;
        },
      },
    ],
    values: () => ({ DataPortCount: 3 }),
  },
  switch: {
    fields: [
      {
        key: FieldNames.kCondition,
        label: 'Condition',
        inputType: 'select',
        event: 'change',
        callUpdate: true,
        options: [
          { value: FieldNames.kGe, label: '≥' },
          { value: FieldNames.kGt, label: '>' },
          { value: FieldNames.kNe, label: '≠' },
        ],
      },
      {
        key: FieldNames.kThreshold,
        label: 'Threshold',
        inputType: 'text',
        event: 'input',
        attrs: { step: '0.1' },
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kThreshold]: block.params[FieldNames.kThreshold] ?? 0.0,
    }),
  },
  busCreator: {
    fields: [
      {
        key: FieldNames.kMemberNames,
        label: 'Member names (one per line)',
        inputType: 'textarea',
        event: 'input',
        attrs: { rows: '4' },
        callUpdate: true,
        coerce: linesOf,
      },
      {
        key: FieldNames.kBusType,
        label: 'Bus type (optional, declared in busTypes)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
      {
        key: FieldNames.kNonVirtual,
        label: 'Nonvirtual (struct at the generated interface)',
        inputType: 'checkbox',
        event: 'change',
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kMemberNames]: Array.isArray(block.params?.[FieldNames.kMemberNames])
        ? (block.params[FieldNames.kMemberNames] as string[]).join('\n')
        : '',
      [FieldNames.kBusType]: block.params?.[FieldNames.kBusType] ?? '',
      [FieldNames.kNonVirtual]: block.params?.[FieldNames.kNonVirtual] === true,
    }),
  },
  busSelector: {
    fields: [
      {
        key: FieldNames.kSelectedSignals,
        label: 'Selected members (one path per line, e.g. sub.a)',
        inputType: 'textarea',
        event: 'input',
        attrs: { rows: '4' },
        callUpdate: true,
        coerce: linesOf,
      },
    ],
    values: (block) => ({
      [FieldNames.kSelectedSignals]: Array.isArray(block.params?.[FieldNames.kSelectedSignals])
        ? (block.params[FieldNames.kSelectedSignals] as string[]).join('\n')
        : '',
    }),
  },
  comment: {
    fields: [
      {
        key: FieldNames.kCommentText,
        label: 'Comment text',
        inputType: 'textarea',
        event: 'input',
        callUpdate: true,
        attrs: { rows: '5' },
      },
      {
        key: FieldNames.kShowBorder,
        label: 'Show border',
        inputType: 'checkbox',
        event: 'change',
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kCommentText]: block.params?.[FieldNames.kCommentText] ?? '',
      [FieldNames.kShowBorder]: block.params?.[FieldNames.kShowBorder] !== false,
    }),
  },
};
//=============================================================================
// params[key] wins over values() in the generic panel, so an array-valued
// param would render comma-joined; rewrite it as one entry per line.
const fixMultilineDisplay = (deps: BlockPanelDeps, block: Block, key: string): void => {
  const v = block.params?.[key];
  if (!Array.isArray(v)) return;
  const ta = deps.inspectorBody?.querySelector<HTMLTextAreaElement>(`textarea[data-edit='${key}']`);
  if (ta) ta.value = v.map((s) => String(s)).join('\n');
};
//=============================================================================
export function renderUtility(block: Block, deps: BlockPanelDeps): boolean {
  const type = String(block.type);
  const def = BLOCK_DEFS[type];
  if (!def) return false;

  renderGenericFieldsPanel(deps.inspectorBody, block, deps, def.fields, def.values?.(block));
  if (type === 'busCreator') fixMultilineDisplay(deps, block, FieldNames.kMemberNames);
  if (type === 'busSelector') {
    fixMultilineDisplay(deps, block, FieldNames.kSelectedSignals);
    appendBusMemberTree(block, deps);
  }
  return true;
}
//=============================================================================
try {
  registerRenderer(renderUtility);
} catch (e) {}
//=============================================================================
