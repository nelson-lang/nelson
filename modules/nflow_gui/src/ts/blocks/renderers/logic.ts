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
//=============================================================================
const OPERATOR_OPTIONS = [
  { value: 'ge', label: '>=' },
  { value: 'gt', label: '>' },
  { value: 'le', label: '<=' },
  { value: 'lt', label: '<' },
  { value: 'eq', label: '==' },
  { value: 'ne', label: '!=' },
];
//=============================================================================
export function renderLogic(block: Block, deps: BlockPanelDeps): boolean {
  if (
    block.type !== 'compareToConstant' &&
    block.type !== 'compareToZero' &&
    block.type !== 'relationalOperator'
  ) {
    return false;
  }

  // The relational block names its comparison operator `Operator`; the
  // compare-to-constant/zero blocks use `relop`. The constant block threshold is `const`.
  const opKey = block.type === 'relationalOperator' ? FieldNames.kOperator : FieldNames.kRelop;
  const operatorField: FieldDef = {
    key: opKey,
    label: 'Operator',
    inputType: 'select',
    event: 'change',
    callUpdate: true,
    options: OPERATOR_OPTIONS,
  };
  const thresholdField: FieldDef = {
    key: FieldNames.kConst,
    label: 'Threshold',
    inputType: 'text',
    event: 'input',
    attrs: { step: '0.1' },
    callUpdate: true,
  };

  const fields =
    block.type === 'compareToConstant' ? [operatorField, thresholdField] : [operatorField];
  const values: Record<string, unknown> = {
    [opKey]: block.params?.[opKey] ?? (block.type === 'compareToZero' ? 'ne' : 'ge'),
  };
  if (block.type === 'compareToConstant') {
    values[FieldNames.kConst] = block.params?.[FieldNames.kConst] ?? 0;
  }
  renderGenericFieldsPanel(deps.inspectorBody, block, deps, fields, values);
  return true;
}
//=============================================================================
try {
  registerRenderer(renderLogic);
} catch (e) {}
//=============================================================================
