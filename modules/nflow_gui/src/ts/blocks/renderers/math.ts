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
import {
  renderGenericFieldsPanel,
  renderStaticInspectorPanel,
} from '../components/inspector-fields.js';
//=============================================================================
const STATIC_PANELS: Record<string, string> = {
  mult: 'Multiply inputs',
  divide: 'Divide input 1 by input 2',
  negate: 'Negate input',
};
//=============================================================================
const BIAS_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kBias,
    label: 'Bias',
    inputType: 'text',
    event: 'input',
    attrs: { step: '0.1' },
    callUpdate: true,
  },
];
//=============================================================================
const USER_FUNC_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kExpr,
    label: 'Function',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
  },
];
//=============================================================================
const MATH_FUNCTION_OPTIONS = [
  { value: 'exp', label: 'exp' },
  { value: 'log', label: 'log' },
  { value: '10^u', label: '10^u' },
  { value: 'log10', label: 'log10' },
  { value: 'square', label: 'square' },
  { value: 'sqrt', label: 'sqrt' },
  { value: 'reciprocal', label: 'reciprocal' },
  { value: 'pow', label: 'pow (2 inputs)' },
  { value: 'hypot', label: 'hypot (2 inputs)' },
  { value: 'rem', label: 'rem (2 inputs)' },
  { value: 'mod', label: 'mod (2 inputs)' },
];
//=============================================================================
const MATH_FUNCTION_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kFunction,
    label: 'Function',
    inputType: 'select',
    event: 'change',
    callUpdate: true,
    options: MATH_FUNCTION_OPTIONS,
  },
];
//=============================================================================
const TRIG_FUNCTION_OPTIONS = [
  { value: 'sin', label: 'sin' },
  { value: 'cos', label: 'cos' },
  { value: 'tan', label: 'tan' },
  { value: 'asin', label: 'asin' },
  { value: 'acos', label: 'acos' },
  { value: 'atan', label: 'atan' },
  { value: 'sinh', label: 'sinh' },
  { value: 'cosh', label: 'cosh' },
  { value: 'tanh', label: 'tanh' },
  { value: 'asinh', label: 'asinh' },
  { value: 'acosh', label: 'acosh' },
  { value: 'atanh', label: 'atanh' },
  { value: 'atan2', label: 'atan2 (2 inputs)' },
  { value: 'sincos', label: 'sincos (2 outputs)' },
];
//=============================================================================
const TRIG_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kFunction,
    label: 'Function',
    inputType: 'select',
    event: 'change',
    callUpdate: true,
    options: TRIG_FUNCTION_OPTIONS,
  },
];
//=============================================================================
const ROUNDING_OPERATOR_OPTIONS = [
  { value: 'floor', label: 'floor' },
  { value: 'ceil', label: 'ceil' },
  { value: 'round', label: 'round' },
  { value: 'fix', label: 'fix' },
];
//=============================================================================
const ROUNDING_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kOperator,
    label: 'Operator',
    inputType: 'select',
    event: 'change',
    callUpdate: true,
    options: ROUNDING_OPERATOR_OPTIONS,
  },
];
//=============================================================================
const SQRT_FUNCTION_OPTIONS = [
  { value: 'sqrt', label: 'sqrt' },
  { value: 'signedSqrt', label: 'signedSqrt' },
  { value: 'rSqrt', label: 'rSqrt' },
];
//=============================================================================
const SQRT_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kFunction,
    label: 'Function',
    inputType: 'select',
    event: 'change',
    callUpdate: true,
    options: SQRT_FUNCTION_OPTIONS,
  },
];
//=============================================================================
export function renderMath(block: Block, deps: BlockPanelDeps): boolean {
  const text = STATIC_PANELS[String(block.type)];
  if (text) {
    renderStaticInspectorPanel(deps.inspectorBody, text);
    return true;
  }

  if (block.type === 'bias') {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, BIAS_FIELDS, {
      [FieldNames.kBias]: block.params?.[FieldNames.kBias] ?? 1,
    });
    return true;
  }

  if (block.type === 'expression') {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, USER_FUNC_FIELDS, {
      [FieldNames.kExpr]: block.params?.[FieldNames.kExpr] ?? 'u',
    });
    return true;
  }

  if (block.type === 'mathFunction') {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, MATH_FUNCTION_FIELDS, {
      [FieldNames.kFunction]: block.params?.[FieldNames.kFunction] ?? 'exp',
    });
    return true;
  }

  if (block.type === 'trigFunction') {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, TRIG_FIELDS, {
      [FieldNames.kFunction]: block.params?.[FieldNames.kFunction] ?? 'sin',
    });
    return true;
  }

  if (block.type === 'roundingFunction') {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, ROUNDING_FIELDS, {
      [FieldNames.kOperator]: block.params?.[FieldNames.kOperator] ?? 'floor',
    });
    return true;
  }

  if (block.type === 'sqrt') {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, SQRT_FIELDS, {
      [FieldNames.kFunction]: block.params?.[FieldNames.kFunction] ?? 'sqrt',
    });
    return true;
  }

  return false;
}
//=============================================================================
try {
  registerRenderer(renderMath);
} catch (e) {}
//=============================================================================
