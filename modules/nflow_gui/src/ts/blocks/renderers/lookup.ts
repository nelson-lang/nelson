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
import type { FieldDef } from '../inspector-utils.js';
import { registerRenderer } from '../inspector-registry.js';
import { renderGenericFieldsPanel } from '../components/inspector-fields.js';
//=============================================================================
const INTERP_OPTIONS = [
  { value: 'Flat', label: 'Flat' },
  { value: 'Nearest', label: 'Nearest' },
  { value: 'Linear point-slope', label: 'Linear point-slope' },
  { value: 'Linear Lagrange', label: 'Linear Lagrange' },
  { value: 'Cubic spline', label: 'Cubic spline' },
  { value: 'Akima spline', label: 'Akima spline' },
];
//=============================================================================
const EXTRAP_OPTIONS = [
  { value: 'Clip', label: 'Clip' },
  { value: 'Linear', label: 'Linear' },
  { value: 'Cubic spline', label: 'Cubic spline' },
  { value: 'Akima spline', label: 'Akima spline' },
];
//=============================================================================
const DIAGNOSTIC_OPTIONS = [
  { value: 'None', label: 'None' },
  { value: 'Warning', label: 'Warning' },
  { value: 'Error', label: 'Error' },
];
//=============================================================================
const textField = (key: string, label: string): FieldDef => ({
  key,
  label,
  inputType: 'text',
  event: 'input',
  callUpdate: true,
});
//=============================================================================
const interpField: FieldDef = {
  key: 'InterpMethod',
  label: 'Interpolation',
  inputType: 'select',
  event: 'change',
  callUpdate: true,
  options: INTERP_OPTIONS,
};
//=============================================================================
const extrapField: FieldDef = {
  key: 'ExtrapMethod',
  label: 'Extrapolation',
  inputType: 'select',
  event: 'change',
  callUpdate: true,
  options: EXTRAP_OPTIONS,
};
//=============================================================================
const diagnosticField: FieldDef = {
  key: 'DiagnosticForOutOfRangeInput',
  label: 'Out-of-range action',
  inputType: 'select',
  event: 'change',
  callUpdate: true,
  options: DIAGNOSTIC_OPTIONS,
};
//=============================================================================
function asText(v: unknown, fallback: string): string {
  if (Array.isArray(v)) return v.join(' ');
  if (v === undefined || v === null) return fallback;
  return String(v);
}
//=============================================================================
export function renderLookup(block: Block, deps: BlockPanelDeps): boolean {
  const t = String(block.type);
  const p = block.params;
  let fields: FieldDef[] = [];
  const values: Record<string, unknown> = {};

  if (t === 'lookup1D') {
    fields = [
      textField('BreakpointsForDimension1', 'Breakpoints'),
      textField('Table', 'Table'),
      interpField,
      extrapField,
      diagnosticField,
    ];
    values.BreakpointsForDimension1 = asText(p.BreakpointsForDimension1, '0 1 2 3 4');
    values.Table = asText(p.Table, '0 1 4 9 16');
    values.InterpMethod = p.InterpMethod ?? 'Linear point-slope';
    values.ExtrapMethod = p.ExtrapMethod ?? 'Linear';
    values.DiagnosticForOutOfRangeInput = p.DiagnosticForOutOfRangeInput ?? 'None';
  } else if (t === 'lookup2D' || t === 'lookupND') {
    fields = [];
    if (t === 'lookupND') {
      fields.push(textField('NumberOfTableDimensions', 'Number of dimensions'));
      values.NumberOfTableDimensions = asText(p.NumberOfTableDimensions, '2');
    }
    fields.push(
      textField('BreakpointsForDimension1', 'Breakpoints (dim 1)'),
      textField('BreakpointsForDimension2', 'Breakpoints (dim 2)'),
      textField('Table', 'Table'),
      interpField,
      extrapField
    );
    values.BreakpointsForDimension1 = asText(p.BreakpointsForDimension1, '1 2 3');
    values.BreakpointsForDimension2 = asText(p.BreakpointsForDimension2, '1 2 3');
    values.Table = asText(p.Table, '4 5 6 5 7 8 6 8 10');
    values.InterpMethod = p.InterpMethod ?? 'Linear point-slope';
    values.ExtrapMethod = p.ExtrapMethod ?? 'Linear';
  } else if (t === 'directLookup') {
    fields = [
      textField('NumberOfTableDimensions', 'Number of dimensions'),
      textField('TableDimensions', 'Table dimensions'),
      textField('Table', 'Table'),
    ];
    values.NumberOfTableDimensions = asText(p.NumberOfTableDimensions, '2');
    values.TableDimensions = asText(p.TableDimensions, '2 3');
    values.Table = asText(p.Table, '0 1 10 11 20 21');
  } else {
    return false;
  }

  renderGenericFieldsPanel(deps.inspectorBody, block, deps, fields, values);
  return true;
}
//=============================================================================
try {
  registerRenderer(renderLookup);
} catch (e) {}
//=============================================================================
