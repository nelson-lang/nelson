//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { createElement } from 'react';
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import * as FieldNames from '../../FieldNames.js';
import type { FieldDef } from '../inspector-utils.js';
import { registerRenderer } from '../inspector-registry.js';
import { renderGenericFieldsPanel } from '../components/inspector-fields.js';
//=============================================================================
const NELSON_FUNCTION_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kFcn,
    label: 'Function',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
    coerce: (el) => (el as HTMLInputElement).value.trim(),
  },
  {
    key: FieldNames.kOutputDims,
    label: 'Output dimensions (-1 = same as input)',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
  },
];
//=============================================================================
export function renderUserDefined(block: Block, deps: BlockPanelDeps): boolean {
  if (block.type !== 'nelsonFunction') return false;
  renderGenericFieldsPanel(
    deps.inspectorBody,
    block,
    deps,
    NELSON_FUNCTION_FIELDS,
    undefined,
    createElement(
      'div',
      { className: 'param' },
      'Function name, @(u) expression, or expression using u. Simulation only: code generation is not supported for this block.'
    )
  );
  return true;
}
//=============================================================================
try {
  registerRenderer(renderUserDefined);
} catch (e) {}
//=============================================================================
