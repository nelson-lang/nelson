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
const FILE_SINK_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kPath,
    label: 'File path',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
  },
];
//=============================================================================
const TO_WORKSPACE_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kVariableName,
    label: 'Variable name',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
    coerce: (el) => (el as HTMLInputElement).value.trim(),
  },
  {
    key: FieldNames.kMaxDataPoints,
    label: 'Limit data points to last',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
  },
  {
    key: FieldNames.kDecimation,
    label: 'Decimation',
    inputType: 'number',
    attrs: { min: '1', step: '1' },
    event: 'input',
    callUpdate: true,
    coerce: (el) => {
      const v = Number((el as HTMLInputElement).value);
      return Number.isFinite(v) && v >= 1 ? Math.floor(v) : 1;
    },
  },
  {
    key: FieldNames.kSaveFormat,
    label: 'Save format',
    inputType: 'select',
    event: 'change',
    callUpdate: true,
    options: [
      { value: 'Structure With Time', label: 'Structure With Time' },
      { value: 'Structure', label: 'Structure' },
      { value: 'Array', label: 'Array' },
    ],
  },
];
//=============================================================================
const LABEL_SINK_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kGotoTag,
    label: 'Label name',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
    coerce: (el) => (el as HTMLInputElement).value.trim(),
  },
  {
    key: FieldNames.kIsExternalPort,
    label: 'Is external port',
    inputType: 'checkbox',
    event: 'change',
    callUpdate: true,
  },
  {
    key: FieldNames.kShowNode,
    label: 'Show node',
    inputType: 'checkbox',
    event: 'change',
    callUpdate: true,
  },
];
//=============================================================================
export function renderSinks(block: Block, deps: BlockPanelDeps): boolean {
  if (block.type === 'fileSink') {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, FILE_SINK_FIELDS);
    return true;
  }

  if (block.type === 'toWorkspace') {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, TO_WORKSPACE_FIELDS);
    return true;
  }

  if (block.type === FieldNames.kLabelSink) {
    renderGenericFieldsPanel(deps.inspectorBody, block, deps, LABEL_SINK_FIELDS);
    return true;
  }

  if (block.type === 'terminator') {
    renderStaticInspectorPanel(deps.inspectorBody, 'Terminate signal');
    return true;
  }

  return false;
}
//=============================================================================
try {
  registerRenderer(renderSinks);
} catch (e) {}
//=============================================================================
