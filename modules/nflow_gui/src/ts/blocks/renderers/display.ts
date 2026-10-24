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
const DISPLAY_FIELDS: FieldDef[] = [
  { key: FieldNames.kLabel, label: 'Title', inputType: 'text', event: 'input', callUpdate: true },
  {
    key: FieldNames.kFormat,
    label: 'Format',
    inputType: 'select',
    event: 'change',
    callUpdate: true,
    options: [
      { value: 'short', label: 'Short' },
      { value: 'long', label: 'Long' },
      { value: 'short_e', label: 'Short (exp)' },
      { value: 'long_e', label: 'Long (exp)' },
      { value: 'bank', label: 'Bank (2 decimals)' },
      { value: 'hex', label: 'Hex' },
      { value: 'binary', label: 'Binary' },
      { value: 'decimal', label: 'Decimal' },
      { value: 'octal', label: 'Octal' },
    ],
  },
  {
    key: FieldNames.kDecimation,
    label: 'Decimation',
    inputType: 'number',
    attrs: { min: '1', step: '1' },
    event: 'input',
    coerce: (el) => {
      const v = Number((el as HTMLInputElement).value);
      return Number.isFinite(v) && v > 0 ? Math.max(1, Math.trunc(v)) : 1;
    },
    callUpdate: true,
  },
];
//=============================================================================
export function renderDisplay(block: Block, deps: BlockPanelDeps): boolean {
  if (block.type !== FieldNames.kDisplay) return false;

  const inspectorEl = deps.inspectorBody as HTMLElement | null;
  renderGenericFieldsPanel(inspectorEl, block, deps, DISPLAY_FIELDS, {
    [FieldNames.kDecimation]: block.params?.decimation ?? 1,
  });

  const labelInput = inspectorEl?.querySelector<HTMLInputElement>("input[data-edit='label']");
  labelInput?.addEventListener('input', () => {
    try {
      const nameInputEl = inspectorEl?.querySelector<HTMLInputElement>('.inspector-name-input');
      if (nameInputEl) nameInputEl.value = labelInput.value;
    } catch (_e) {}
  });

  return true;
}
//=============================================================================
try {
  registerRenderer(renderDisplay);
} catch (e) {}
//=============================================================================
