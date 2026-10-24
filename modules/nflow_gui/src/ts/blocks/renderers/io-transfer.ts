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
import type { BlockPanelDeps } from '../inspector-block.js';
import * as FieldNames from '../../FieldNames.js';
import { parseList, type FieldDef } from '../inspector-utils.js';
import { registerRenderer } from '../inspector-registry.js';
import { renderGenericFieldsPanel } from '../components/inspector-fields.js';
//=============================================================================
type BlockDef = {
  fields: FieldDef[];
  values?: (block: Block) => Record<string, unknown>;
};
//=============================================================================
const BLOCK_DEFS: Record<string, BlockDef> = {
  tf: {
    fields: [
      {
        key: FieldNames.kNum,
        label: 'Numerator (comma separated)',
        inputType: 'text',
        event: 'input',
        coerce: (el: HTMLInputElement | HTMLSelectElement) => {
          const v = parseList((el as HTMLInputElement).value);
          try {
            (el as unknown as HTMLElement).dispatchEvent(new Event('tf-changed'));
          } catch (e) {}
          return v;
        },
        callUpdate: true,
      },
      {
        key: FieldNames.kDen,
        label: 'Denominator (comma separated)',
        inputType: 'text',
        event: 'input',
        coerce: (el: HTMLInputElement | HTMLSelectElement) => {
          const v = parseList((el as HTMLInputElement).value);
          try {
            (el as unknown as HTMLElement).dispatchEvent(new Event('tf-changed'));
          } catch (e) {}
          return v;
        },
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kNum]: Array.isArray(block.params?.[FieldNames.kNum])
        ? (block.params[FieldNames.kNum] as (number | string)[]).join(', ')
        : '',
      [FieldNames.kDen]: Array.isArray(block.params?.[FieldNames.kDen])
        ? (block.params[FieldNames.kDen] as (number | string)[]).join(', ')
        : '',
    }),
  },
  gain: {
    fields: [
      { key: FieldNames.kGain, label: 'Gain', inputType: 'text', event: 'input', callUpdate: true },
    ],
  },
  sum: {
    fields: [
      {
        key: FieldNames.kSumInputs,
        label: 'Inputs',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
  },
};
//=============================================================================
export function renderIOTransfer(block: Block, deps: BlockPanelDeps): boolean {
  const { inspectorBody, signalDiagramChanged } = deps;
  const inspectorEl = inspectorBody as HTMLElement | null;
  const def = BLOCK_DEFS[String(block.type)];
  if (!def) return false;

  if (inspectorEl) {
    renderGenericFieldsPanel(inspectorEl, block, deps, def.fields, def.values?.(block));

    if (block.type === 'tf') {
      const el = inspectorEl.querySelector<HTMLInputElement>(`[data-edit="${FieldNames.kNum}"]`);
      const el2 = inspectorEl.querySelector<HTMLInputElement>(`[data-edit="${FieldNames.kDen}"]`);
      const notify = () => {
        try {
          signalDiagramChanged?.();
        } catch (e) {}
      };
      el?.addEventListener('tf-changed', notify as EventListener);
      el2?.addEventListener('tf-changed', notify as EventListener);
    }
  }

  return true;
}
//=============================================================================
try {
  registerRenderer(renderIOTransfer);
} catch (e) {}
//=============================================================================
