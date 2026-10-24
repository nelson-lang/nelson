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
import * as FieldNames from '../../FieldNames.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import { registerRenderer } from '../inspector-registry.js';
import type { FieldDef } from '../inspector-utils.js';
import { renderGenericFieldsPanel } from '../components/inspector-fields.js';
//=============================================================================
type BlockDef = {
  fields: FieldDef[];
  values?: (block: Block) => Record<string, unknown>;
  after?: (block: Block) => JSX.Element;
};
//=============================================================================
const FILE_SOURCE_FIELDS: FieldDef[] = [
  {
    key: FieldNames.kPath,
    label: 'File path',
    inputType: 'text',
    event: 'input',
    callUpdate: true,
  },
];
//=============================================================================
export function parseFileSourceCsvText(text: string): { times: number[]; values: number[] } {
  const rows = text
    .split(/\r?\n/)
    .map((line) => line.trim())
    .filter((line) => line.length > 0);
  const times: number[] = [];
  const values: number[] = [];
  rows.forEach((line, idx) => {
    const cols = line
      .split(/[,\t]/)
      .map((v) => v.trim())
      .filter((v) => v.length > 0);
    const nums = cols.map((v) => Number(v)).filter((v) => Number.isFinite(v));
    if (nums.length === 0) return;
    if (nums.length >= 2) {
      times.push(nums[0]);
      values.push(nums[1]);
    } else {
      times.push(idx);
      values.push(nums[0]);
    }
  });
  const pairs = times.map((t, i) => ({ t, v: values[i] }));
  pairs.sort((a, b) => a.t - b.t);
  return {
    times: pairs.map((p) => p.t),
    values: pairs.map((p) => p.v),
  };
}
//=============================================================================
function fileSourceAfter(block: Block): JSX.Element {
  return createElement(
    'span',
    null,
    createElement(
      'label',
      { className: 'param' },
      'CSV file ',
      createElement('input', {
        type: 'file',
        'data-edit': 'file',
        accept: '.csv,text/csv',
      })
    ),
    createElement(
      'div',
      { className: 'param' },
      block.params?.loaded ? 'Loaded CSV' : 'No CSV loaded'
    )
  );
}
//=============================================================================
function bindFileSourceInput(block: Block, deps: BlockPanelDeps): void {
  const inspectorEl = deps.inspectorBody;
  const fileInput = inspectorEl?.querySelector<HTMLInputElement>("input[data-edit='file']");
  if (!fileInput) return;

  fileInput.addEventListener('change', () => {
    const file = fileInput.files?.[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = () => {
      const parsed = parseFileSourceCsvText(String(reader.result || ''));
      if (!block.params) block.params = {};
      block.params.times = parsed.times;
      block.params.values = parsed.values;
      block.params.loaded = parsed.times.length > 0;
      deps.callUpdate(block);
      deps.renderInspector(block);
    };
    reader.readAsText(file);
  });
}
//=============================================================================
const BLOCK_DEFS: Record<string, BlockDef> = {
  constant: {
    fields: [
      {
        key: FieldNames.kValue,
        label: 'Value',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  step: {
    fields: [
      {
        key: FieldNames.kStepTime,
        label: 'Step time (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  ramp: {
    fields: [
      {
        key: FieldNames.kSlope,
        label: 'Slope',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kStart,
        label: 'Start time (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  impulse: {
    fields: [
      {
        key: FieldNames.kTime,
        label: 'Time (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kAmp,
        label: 'Amplitude',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  sine: {
    fields: [
      {
        key: FieldNames.kAmp,
        label: 'Amplitude',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kFreq,
        label: 'Frequency (Hz)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kPhase,
        label: 'Phase (rad)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  chirp: {
    fields: [
      {
        key: FieldNames.kAmp,
        label: 'Amplitude',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kF0,
        label: 'Start freq (Hz)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kF1,
        label: 'End freq (Hz)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
      {
        key: FieldNames.kT1,
        label: 'Duration (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  noise: {
    fields: [
      {
        key: FieldNames.kAmp,
        label: 'Amplitude',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        attrs: { step: '0.1' },
      },
    ],
  },
  clock: {
    fields: [
      {
        key: FieldNames.kDisplayTime,
        label: 'Display time',
        inputType: 'checkbox',
        event: 'change',
        callUpdate: true,
      },
      {
        key: FieldNames.kDecimation,
        label: 'Decimation',
        inputType: 'number',
        attrs: { min: '1', step: '1' },
        event: 'input',
        coerce: (el) => {
          const v = Number((el as HTMLInputElement).value);
          return Number.isFinite(v) && v > 0 ? Math.max(1, Math.trunc(v)) : 10;
        },
        callUpdate: true,
      },
    ],
    values: (block) => ({
      [FieldNames.kDisplayTime]: block.params?.[FieldNames.kDisplayTime] ?? false,
      [FieldNames.kDecimation]: block.params?.decimation ?? 10,
    }),
    after: () =>
      createElement('div', { className: 'param' }, 'Outputs simulation time in seconds.'),
  },
  fileSource: {
    fields: FILE_SOURCE_FIELDS,
    after: fileSourceAfter,
  },
  fromWorkspace: {
    fields: [
      {
        key: FieldNames.kVariableName,
        label: 'Variable name',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
        coerce: (el) => (el as HTMLInputElement).value.trim(),
      },
      {
        key: FieldNames.kInterpolate,
        label: 'Interpolate data',
        inputType: 'select',
        event: 'change',
        callUpdate: true,
        options: [
          { value: 'on', label: 'on' },
          { value: 'off', label: 'off' },
        ],
      },
      {
        key: FieldNames.kOutputAfterFinalValue,
        label: 'Form output after final data value by',
        inputType: 'select',
        event: 'change',
        callUpdate: true,
        options: [
          { value: 'Extrapolation', label: 'Extrapolation' },
          { value: 'Setting to zero', label: 'Setting to zero' },
          { value: 'Holding final value', label: 'Holding final value' },
        ],
      },
      {
        key: FieldNames.kTs,
        label: 'Sample time (s)',
        inputType: 'text',
        event: 'input',
        callUpdate: true,
      },
    ],
    after: () =>
      createElement(
        'div',
        { className: 'param' },
        'Reads a [time, values] matrix or a struct with time/signals fields from the workspace.'
      ),
  },
  [FieldNames.kLabelSource]: {
    fields: [
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
    ],
  },
};
//=============================================================================
export function renderGenerators(block: Block, deps: BlockPanelDeps): boolean {
  const def = BLOCK_DEFS[String(block.type)];
  if (!def) return false;

  renderGenericFieldsPanel(
    deps.inspectorBody,
    block,
    deps,
    def.fields,
    def.values?.(block),
    def.after?.(block)
  );
  if (block.type === 'fileSource') bindFileSourceInput(block, deps);
  return true;
}
//=============================================================================
try {
  registerRenderer(renderGenerators);
} catch (e) {}
//=============================================================================
