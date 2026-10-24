//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
import type { Block } from '../inspector.js';
import type { BlockPanelDeps } from '../inspector-block.js';
import { bindGenericInputs, type FieldDef } from '../inspector-utils.js';
import { renderInspectorReact } from '../inspector-react-root.js';
//=============================================================================
type FieldValues = Record<string, unknown>;
function displayValue(value: unknown): string {
  return value == null ? '' : String(value);
}
//=============================================================================
function valueFor(block: Block, field: FieldDef, values?: FieldValues): unknown {
  return (block.params || {})[field.key] ?? values?.[field.key];
}
//=============================================================================
function InspectorField({ field, value }: { field: FieldDef; value: unknown }): JSX.Element {
  const attrs = field.attrs || {};

  if (field.inputType === 'textarea') {
    return (
      <label className="param param-stack">
        <span className="param-name">{field.label}</span>
        <textarea data-edit={field.key} defaultValue={displayValue(value)} {...attrs} />
      </label>
    );
  }

  if (field.inputType === 'checkbox') {
    return (
      <div className="param param-inline">
        <span className="param-name">{field.label}</span>
        <input type="checkbox" data-edit={field.key} defaultChecked={Boolean(value)} {...attrs} />
      </div>
    );
  }

  if (field.inputType === 'select') {
    return (
      <label className="param">
        {field.label}
        <select data-edit={field.key} defaultValue={displayValue(value)} {...attrs}>
          {(field.options || []).map((option) => (
            <option key={option.value} value={option.value}>
              {option.label}
            </option>
          ))}
        </select>
      </label>
    );
  }

  const inputType = field.inputType || 'text';
  return (
    <label className="param">
      {field.label}
      <input type={inputType} data-edit={field.key} defaultValue={displayValue(value)} {...attrs} />
    </label>
  );
}
//=============================================================================
function InspectorFields({
  block,
  fields,
  values,
  after,
}: {
  block: Block;
  fields: FieldDef[];
  values?: FieldValues;
  after?: JSX.Element;
}): JSX.Element {
  return (
    <>
      {fields.map((field) => (
        <InspectorField key={field.key} field={field} value={valueFor(block, field, values)} />
      ))}
      {after}
    </>
  );
}
//=============================================================================
export function renderGenericFieldsPanel(
  inspectorBody: HTMLElement | null,
  block: Block,
  deps: BlockPanelDeps,
  fields: FieldDef[],
  values?: FieldValues,
  after?: JSX.Element
): void {
  renderInspectorReact(
    inspectorBody,
    <InspectorFields block={block} fields={fields} values={values} after={after} />
  );
  bindGenericInputs(inspectorBody, block, deps, fields);
}
//=============================================================================
export function renderStaticInspectorPanel(inspectorBody: HTMLElement | null, text: string): void {
  renderInspectorReact(inspectorBody, <div className="param">{text}</div>);
}
//=============================================================================
