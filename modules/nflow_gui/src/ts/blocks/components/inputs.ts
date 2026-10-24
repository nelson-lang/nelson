//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/* eslint-disable @typescript-eslint/no-explicit-any */
export function labeledInput(
  edit: string,
  label: string,
  value: string | number = '',
  type = 'text',
  attrs = ''
): string {
  const v = value == null ? '' : String(value);
  return `<label class="param">${label}\n  <input type="${type}" data-edit="${edit}" value="${v}" ${attrs}>\n</label>`;
}
//=============================================================================
export function inlineCheckbox(edit: string, label: string, checked = false): string {
  return `<div class="param param-inline"><span class="param-name">${label}</span><input type="checkbox" data-edit="${edit}" ${
    checked ? 'checked' : ''
  }></div>`;
}
//=============================================================================
export function selectInput(
  edit: string,
  label: string,
  options: Array<{ value: string; label: string; selected?: boolean }>
): string {
  const opts = options
    .map((o) => `<option value="${o.value}" ${o.selected ? 'selected' : ''}>${o.label}</option>`)
    .join('');
  return `<label class="param">${label}\n  <select data-edit="${edit}">${opts}</select>\n</label>`;
}
//=============================================================================
export function colorInput(edit: string, label: string, value = '#000000'): string {
  return `<label class="param">${label}\n  <input type="color" data-edit="${edit}" value="${value}">\n</label>`;
}
//=============================================================================
