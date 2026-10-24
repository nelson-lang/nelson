//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { fieldToHTML, bindGenericInputs, FieldDef } from './inspector-utils';
//=============================================================================
describe('fieldToHTML', () => {
  test('renders inputs correctly', () => {
    const checkbox: FieldDef = { key: 'chk', label: 'Check', inputType: 'checkbox' };
    const sel: FieldDef = {
      key: 'sel',
      label: 'Sel',
      inputType: 'select',
      options: [
        { value: 'a', label: 'A' },
        { value: 'b', label: 'B' },
      ],
    };
    const color: FieldDef = { key: 'bg', label: 'Bg', inputType: 'color' };
    const num: FieldDef = {
      key: 'n',
      label: 'N',
      inputType: 'number',
      attrs: { min: 1, step: 0.5 },
    };
    const txt: FieldDef = { key: 't', label: 'T', inputType: 'text' };

    const htmlChk = fieldToHTML(checkbox, true);
    expect(htmlChk).toContain('type="checkbox"');
    expect(htmlChk).toContain('data-edit="chk"');

    const htmlSel = fieldToHTML(sel, 'b');
    expect(htmlSel).toContain('<select');
    expect(htmlSel).toContain('option value="b"');
    expect(htmlSel).toContain('selected');

    const htmlColor = fieldToHTML(color, '#ff0000');
    expect(htmlColor).toContain('type="color"');
    expect(htmlColor).toContain('value="#ff0000"');

    const htmlNum = fieldToHTML(num, 5);
    expect(htmlNum).toContain('type="number"');
    expect(htmlNum).toContain('min="1"');

    const htmlTxt = fieldToHTML(txt, 'hello');
    expect(htmlTxt).toContain('type="text"');
  });
});
//=============================================================================
describe('bindGenericInputs', () => {
  test('updates block.params and calls callUpdate when inputs change', () => {
    const defs: FieldDef[] = [
      { key: 'a', label: 'A', inputType: 'text', event: 'input', callUpdate: true },
      { key: 'b', label: 'B', inputType: 'checkbox', event: 'change', callUpdate: true },
      {
        key: 'c',
        label: 'C',
        inputType: 'select',
        options: [
          { value: '1', label: 'One' },
          { value: '2', label: 'Two' },
        ],
        event: 'change',
      },
    ];

    const container = document.createElement('div');
    container.innerHTML =
      fieldToHTML(defs[0], 'x') + fieldToHTML(defs[1], false) + fieldToHTML(defs[2], '2');

    const block: any = { params: {} };
    const updates: any[] = [];
    const deps: any = { callUpdate: (b: any) => updates.push(b) };

    bindGenericInputs(container, block, deps, defs);

    const inputA = container.querySelector('input[data-edit="a"]') as HTMLInputElement;
    inputA.value = 'changed';
    inputA.dispatchEvent(new Event('input', { bubbles: true }));

    const inputB = container.querySelector('input[data-edit="b"]') as HTMLInputElement;
    inputB.checked = true;
    inputB.dispatchEvent(new Event('change', { bubbles: true }));

    const selectC = container.querySelector('select[data-edit="c"]') as HTMLSelectElement;
    selectC.value = '1';
    selectC.dispatchEvent(new Event('change', { bubbles: true }));

    expect(block.params.a).toBe('changed');
    expect(block.params.b).toBe(true);
    expect(block.params.c).toBe('1');
    expect(updates.length).toBeGreaterThanOrEqual(1);
  });
});
//=============================================================================
