//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Non-regression test: renderBlockPanel must set a `name` attribute on every
// form field that carries a `data-edit` attribute so that browsers can
// identify form fields (fixes DevTools "form field has no id or name" warning).
//=============================================================================
import { renderBlockPanel } from './inspector-block';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
function makeContainer() {
  const el = document.createElement('div');
  return el;
}
//=============================================================================
function makeDeps(inspectorBody: HTMLElement) {
  return {
    inspectorBody,
    callUpdate: () => {},
    callResize: () => {},
    renderInspector: () => {},
  } as any;
}
//=============================================================================
/** Assert that every [data-edit] field inside `container` has a `name` attr. */
function expectAllFieldsHaveName(container: HTMLElement) {
  const fields = Array.from(
    container.querySelectorAll<HTMLElement>(
      'input[data-edit], select[data-edit], textarea[data-edit]'
    )
  );
  expect(fields.length).toBeGreaterThan(0);
  fields.forEach((el) => {
    expect(el.getAttribute('name')).toBeTruthy();
  });
}
//=============================================================================
test('constant block: all data-edit fields have a name attribute', () => {
  const body = makeContainer();
  const block: any = { type: 'constant', params: { Value: '1' } };
  renderBlockPanel(block, makeDeps(body));
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('step block: all data-edit fields have a name attribute', () => {
  const body = makeContainer();
  const block: any = { type: 'step', params: { Time: '0.5' } };
  renderBlockPanel(block, makeDeps(body));
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('sine block: all data-edit fields have a name attribute', () => {
  const body = makeContainer();
  const block: any = { type: 'sine', params: { amp: '1', freq: '1', phase: '0' } };
  renderBlockPanel(block, makeDeps(body));
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('scope block: all data-edit fields have a name attribute', () => {
  const body = makeContainer();
  const block: any = {
    type: 'scope',
    params: {},
    width: 200,
    height: 150,
    computedLimits: {},
  };
  renderBlockPanel(block, { ...makeDeps(body), getRuntimeSeconds: () => 10 });
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('tf block: all data-edit fields have a name attribute', () => {
  const body = makeContainer();
  const block: any = { type: 'tf', params: { num: [1], den: [1, 1] } };
  renderBlockPanel(block, makeDeps(body));
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('sum block: all data-edit fields (select) have a name attribute', () => {
  const body = makeContainer();
  const block: any = { type: 'sum', params: { signs: [1, 1, 1] } };
  renderBlockPanel(block, makeDeps(body));
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('comment block: input and textarea data-edit fields have a name attribute', () => {
  const body = makeContainer();
  const block: any = { type: 'comment', params: { commentText: 'hi', showBorder: true } };
  renderBlockPanel(block, makeDeps(body));
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('gain block: all data-edit fields have a name attribute', () => {
  const body = makeContainer();
  const block: any = { type: FieldNames.kGain, params: { gain: '2' } };
  renderBlockPanel(block, makeDeps(body));
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('pid block: all data-edit fields have a name attribute', () => {
  const body = makeContainer();
  const block: any = { type: 'pid', params: { kp: '1', ki: '0', kd: '0' } };
  renderBlockPanel(block, makeDeps(body));
  expectAllFieldsHaveName(body);
});
//=============================================================================
test('name attribute matches data-edit value', () => {
  const body = makeContainer();
  const block: any = { type: 'constant', params: { value: '1' } };
  renderBlockPanel(block, makeDeps(body));
  const input = body.querySelector<HTMLElement>(`input[data-edit="${FieldNames.kValue}"]`);
  expect(input).not.toBeNull();
  expect(input!.getAttribute('name')).toBe(FieldNames.kValue);
});
//=============================================================================
