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
import { vi } from 'vitest';
import { renderBlockPanel } from './inspector-block';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
function makeContainer() {
  const el = document.createElement('div');
  return el;
}
//=============================================================================
function makeDeps(inspectorBody: HTMLElement | null) {
  return {
    inspectorBody,
    callUpdate: vi.fn(),
    callResize: vi.fn(),
    renderInspector: vi.fn(),
  } as any;
}
//=============================================================================
function makeSvgGroup() {
  const g = document.createElementNS(ns, 'g');
  const mask = document.createElementNS(ns, 'g');
  mask.className.baseVal = 'masked-indicator';
  mask.setAttribute('data-visible', 'false');
  g.appendChild(mask);
  return g;
}
//=============================================================================
test('subsystem inspector shows open button when not masked', () => {
  const body = makeContainer();
  const block: any = {
    type: FieldNames.kSubsystem,
    params: { name: 'S', masked: false, externalInputs: [], externalOutputs: [] },
  };
  renderBlockPanel(block, makeDeps(body));
  const openBtn = body.querySelector("button[data-action='open-subsystem']");
  expect(openBtn).not.toBeNull();
  const maskedInput = body.querySelector<HTMLInputElement>(
    `input[data-edit="${FieldNames.kMasked}"]`
  );
  expect(maskedInput).not.toBeNull();
  expect(maskedInput!.checked).toBe(false);
});
//=============================================================================
test('subsystem inspector hides open button when masked is true', () => {
  const body = makeContainer();
  const block: any = {
    type: FieldNames.kSubsystem,
    params: { name: 'S', masked: true, externalInputs: [], externalOutputs: [] },
  };
  renderBlockPanel(block, makeDeps(body));
  const openBtn = body.querySelector("button[data-action='open-subsystem']");
  expect(openBtn).toBeNull();
  const maskedInput = body.querySelector<HTMLInputElement>(
    `input[data-edit="${FieldNames.kMasked}"]`
  );
  expect(maskedInput).not.toBeNull();
  expect(maskedInput!.checked).toBe(true);
});
//=============================================================================
test('changing masked checkbox updates params, mask indicator and re-renders', () => {
  const body = makeContainer();
  const deps = makeDeps(body);
  const block: any = {
    id: 'sub1',
    type: FieldNames.kSubsystem,
    params: { name: 'S', masked: false, externalInputs: [], externalOutputs: [] },
    group: makeSvgGroup(),
  };

  renderBlockPanel(block, deps);

  const maskedInput = body.querySelector<HTMLInputElement>(
    `input[data-edit="${FieldNames.kMasked}"]`
  );
  expect(maskedInput).not.toBeNull();
  maskedInput!.checked = true;
  maskedInput!.dispatchEvent(new Event('change', { bubbles: true }));

  expect(block.params.masked).toBe(true);
  expect((deps.callUpdate as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(1);
  expect((deps.renderInspector as vi.Mock).mock.calls.length).toBeGreaterThanOrEqual(1);

  const maskEl = block.group.querySelector('g.masked-indicator');
  expect(maskEl).not.toBeNull();
  expect(maskEl!.getAttribute('data-visible')).toBe('true');
});
//=============================================================================
