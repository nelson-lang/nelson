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
import { applySubsystemBlockLabel } from './block/renderer-block-label-subsystem';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeGroup = () => document.createElementNS(ns, 'g');
const makeEl = (tag: string, cls: string) => {
  const el = document.createElementNS(ns, tag);
  el.setAttribute('class', cls);
  return el;
};
//=============================================================================
const makeCtx = (overrides: any = {}) => ({
  toLeadingMinusTex: () => ({ tex: '' }),
  scaleMathToFit: () => {},
  updateCommentLayout: () => {},
  resizeUserFuncFromLabel: () => {},
  ...overrides,
});
//=============================================================================
describe('applySubsystemBlockLabel', () => {
  test('labelSource sets mathGroup transform attribute', () => {
    const group = makeGroup();
    const mathGroup = makeEl('g', 'label-math');
    group.appendChild(mathGroup);
    const block: any = {
      type: FieldNames.kLabelSource,
      group,
      width: 80,
      height: 40,
      params: { GotoTag: 'sig' },
    };

    applySubsystemBlockLabel(block, makeCtx());
    const transform = mathGroup.getAttribute('transform');
    expect(transform).toBeTruthy();
    expect(transform).toMatch(/translate/);
  });

  test('labelSink shows circle and line when showNode is true', () => {
    const group = makeGroup();
    const mathGroup = makeEl('g', 'label-math');
    const circle = document.createElementNS(ns, 'circle');
    circle.setAttribute('class', 'label-node');
    const line = document.createElementNS(ns, 'line');
    line.setAttribute('class', 'label-node');
    group.appendChild(mathGroup);
    group.appendChild(circle);
    group.appendChild(line);
    const block: any = {
      type: FieldNames.kLabelSink,
      group,
      width: 80,
      height: 40,
      params: { GotoTag: 'out', ShowNode: true },
    };

    applySubsystemBlockLabel(block, makeCtx());

    expect(circle.style.display).not.toBe('none');
    expect(line.style.display).not.toBe('none');
  });

  test('labelSink hides circle and line when showNode is false', () => {
    const group = makeGroup();
    const mathGroup = makeEl('g', 'label-math');
    const circle = document.createElementNS(ns, 'circle');
    circle.setAttribute('class', 'label-node');
    const line = document.createElementNS(ns, 'line');
    line.setAttribute('class', 'label-node');
    group.appendChild(mathGroup);
    group.appendChild(circle);
    group.appendChild(line);
    const block: any = {
      type: FieldNames.kLabelSink,
      group,
      width: 80,
      height: 40,
      params: { GotoTag: 'out', ShowNode: false },
    };

    applySubsystemBlockLabel(block, makeCtx());

    expect(circle.style.display).toBe('none');
    expect(line.style.display).toBe('none');
  });

  test('subsystem sets title text from params.name', () => {
    const group = makeGroup();
    const title = document.createElementNS(ns, 'text');
    title.setAttribute('class', 'block-text');
    const labelLayer = makeEl('g', 'subsystem-port-labels');
    group.appendChild(title);
    group.appendChild(labelLayer);
    const block: any = {
      type: FieldNames.kSubsystem,
      group,
      width: 120,
      height: 80,
      params: { name: 'MySystem', externalInputs: [], externalOutputs: [] },
      ports: [],
    };

    applySubsystemBlockLabel(block, makeCtx());

    expect(title.textContent).toBe('MySystem');
  });

  test('subsystem uses Subsystem as fallback title when name missing', () => {
    const group = makeGroup();
    const title = document.createElementNS(ns, 'text');
    title.setAttribute('class', 'block-text');
    const labelLayer = makeEl('g', 'subsystem-port-labels');
    group.appendChild(title);
    group.appendChild(labelLayer);
    const block: any = {
      type: FieldNames.kSubsystem,
      group,
      width: 120,
      height: 80,
      params: { externalInputs: [], externalOutputs: [] },
      ports: [],
    };

    applySubsystemBlockLabel(block, makeCtx());

    expect(title.textContent).toBe('Subsystem');
  });

  test('does nothing for non-matching block types', () => {
    const block: any = {
      type: FieldNames.kGain,
      group: makeGroup(),
      width: 60,
      height: 40,
      params: {},
    };
    expect(() => applySubsystemBlockLabel(block, makeCtx())).not.toThrow();
  });
});
//=============================================================================
