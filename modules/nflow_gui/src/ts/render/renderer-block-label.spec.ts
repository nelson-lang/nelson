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
//=============================================================================
vi.mock('../app/library-loader', () => ({
  buildAllBlockTemplates: () => ({}),
  discoverBuiltinLibraries: async () => {},
  loadLibrary: async () => null,
}));
//=============================================================================
import { makeBlockLabelHandlers } from './block/renderer-block-label';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeCtx = (overrides: any = {}) => ({
  state: { blocks: new Map(), connections: [] },
  toLeadingMinusTex: (_v: any, d: string) => ({ tex: d }),
  scaleMathToFit: () => {},
  positionGainMath: () => {},
  updateCommentLayout: () => {},
  resizeUserFuncFromLabel: () => {},
  updateBlockTransform: vi.fn(),
  createPortCircle: () => document.createElementNS(ns, 'circle'),
  updateConnections: vi.fn(),
  updateParamDisplay: vi.fn(),
  updatePortElement: vi.fn(),
  ...overrides,
});
//=============================================================================
const makeBlock = (type: string, params: any = {}) => {
  const group = document.createElementNS(ns, 'g');
  return {
    id: 'b1',
    type,
    group,
    width: 80,
    height: 40,
    rotation: 0,
    ports: [],
    params,
  };
};
//=============================================================================
describe('makeBlockLabelHandlers', () => {
  test('returns object with updateBlockLabel function', () => {
    const ctx = makeCtx();

    const handlers = makeBlockLabelHandlers(ctx as any);

    expect(typeof handlers.updateBlockLabel).toBe('function');
  });

  test('updateBlockLabel calls ctx.updateParamDisplay', () => {
    const updateParamDisplay = vi.fn();
    const ctx = makeCtx({ updateParamDisplay });
    const { updateBlockLabel } = makeBlockLabelHandlers(ctx as any);
    const block = makeBlock('step', { stepTime: 1 });

    updateBlockLabel(block as any);

    expect(updateParamDisplay).toHaveBeenCalledWith(block);
  });

  test('updateBlockLabel does not throw for gain block type', () => {
    const ctx = makeCtx();
    const { updateBlockLabel } = makeBlockLabelHandlers(ctx as any);
    const group = document.createElementNS(ns, 'g');
    const mathGroup = document.createElementNS(ns, 'g');
    mathGroup.setAttribute('class', 'nf-gain-formula');
    group.appendChild(mathGroup);
    const block = { ...makeBlock(FieldNames.kGain, { gain: 2 }), group };
    expect(() => updateBlockLabel(block as any)).not.toThrow();
  });

  test('updateBlockLabel handles noise block and sets text content', () => {
    const ctx = makeCtx();
    const { updateBlockLabel } = makeBlockLabelHandlers(ctx as any);
    const group = document.createElementNS(ns, 'g');
    const t0 = document.createElementNS(ns, 'text');
    const t1 = document.createElementNS(ns, 'text');
    group.appendChild(t0);
    group.appendChild(t1);
    const block = { ...makeBlock('noise', { Amplitude: 0.1 }), group };

    updateBlockLabel(block as any);
    expect(t1.textContent).toBe('A=0.1');
  });

  test('updateBlockLabel invokes updateParamDisplay once', () => {
    const calls: any[] = [];
    const ctx = makeCtx({ updateParamDisplay: (b: any) => calls.push(b) });
    const { updateBlockLabel } = makeBlockLabelHandlers(ctx as any);
    const block = makeBlock('constant', { Value: 5 });

    updateBlockLabel(block as any);

    expect(calls.length).toBe(1);
  });

  test('updateBlockLabel updates comparison operator display', () => {
    const ctx = makeCtx();
    const { updateBlockLabel } = makeBlockLabelHandlers(ctx as any);
    const block = makeBlock('compareToConstant', { relop: 'ge' });
    const expressionText = document.createElementNS(ns, 'text');
    expressionText.setAttribute('class', 'logic-expression-display');
    expressionText.textContent = 'u >= c';
    block.group.appendChild(expressionText);

    block.params.relop = 'lt';
    updateBlockLabel(block as any);

    expect(expressionText.textContent).toBe('u < c');
  });
});
//=============================================================================
