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
import { applyBlockTypeLabels } from './block/renderer-block-label-types';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeGroup = () => document.createElementNS(ns, 'g');
const makeText = () => document.createElementNS(ns, 'text');
const makeCtx = (overrides: any = {}) => ({
  toLeadingMinusTex: (v: any, d: string) => ({ tex: String(v ?? d) }),
  scaleMathToFit: () => {},
  updateCommentLayout: () => {},
  resizeUserFuncFromLabel: () => {},
  ...overrides,
});
//=============================================================================
const makeBlock = (type: string, params: any = {}, groupFn?: () => Element) => {
  const group = groupFn ? groupFn() : makeGroup();
  return { type, group, params, width: 80, height: 40 };
};
//=============================================================================
describe('applyBlockTypeLabels', () => {
  test('step sets stepTime as text[1] content', () => {
    const group = makeGroup();
    group.appendChild(makeText()); // texts[0]
    const t1 = makeText();
    group.appendChild(t1);
    const block: any = makeBlock('step', { Time: 2.5 });
    (block as any).group = group;

    applyBlockTypeLabels(block, makeCtx());

    expect(t1.textContent).toBe('t=2.5');
  });

  test('ramp sets slope and start in text[1]', () => {
    const group = makeGroup();
    group.appendChild(makeText());
    const t1 = makeText();
    group.appendChild(t1);
    const block: any = makeBlock('ramp', { slope: 2, start: 0.5 });
    (block as any).group = group;

    applyBlockTypeLabels(block, makeCtx());

    expect(t1.textContent).toBe('m=2 t0=0.5');
  });

  test('impulse sets time and amp in text[1]', () => {
    const group = makeGroup();
    group.appendChild(makeText());
    const t1 = makeText();
    group.appendChild(t1);
    const block: any = makeBlock('impulse', { Time: 1, Amplitude: 3 });
    (block as any).group = group;

    applyBlockTypeLabels(block, makeCtx());

    expect(t1.textContent).toBe('t=1 A=3');
  });

  test('sine sets amp/freq and phase in text[1]/text[2]', () => {
    const group = makeGroup();
    group.appendChild(makeText());
    const t1 = makeText();
    const t2 = makeText();
    group.appendChild(t1);
    group.appendChild(t2);
    const block: any = makeBlock('sine', { Amplitude: 1, Frequency: 50, Phase: 0 });
    (block as any).group = group;

    applyBlockTypeLabels(block, makeCtx());

    expect(t1.textContent).toBe('A=1 f=50');
    expect(t2.textContent).toBe('ph=0');
  });

  test('chirp sets amp/f0 and f1/t1 in text[1]/text[2]', () => {
    const group = makeGroup();
    group.appendChild(makeText());
    const t1 = makeText();
    const t2 = makeText();
    group.appendChild(t1);
    group.appendChild(t2);
    const block: any = makeBlock('chirp', { Amplitude: 1, f1: 10, f2: 100, T: 5 });
    (block as any).group = group;

    applyBlockTypeLabels(block, makeCtx());

    expect(t1.textContent).toBe('A=1 f1=10');
    expect(t2.textContent).toBe('f2=100 T=5');
  });

  test('noise sets amp in text[1]', () => {
    const group = makeGroup();
    group.appendChild(makeText());
    const t1 = makeText();
    group.appendChild(t1);
    const block: any = makeBlock('noise', { Amplitude: 0.5 });
    (block as any).group = group;

    applyBlockTypeLabels(block, makeCtx());

    expect(t1.textContent).toBe('A=0.5');
  });

  test('comment calls ctx.updateCommentLayout', () => {
    const calls: any[] = [];
    const ctx = makeCtx({ updateCommentLayout: (b: any) => calls.push(b) });
    const block: any = makeBlock('comment', {});

    applyBlockTypeLabels(block, ctx);

    expect(calls.length).toBe(1);
    expect(calls[0]).toBe(block);
  });

  test('userFunc calls ctx.resizeUserFuncFromLabel', () => {
    const calls: any[] = [];
    const ctx = makeCtx({ resizeUserFuncFromLabel: (b: any) => calls.push(b) });
    const group = makeGroup();
    const mathGroup = document.createElementNS(ns, 'g');
    mathGroup.setAttribute('class', 'nf-userfunc-formula');
    group.appendChild(mathGroup);
    const block: any = makeBlock('expression', { expr: 'sin(u)' });
    (block as any).group = group;

    applyBlockTypeLabels(block, ctx);

    expect(calls.length).toBe(1);
  });

  test('pid calls renderTeXMath on .nf-pid-formula group (no throw)', () => {
    const group = makeGroup();
    const mathGroup = document.createElementNS(ns, 'g');
    mathGroup.setAttribute('class', 'nf-pid-formula');
    group.appendChild(mathGroup);
    const block: any = makeBlock('pid', {});
    (block as any).group = group;
    expect(() => applyBlockTypeLabels(block, makeCtx())).not.toThrow();
  });

  test('unknown type does nothing and does not throw', () => {
    const block: any = makeBlock('unknownXYZ', {});
    expect(() => applyBlockTypeLabels(block, makeCtx())).not.toThrow();
  });
});
//=============================================================================
