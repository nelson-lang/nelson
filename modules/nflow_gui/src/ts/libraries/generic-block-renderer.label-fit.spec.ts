//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Regression: a `\mathtt{...}` literal-string label (a To Workspace variable
// name, a Nelson function body) must (1) render its underscores literally
// instead of as LaTeX subscripts, and (2) shrink to fit its block so it no
// longer overflows the body and clips over the input port.
import { describe, test, expect } from 'vitest';
import { buildRegistryFromManifest } from './generic-block-renderer.js';
import { renderTeXMath } from '../render/svg-elements.js';
//=============================================================================
function makeSinkRenderer() {
  const manifest: any = {
    id: 'test.sink',
    blocks: [
      {
        type: 'toWorkspace',
        width: 80,
        height: 48,
        inputs: [{ x: 0, y: 24, side: 'left' }],
        outputs: [],
        defaultParams: { VariableName: 'simout' },
        render: { type: 'math', formula: '\\mathtt{{params.VariableName}}', textSize: 14 },
      },
      {
        // A numeric label (no \mathtt wrapper) must NOT be escaped: subscripts
        // and other TeX markup stay meaningful for real formulas.
        type: 'plainValue',
        width: 80,
        height: 48,
        inputs: [{ x: 0, y: 24, side: 'left' }],
        outputs: [{ x: 80, y: 24, side: 'right' }],
        defaultParams: { Value: '0' },
        render: { type: 'math', formula: '{params.Value}', textSize: 14 },
      },
    ],
  };
  const registry = buildRegistryFromManifest(manifest as any);
  return (registry as any).getDefinitions();
}
//=============================================================================
function makeGroup(): SVGGElement {
  return document.createElementNS('http://www.w3.org/2000/svg', 'g') as SVGGElement;
}
//=============================================================================
describe('generic math label: literal-string labels fit and escape', () => {
  test('toWorkspace variable name gets data-fit-text so it never overflows the port', () => {
    const defs = makeSinkRenderer();
    const group = makeGroup();
    const block = {
      group,
      width: 80,
      height: 48,
      id: 'tw',
      type: 'toWorkspace',
      params: { VariableName: 'command_bode_out' },
    };
    defs['toWorkspace'].render(block, {} as any);
    const mathGroup = group.querySelector('.nf-block-formula');
    expect(mathGroup).not.toBeNull();
    expect(mathGroup?.getAttribute('data-fit-text')).toBe('1');
  });

  test('underscores in the variable name are LaTeX-escaped (literal, not subscripts)', () => {
    const defs = makeSinkRenderer();
    const group = makeGroup();
    const block = {
      group,
      width: 80,
      height: 48,
      id: 'tw',
      type: 'toWorkspace',
      params: { VariableName: 'command_bode_out' },
    };
    defs['toWorkspace'].render(block, { renderTeXMath } as any);
    const span = group.querySelector('span[data-tex]') as HTMLElement | null;
    expect(span).not.toBeNull();
    expect(span?.dataset.tex).toBe('\\mathtt{command\\_bode\\_out}');
  });

  test('a non-literal (numeric) formula is left untouched: no escaping, no forced fit', () => {
    const defs = makeSinkRenderer();
    const group = makeGroup();
    const block = {
      group,
      width: 80,
      height: 48,
      id: 'pv',
      type: 'plainValue',
      // A value that is not a plain single token and carries an underscore:
      // it must survive verbatim (escaping is scoped to \mathtt labels).
      params: { Value: 'a_b+c' },
    };
    defs['plainValue'].render(block, { renderTeXMath } as any);
    const mathGroup = group.querySelector('.nf-block-formula');
    expect(mathGroup?.getAttribute('data-fit-text')).toBeNull();
    const span = group.querySelector('span[data-tex]') as HTMLElement | null;
    expect(span?.dataset.tex).toBe('a_b+c');
  });
});
//=============================================================================
