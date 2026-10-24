//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import * as FieldNames from '../FieldNames.js';
//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
describe('drawIcon helpers', () => {
  let render: any;

  beforeAll(async () => {
    (global as any).queueMathJaxTypeset = (global as any).queueMathJaxTypeset || (() => {});
    render = await import('../../../dist/render/block/block-icons.js');
  });

  test('drawIcon returns SVG group and contains expected children for types', () => {
    const types = [
      'constant',
      'step',
      'ramp',
      'impulse',
      'sine',
      'chirp',
      'noise',
      'file',
      'scope',
      'saturation',
      'rate',
      'backlash',
      'lpf',
      'hpf',
      'pid',
      'tf',
      'dtf',
      'zoh',
      'foh',
      'integrator',
      'derivative',
      FieldNames.kDelay,
      'ddelay',
      'stateSpace',
      'dstateSpace',
      'unknown',
    ];

    types.forEach((t) => {
      const g = render.drawIcon(t, 0, 0, 22);
      expect(g).toBeTruthy();
      expect(g.tagName.toLowerCase()).toBe('g');
      expect(g.childElementCount).toBeGreaterThanOrEqual(1);
    });
  });

  test('drawIcon math variants include foreignObject', () => {
    const mathTypes = ['dtf', 'ddelay'];
    mathTypes.forEach((t) => {
      const g = render.drawIcon(t, 0, 0, 22);
      const fo = g.querySelector('foreignObject');
      expect(fo).not.toBeNull();
    });
  });
});
//=============================================================================
