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
import { makeParamDisplayHandlers } from './block/renderer-param-display';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeTextEl = () => document.createElementNS(ns, 'text');
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
describe('makeParamDisplayHandlers', () => {
  describe('updateParamDisplay', () => {
    test('no-op when paramDisplay element is missing', () => {
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);
      const block: any = { params: { _visible: { a: true } } };

      expect(() => updateParamDisplay(block)).not.toThrow();
    });

    test('hides text when _visible is missing', () => {
      const textEl = makeTextEl();
      const block: any = { width: 60, height: 30, paramDisplay: textEl, params: {} };
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);

      updateParamDisplay(block);

      expect(textEl.getAttribute('display')).toBe('none');
    });

    test('hides text when _visible is empty object', () => {
      const textEl = makeTextEl();
      const block: any = { width: 60, height: 30, paramDisplay: textEl, params: { _visible: {} } };
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);

      updateParamDisplay(block);

      expect(textEl.getAttribute('display')).toBe('none');
      expect(textEl.textContent).toBe('');
    });

    test('shows entries with param labels and values', () => {
      const textEl = makeTextEl();
      const block: any = {
        width: 40,
        height: 20,
        paramDisplay: textEl,
        params: { _visible: { a: true, b: true }, a: '1', b: 2 },
        paramLabels: { a: 'Alpha' },
      };
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);

      updateParamDisplay(block);

      expect(textEl.getAttribute('display')).toBe('block');
      const tspan = textEl.querySelector('tspan');
      expect(tspan).not.toBeNull();
      expect(tspan!.textContent).toContain('Alpha: 1');
    });

    test('uses key as label when paramLabels entry is absent', () => {
      const textEl = makeTextEl();
      const block: any = {
        width: 40,
        height: 20,
        paramDisplay: textEl,
        params: { _visible: { gain: true }, gain: 5 },
        paramLabels: {},
      };
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);

      updateParamDisplay(block);

      const tspan = textEl.querySelector('tspan');
      expect(tspan!.textContent).toContain('gain: 5');
    });

    test('scope tMax fallback: uses runtimeInput.value when present', () => {
      const runtime = document.createElement('input');
      runtime.id = 'runtimeInput';
      runtime.value = '7';
      document.body.appendChild(runtime);
      const textEl = makeTextEl();
      const block: any = {
        type: 'scope',
        width: 40,
        height: 20,
        paramDisplay: textEl,
        params: { _visible: { TMax: true } },
        computedLimits: {},
      };
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);

      updateParamDisplay(block);

      const tspan = textEl.querySelector('tspan');
      expect(tspan!.textContent).toContain('TMax: 7');
    });

    test('scope tMin fallback returns 0 when computedLimits empty', () => {
      const textEl = makeTextEl();
      const block: any = {
        type: 'scope',
        width: 40,
        height: 20,
        paramDisplay: textEl,
        params: { _visible: { TMin: true } },
        computedLimits: {},
      };
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);

      updateParamDisplay(block);

      const tspan = textEl.querySelector('tspan');
      expect(tspan!.textContent).toContain('TMin: 0');
    });

    test('xyScope xMin fallback returns -1.2 when computedLimits empty', () => {
      const textEl = makeTextEl();
      const block: any = {
        type: 'xyScope',
        width: 40,
        height: 20,
        paramDisplay: textEl,
        params: { _visible: { XMin: true } },
        computedLimits: {},
      };
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);

      updateParamDisplay(block);

      const tspan = textEl.querySelector('tspan');
      expect(tspan!.textContent).toContain('XMin: -1.2');
    });

    test('creates one tspan per visible entry', () => {
      const textEl = makeTextEl();
      const block: any = {
        width: 60,
        height: 30,
        paramDisplay: textEl,
        params: { _visible: { a: true, b: true, c: true }, a: 1, b: 2, c: 3 },
        paramLabels: {},
      };
      const { updateParamDisplay } = makeParamDisplayHandlers({} as any);

      updateParamDisplay(block);

      expect(textEl.querySelectorAll('tspan').length).toBe(3);
    });
  });
});
//=============================================================================
