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
import { makePortHandlers } from './interaction/renderer-port';
import * as FieldNames from '../FieldNames.js';
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeSvg = () => document.createElementNS(ns, 'svg');
const makePort = (overrides: any = {}) => ({
  x: 0,
  y: 20,
  side: 'right',
  wireX: 0,
  wireY: 20,
  ...overrides,
});
//=============================================================================
const makeCtx = (overrides: any = {}) => ({
  svg: makeSvg(),
  state: {
    blocks: new Map(),
    connections: [],
    selectedIds: new Set(),
    pendingPort: null,
  },
  ...overrides,
});
//=============================================================================
beforeEach(() => {
  document.body.innerHTML = '';
});
//=============================================================================
describe('makePortHandlers', () => {
  describe('createPortCircle', () => {
    test('returns a group element with port-group class', () => {
      const ctx = makeCtx();
      const { createPortCircle } = makePortHandlers(ctx as any);

      const group = createPortCircle('b1', 'in', 0, makePort() as any, FieldNames.kGain);

      expect(group.tagName.toLowerCase()).toBe('g');
      expect(group.getAttribute('class')).toContain('port-group');
    });

    test('sets data-block-id attribute on group', () => {
      const ctx = makeCtx();
      const { createPortCircle } = makePortHandlers(ctx as any);

      const group = createPortCircle('block42', 'out', 0, makePort() as any);

      expect(group.getAttribute('data-block-id')).toBe('block42');
    });

    test('sets data-port-type attribute', () => {
      const ctx = makeCtx();
      const { createPortCircle } = makePortHandlers(ctx as any);

      const inGroup = createPortCircle('b1', 'in', 0, makePort() as any);
      const outGroup = createPortCircle('b1', 'out', 0, makePort() as any);

      expect(inGroup.getAttribute('data-port-type')).toBe('in');
      expect(outGroup.getAttribute('data-port-type')).toBe('out');
    });

    test('in port renders a rect as shape, out port renders a triangle', () => {
      const ctx = makeCtx();
      const { createPortCircle } = makePortHandlers(ctx as any);

      const inGroup = createPortCircle('b1', 'in', 0, makePort() as any);
      const outGroup = createPortCircle('b1', 'out', 0, makePort() as any);

      expect(inGroup.querySelector('rect.port-square')).not.toBeNull();
      expect(outGroup.querySelector('polygon.port')).not.toBeNull();
    });

    test('adds hovered class on pointerenter', () => {
      const ctx = makeCtx();
      const { createPortCircle } = makePortHandlers(ctx as any);
      const group = createPortCircle('b1', 'out', 0, makePort() as any);

      group.dispatchEvent(new PointerEvent('pointerenter'));

      expect(group.classList.contains('hovered')).toBe(true);
    });

    test('removes hovered class on pointerleave', () => {
      const ctx = makeCtx();
      const { createPortCircle } = makePortHandlers(ctx as any);
      const group = createPortCircle('b1', 'out', 0, makePort() as any);
      group.classList.add('hovered');

      group.dispatchEvent(new PointerEvent('pointerleave'));

      expect(group.classList.contains('hovered')).toBe(false);
    });
  });

  describe('clearPending', () => {
    test('sets pendingPort to null', () => {
      const ctx = makeCtx();
      ctx.state.pendingPort = { blockId: 'b1', portType: 'out', portIndex: 0 };
      const { clearPending } = makePortHandlers(ctx as any);

      clearPending();

      expect(ctx.state.pendingPort).toBeNull();
    });
  });

  describe('updatePortVisibility', () => {
    test('adds ports-visible class to svg when pendingPort present', () => {
      const svg = makeSvg();
      const ctx = makeCtx({ svg });
      ctx.state.pendingPort = { blockId: 'b1', portType: 'out', portIndex: 0 };
      const { updatePortVisibility } = makePortHandlers(ctx as any);

      updatePortVisibility();

      expect(svg.classList.contains('ports-visible')).toBe(true);
    });

    test('removes ports-visible class when nothing selected or pending', () => {
      const svg = makeSvg();
      svg.classList.add('ports-visible');
      const ctx = makeCtx({ svg });
      ctx.state.pendingPort = null;
      ctx.state.selectedIds = new Set();
      const { updatePortVisibility } = makePortHandlers(ctx as any);

      updatePortVisibility();

      expect(svg.classList.contains('ports-visible')).toBe(false);
    });
  });

  describe('handlePortClick', () => {
    test('first click on out port sets pendingPort', () => {
      const ctx = makeCtx();
      const { createPortCircle, handlePortClick } = makePortHandlers(ctx as any);
      const group = createPortCircle('b1', 'out', 0, makePort() as any);

      handlePortClick(group);

      expect((ctx.state as any).pendingPort).not.toBeNull();
    });

    test('click on in port when no pendingPort does nothing', () => {
      const ctx = makeCtx();
      const { createPortCircle, handlePortClick } = makePortHandlers(ctx as any);
      const inGroup = createPortCircle('b1', 'in', 0, makePort() as any);

      handlePortClick(inGroup);

      expect((ctx.state as any).pendingPort).toBeNull();
    });

    test('suppresses click when suppressNextPortClick is true', () => {
      const ctx = makeCtx();
      (ctx.state as any).suppressNextPortClick = true;
      const { createPortCircle, handlePortClick } = makePortHandlers(ctx as any);
      const group = createPortCircle('b1', 'out', 0, makePort() as any);

      handlePortClick(group);

      expect((ctx as any).state.pendingPort).toBeNull();
      expect((ctx.state as any).suppressNextPortClick).toBe(false);
    });

    test('two-click connection: out then in calls createConnection', () => {
      const createConnection = vi.fn();
      const ctx = makeCtx({ createConnection });
      const { createPortCircle, handlePortClick } = makePortHandlers(ctx as any);
      const outGroup = createPortCircle('b1', 'out', 0, makePort() as any);
      const inGroup = createPortCircle('b2', 'in', 0, makePort() as any);

      handlePortClick(outGroup);
      handlePortClick(inGroup);

      expect(createConnection).toHaveBeenCalledWith('b1', 'b2', 0, 0);
    });

    test('clicking same block clears pending', () => {
      const ctx = makeCtx();
      const { createPortCircle, handlePortClick } = makePortHandlers(ctx as any);
      const out1 = createPortCircle('sameBlock', 'out', 0, makePort() as any);
      const in1 = createPortCircle('sameBlock', 'in', 0, makePort() as any);

      handlePortClick(out1);
      handlePortClick(in1);

      expect((ctx.state as any).pendingPort).toBeNull();
    });
  });
});
//=============================================================================
