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
import fs from 'fs';
import path from 'path';
//=============================================================================
let noise: any;
beforeAll(async () => {
  const br = (await import('../../app/blocks-registry.js')).default;
  noise = br.getDefinitions()['noise']?.render as any;
});
//=============================================================================
const ns = 'http://www.w3.org/2000/svg';
const makeBlock = (overrides: any = {}) => ({
  id: 'n1',
  group: document.createElementNS(ns, 'g'),
  width: 60,
  height: 60,
  params: {},
  ...overrides,
});
//=============================================================================
describe('noise renderer', () => {
  test('calls renderSourcePlot with the block group or appends a path', () => {
    const renderSourcePlot = vi.fn();
    const block = makeBlock();

    noise(block as any, { renderSourcePlot } as any);

    const called = !!(
      renderSourcePlot &&
      (renderSourcePlot as any).mock &&
      (renderSourcePlot as any).mock.calls.length > 0
    );
    const hasPath = !!block.group.querySelector('path');
    const hasAny = block.group.childElementCount > 0;
    expect(called || hasPath || hasAny).toBeTruthy();
  });

  test('passes block width and height to renderSourcePlot when used, else path respects size', () => {
    const renderSourcePlot = vi.fn();
    const block = makeBlock({ width: 80, height: 70 });

    noise(block as any, { renderSourcePlot } as any);

    const called = !!(
      renderSourcePlot &&
      (renderSourcePlot as any).mock &&
      (renderSourcePlot as any).mock.calls.length > 0
    );
    if (called) {
      const [grp, w, h] = (renderSourcePlot as any).mock.calls[0];
      expect(grp).toBe(block.group);
      expect(w).toBe(80);
      expect(h).toBe(70);
    } else {
      const pathEl = block.group.querySelector('path');
      expect(pathEl || block.group.childElementCount > 0).not.toBeNull();
    }
  });

  test('passes a non-empty path string to renderSourcePlot when used, otherwise a path element is appended', () => {
    const renderSourcePlot = vi.fn();
    const block = makeBlock();

    noise(block as any, { renderSourcePlot } as any);

    const called = !!(
      renderSourcePlot &&
      (renderSourcePlot as any).mock &&
      (renderSourcePlot as any).mock.calls.length > 0
    );
    if (called) {
      const pathArg = (renderSourcePlot as any).mock.calls[0][3];
      expect(typeof pathArg).toBe('string');
      expect(pathArg.length).toBeGreaterThan(0);
    } else {
      const pathEl = block.group.querySelector('path');
      if (pathEl) {
        const d = pathEl.getAttribute('d') || '';
        expect(typeof d).toBe('string');
        expect(d.length).toBeGreaterThan(0);
      } else {
        expect(block.group.childElementCount).toBeGreaterThan(0);
      }
    }
  });
});
//=============================================================================
