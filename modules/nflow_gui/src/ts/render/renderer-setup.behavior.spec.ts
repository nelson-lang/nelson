//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { vi } from 'vitest';
import { setupRendererDom } from './core/renderer-setup';
//=============================================================================
beforeEach(() => {
  // @ts-ignore
  global.requestAnimationFrame = (cb) => {
    cb();
    return 0;
  };
});
//=============================================================================
test('setupRendererDom creates defs and layers and attaches selection/marquee', async () => {
  const ns = 'http://www.w3.org/2000/svg';
  const svg = document.createElementNS(ns, 'svg');
  const overlay = document.createElement('div');
  document.body.appendChild(overlay);

  const debugLog = document.createElement('div');
  debugLog.id = 'debugLog';
  debugLog.textContent = 'log text';
  document.body.appendChild(debugLog);
  const copyBtn = document.createElement('button');
  copyBtn.id = 'copyDebug';
  document.body.appendChild(copyBtn);

  // @ts-ignore
  global.navigator = global.navigator || {};
  // @ts-ignore
  global.navigator.clipboard = { writeText: vi.fn().mockResolvedValue(undefined) };

  const ctx: any = { svg, overlayLayer: overlay };
  setupRendererDom(ctx);

  const defs = svg.querySelector('defs');
  expect(defs).not.toBeNull();
  expect(defs!.querySelector('#wire-arrow')).not.toBeNull();

  expect(overlay.contains(ctx.selectionRect)).toBe(true);
  expect(overlay.contains(ctx.marqueeRect)).toBe(true);

  expect(ctx.wireJunctionLayer).toBeDefined();
  expect(ctx.wireCornerLayer).toBeDefined();

  copyBtn.click();
  await Promise.resolve();
  // @ts-ignore
  expect(navigator.clipboard.writeText).toHaveBeenCalled();
});
//=============================================================================
