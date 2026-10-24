//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { test, expect, vi } from 'vitest';
import { runDeferredFit } from './diagram-handlers.js';
//=============================================================================
// Drive the frame scheduler synchronously so the whole poll runs in one call.
const sync = (cb: () => void): void => cb();
test('fits only once the canvas size is stable across two frames', () => {
  const sizes = [
    { width: 100, height: 100 },
    { width: 200, height: 200 },
    { width: 200, height: 200 },
  ];
  let i = 0;
  const getRect = () => sizes[Math.min(i++, sizes.length - 1)];
  const fit = vi.fn();
  runDeferredFit(getRect, fit, sync);
  expect(fit).toHaveBeenCalledTimes(1);
  // Three ticks: 100 -> 200 (changed) -> 200 (stable, fit).
  expect(i).toBe(3);
});
//=============================================================================
test('fits immediately when there is no canvas (jsdom / detached)', () => {
  const fit = vi.fn();
  runDeferredFit(() => null, fit, sync);
  expect(fit).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('caps attempts and fits when the size never stabilizes', () => {
  let i = 0;
  const getRect = () => ({ width: 100 + i++ * 10, height: 100 });
  const fit = vi.fn();
  runDeferredFit(getRect, fit, sync, 5);
  expect(fit).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('waits out a zero-sized canvas, then fits at the attempt cap', () => {
  const fit = vi.fn();
  runDeferredFit(() => ({ width: 0, height: 0 }), fit, sync, 4);
  expect(fit).toHaveBeenCalledTimes(1);
});
//=============================================================================
export {};
//=============================================================================
