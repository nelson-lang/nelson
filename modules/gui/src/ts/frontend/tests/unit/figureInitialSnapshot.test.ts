//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, expect, it, vi } from 'vitest';
import { loadInitialFigureSnapshot } from '../../src/panels/figureInitialSnapshot';
//=============================================================================
const frame = { handle: 0, width: 400, height: 300, primitives: [], controls: [] };
afterEach(() => vi.useRealTimers());
it('does not repeatedly reload a valid static plot with no UI controls', async () => {
  const load = vi.fn(async () => frame);
  const accept = vi.fn();
  await loadInitialFigureSnapshot(load, accept, () => true);
  expect(load).toHaveBeenCalledOnce();
  expect(accept).toHaveBeenCalledWith(frame);
});
//=============================================================================
it('retries an unavailable initial frame but stops at the first success', async () => {
  vi.useFakeTimers();
  const load = vi.fn().mockRejectedValueOnce(new Error('Not ready')).mockResolvedValue(frame);
  const accept = vi.fn();
  const pending = loadInitialFigureSnapshot(load, accept, () => true);
  await vi.advanceTimersByTimeAsync(200);
  await pending;
  expect(load).toHaveBeenCalledTimes(2);
  expect(accept).toHaveBeenCalledOnce();
});
//=============================================================================
it('ignores an in-flight snapshot after the panel closes', async () => {
  let active = true;
  let finish!: (value: typeof frame) => void;
  const load = vi.fn(
    () =>
      new Promise<typeof frame>((resolve) => {
        finish = resolve;
      })
  );
  const accept = vi.fn();
  const pending = loadInitialFigureSnapshot(load, accept, () => active);
  active = false;
  finish(frame);
  await pending;
  expect(accept).not.toHaveBeenCalled();
  await loadInitialFigureSnapshot(load, accept, () => false);
  expect(load).toHaveBeenCalledOnce();
});
//=============================================================================
