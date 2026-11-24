//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import { createLatestAnimationFrameQueue } from '../../src/panels/figureAnimationFrameQueue';
//=============================================================================
describe('latest figure animation frame queue', () => {
  it('applies only the latest value once per presentation frame', () => {
    const callbacks: Array<() => void> = [];
    const apply = vi.fn();
    const queue = createLatestAnimationFrameQueue(
      apply,
      (callback) => {
        callbacks.push(callback);
        return callbacks.length;
      },
      vi.fn()
    );

    queue.push('first');
    queue.push('second');
    queue.push('latest');

    expect(callbacks).toHaveLength(1);
    expect(apply).not.toHaveBeenCalled();
    callbacks[0]();
    expect(apply).toHaveBeenCalledOnce();
    expect(apply).toHaveBeenCalledWith('latest');

    queue.push('next');
    expect(callbacks).toHaveLength(2);
  });

  it('discards a queued value when cancelled', () => {
    let callback = () => undefined;
    const cancel = vi.fn();
    const apply = vi.fn();
    const queue = createLatestAnimationFrameQueue(
      apply,
      (next) => {
        callback = next;
        return 41;
      },
      cancel
    );

    queue.push('pending');
    queue.cancel();
    callback();

    expect(cancel).toHaveBeenCalledWith(41);
    expect(apply).not.toHaveBeenCalled();
  });
});
//=============================================================================
