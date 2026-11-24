//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export type AnimationFrameScheduler = (callback: () => void) => number;
export type AnimationFrameCanceller = (handle: number) => void;
export function createLatestAnimationFrameQueue<T>(
  apply: (value: T) => void,
  schedule: AnimationFrameScheduler = (callback) => window.requestAnimationFrame(callback),
  cancelFrame: AnimationFrameCanceller = (handle) => window.cancelAnimationFrame(handle)
) {
  let scheduledFrame: number | null = null;
  let latestValue: T | undefined;

  const flush = () => {
    scheduledFrame = null;
    const value = latestValue;
    latestValue = undefined;
    if (value !== undefined) apply(value);
  };

  return {
    push(value: T) {
      latestValue = value;
      if (scheduledFrame === null) scheduledFrame = schedule(flush);
    },
    cancel() {
      if (scheduledFrame !== null) cancelFrame(scheduledFrame);
      scheduledFrame = null;
      latestValue = undefined;
    },
  };
}
//=============================================================================
