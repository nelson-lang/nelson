//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, expect, test, vi } from 'vitest';
import { abortableSleep, isAbortError, withDeadline } from './deadline.js';
//=============================================================================
afterEach(() => {
  vi.useRealTimers();
});
//=============================================================================
test('withDeadline without options just runs the work', async () => {
  await expect(withDeadline('job', async () => 42)).resolves.toBe(42);
});
//=============================================================================
test('withDeadline rejects with a TimeoutError and aborts the work signal', async () => {
  vi.useFakeTimers();
  let workSignal: AbortSignal | null = null;
  const pending = withDeadline(
    'RPC nflow.slow',
    (signal) => {
      workSignal = signal;
      return new Promise<never>(() => undefined);
    },
    { timeoutMs: 1000 }
  );
  const assertion = expect(pending).rejects.toMatchObject({
    name: 'TimeoutError',
    message: 'RPC nflow.slow timed out after 1000 ms',
  });
  await vi.advanceTimersByTimeAsync(1000);
  await assertion;
  expect(workSignal!.aborted).toBe(true);
});
//=============================================================================
test('withDeadline rejects with an AbortError when the caller aborts', async () => {
  const controller = new AbortController();
  const pending = withDeadline('job', () => new Promise<never>(() => undefined), {
    signal: controller.signal,
  });
  controller.abort();
  await expect(pending).rejects.toMatchObject({ name: 'AbortError' });
});
//=============================================================================
test('withDeadline rejects at once for an already aborted signal', async () => {
  const controller = new AbortController();
  controller.abort();
  const work = vi.fn(async () => 1);
  await expect(withDeadline('job', work, { signal: controller.signal })).rejects.toMatchObject({
    name: 'AbortError',
  });
  expect(work).not.toHaveBeenCalled();
});
//=============================================================================
test('withDeadline keeps the result when work finishes before the timeout', async () => {
  vi.useFakeTimers();
  const pending = withDeadline('job', async () => 'ok', { timeoutMs: 1000 });
  await expect(pending).resolves.toBe('ok');
  await vi.advanceTimersByTimeAsync(2000);
});
//=============================================================================
test('abortableSleep stops waiting when its signal aborts', async () => {
  const controller = new AbortController();
  const sleeping = abortableSleep(60000, controller.signal);
  controller.abort();
  await expect(sleeping).rejects.toBeDefined();
});
//=============================================================================
test('isAbortError recognizes cancellation and timeout errors only', () => {
  expect(isAbortError(Object.assign(new Error('x'), { name: 'AbortError' }))).toBe(true);
  expect(isAbortError(Object.assign(new Error('x'), { name: 'TimeoutError' }))).toBe(true);
  expect(isAbortError(new Error('x'))).toBe(false);
  expect(isAbortError(null)).toBe(false);
});
//=============================================================================
