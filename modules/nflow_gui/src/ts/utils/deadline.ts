//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Cancellation and timeout for promise-based work. Aborting only stops the
// client side from waiting: a request already sent to the engine still runs.
export interface DeadlineOptions {
  signal?: AbortSignal;
  timeoutMs?: number;
}
//=============================================================================
export function makeAbortError(label: string): Error {
  const error = new Error(`${label} was cancelled`);
  error.name = 'AbortError';
  return error;
}
//=============================================================================
export function makeTimeoutError(label: string, timeoutMs: number): Error {
  const error = new Error(`${label} timed out after ${timeoutMs} ms`);
  error.name = 'TimeoutError';
  return error;
}
//=============================================================================
export function isAbortError(error: unknown): boolean {
  const name = (error as { name?: unknown } | null)?.name;
  return name === 'AbortError' || name === 'TimeoutError';
}
//=============================================================================
// Resolves after `ms`, or rejects as soon as `signal` aborts.
export function abortableSleep(ms: number, signal?: AbortSignal): Promise<void> {
  return new Promise((resolve, reject) => {
    if (signal?.aborted) {
      reject(signal.reason ?? makeAbortError('sleep'));
      return;
    }
    const onAbort = () => {
      clearTimeout(timer);
      reject(signal?.reason ?? makeAbortError('sleep'));
    };
    const timer = setTimeout(() => {
      signal?.removeEventListener('abort', onAbort);
      resolve();
    }, ms);
    signal?.addEventListener('abort', onAbort, { once: true });
  });
}
//=============================================================================
// Runs `work` with a signal that aborts when the caller's signal aborts or the
// timeout elapses; the returned promise rejects at that moment even if `work`
// ignores its signal.
export function withDeadline<T>(
  label: string,
  work: (signal: AbortSignal) => Promise<T>,
  options: DeadlineOptions = {}
): Promise<T> {
  const { signal, timeoutMs } = options;
  const hasTimeout = typeof timeoutMs === 'number' && timeoutMs > 0;
  if (!signal && !hasTimeout) return work(new AbortController().signal);

  const inner = new AbortController();
  return new Promise<T>((resolve, reject) => {
    let timer: ReturnType<typeof setTimeout> | undefined;
    const cleanup = () => {
      if (timer !== undefined) clearTimeout(timer);
      signal?.removeEventListener('abort', onOuterAbort);
    };
    const fail = (error: Error) => {
      cleanup();
      inner.abort(error);
      reject(error);
    };
    const onOuterAbort = () => fail(makeAbortError(label));

    if (signal?.aborted) {
      onOuterAbort();
      return;
    }
    signal?.addEventListener('abort', onOuterAbort, { once: true });
    if (hasTimeout) timer = setTimeout(() => fail(makeTimeoutError(label, timeoutMs)), timeoutMs);

    work(inner.signal).then(
      (value) => {
        cleanup();
        resolve(value);
      },
      (error) => {
        cleanup();
        reject(error);
      }
    );
  });
}
//=============================================================================
