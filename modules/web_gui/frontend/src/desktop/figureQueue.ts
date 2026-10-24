//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Serialises figure draw-list requests.
//
// Every figure panel fetches its own draw list, and a surface figure's list can
// be around a megabyte. Restoring a session with dozens of figures fired them
// all at once: each request pins a server worker while the engine renders on
// its single interpreter thread, and the combined payloads were enough to take
// the engine down. Limiting how many are in flight keeps peak memory bounded
// and the figures still appear promptly, one after another.
import { rpc } from '../rpc/client';
import type { FigureDisplayListResult } from '../rpc/types';
//=============================================================================
const MAX_IN_FLIGHT = 2;
let inFlight = 0;
const waiting: (() => void)[] = [];
async function acquire(): Promise<void> {
  if (inFlight < MAX_IN_FLIGHT) {
    inFlight++;
    return;
  }
  await new Promise<void>((resolve) => waiting.push(resolve));
  inFlight++;
}
//=============================================================================
function release(): void {
  inFlight--;
  const next = waiting.shift();
  if (next) next();
}
//=============================================================================
export async function withFigureSlot<T>(run: () => Promise<T>): Promise<T> {
  await acquire();
  try {
    return await run();
  } finally {
    release();
  }
}
//=============================================================================
export function fetchDisplayList(params: {
  handle: number;
  softwareFallback?: boolean;
}): Promise<FigureDisplayListResult> {
  return withFigureSlot(() => rpc<FigureDisplayListResult>('figure.getDisplayList', params));
}
//=============================================================================
// With two requests in flight, draw lists can come back out of order. Each
// request takes a ticket when it is issued: `order(apply)` returns a callback
// that applies a response only if no later-issued one has been applied yet.
export function createResponseOrder(): <T>(apply: (value: T) => void) => (value: T) => void {
  let issued = 0;
  let applied = 0;
  return <T>(apply: (value: T) => void) => {
    const ticket = ++issued;
    return (value: T) => {
      if (ticket < applied) return;
      applied = ticket;
      apply(value);
    };
  };
}
//=============================================================================
