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
import { createResponseOrder, withFigureSlot } from '../../src/desktop/figureQueue';
//=============================================================================
function deferred<T>() {
  let resolve: (value: T) => void = () => undefined;
  const promise = new Promise<T>((r) => (resolve = r));
  return { promise, resolve };
}
//=============================================================================
describe('figure request ordering', () => {
  it('drops a response that lands after a later-issued one', async () => {
    const order = createResponseOrder();
    const applied: string[] = [];
    const older = deferred<string>();
    const newer = deferred<string>();
    const doneOld = older.promise.then(order((v: string) => applied.push(v)));
    const doneNew = newer.promise.then(order((v: string) => applied.push(v)));

    newer.resolve('new');
    await doneNew;
    older.resolve('old');
    await doneOld;

    expect(applied).toEqual(['new']);
  });

  it('applies in-order responses', () => {
    const order = createResponseOrder();
    const apply = vi.fn();
    const first = order(apply);
    const second = order(apply);
    first('a');
    second('b');
    expect(apply.mock.calls).toEqual([['a'], ['b']]);
  });

  it('keeps at most two figure requests in flight', async () => {
    let inFlight = 0;
    let peak = 0;
    const gates = [deferred<void>(), deferred<void>(), deferred<void>()];
    const runs = gates.map((gate) =>
      withFigureSlot(async () => {
        inFlight++;
        peak = Math.max(peak, inFlight);
        await gate.promise;
        inFlight--;
      })
    );
    await Promise.resolve();
    gates.forEach((gate) => gate.resolve());
    await Promise.all(runs);
    expect(peak).toBe(2);
  });
});
//=============================================================================
