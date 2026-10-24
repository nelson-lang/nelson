//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { expect, test, vi } from 'vitest';
import { createSimController } from './sim-controller.js';
import type { SimPhase } from './sim-controller.js';
//=============================================================================
// A run that only settles when its signal aborts or `finish` is called.
function controllableRun() {
  let finish: (value: unknown) => void = () => undefined;
  let signal: AbortSignal | null = null;
  const run = vi.fn((s: AbortSignal) => {
    signal = s;
    return new Promise((resolve) => {
      finish = resolve;
      s.addEventListener('abort', () => resolve({ status: 'stopped' }), { once: true });
    });
  });
  return { run, finish: (v: unknown) => finish(v), signal: () => signal };
}
//=============================================================================
function makeController(run: (signal: AbortSignal) => Promise<unknown>) {
  const rpc = vi.fn(async () => undefined);
  const controller = createSimController({ run, rpc });
  const phases: SimPhase[] = [];
  controller.subscribe((phase) => phases.push(phase));
  return { controller, rpc, phases };
}
//=============================================================================
test('a run goes idle -> running -> idle and resolves with the run result', async () => {
  const sim = controllableRun();
  const { controller, phases } = makeController(sim.run);

  const running = controller.start();
  expect(controller.phase).toBe('running');
  sim.finish({ status: 'done' });

  await expect(running).resolves.toEqual({ status: 'done' });
  expect(controller.phase).toBe('idle');
  expect(phases).toEqual(['running', 'idle']);
});
//=============================================================================
test('start is ignored while a run is in flight', async () => {
  const sim = controllableRun();
  const { controller } = makeController(sim.run);

  controller.start();
  expect(controller.start()).toBeNull();
  await Promise.resolve();
  expect(sim.run).toHaveBeenCalledTimes(1);
  sim.finish({ status: 'done' });
});
//=============================================================================
test('togglePause pauses then resumes through the engine', async () => {
  const sim = controllableRun();
  const { controller, rpc, phases } = makeController(sim.run);
  controller.start();

  await controller.togglePause();
  expect(controller.phase).toBe('paused');
  await controller.togglePause();
  expect(controller.phase).toBe('running');

  expect(rpc.mock.calls).toEqual([
    ['nflow.sim.pause', { pause: true }],
    ['nflow.sim.pause', { pause: false }],
  ]);
  expect(phases).toEqual(['running', 'paused', 'running']);
  sim.finish({ status: 'done' });
});
//=============================================================================
test('togglePause does nothing when idle', async () => {
  const { controller, rpc } = makeController(controllableRun().run);
  await controller.togglePause();
  expect(controller.phase).toBe('idle');
  expect(rpc).not.toHaveBeenCalled();
});
//=============================================================================
test('stop asks the engine, aborts the run and resolves once it settled', async () => {
  const sim = controllableRun();
  const { controller, rpc, phases } = makeController(sim.run);
  const running = controller.start();
  await Promise.resolve();

  await expect(controller.stop()).resolves.toBe(true);

  expect(rpc).toHaveBeenCalledWith('nflow.sim.stop', undefined);
  expect(sim.signal()!.aborted).toBe(true);
  await expect(running).resolves.toEqual({ status: 'stopped' });
  expect(controller.phase).toBe('idle');
  expect(phases).toEqual(['running', 'stopping', 'idle']);
});
//=============================================================================
test('stop from pause and concurrent stops settle together', async () => {
  const sim = controllableRun();
  const { controller, rpc } = makeController(sim.run);
  controller.start();
  await controller.togglePause();

  const [first, second] = await Promise.all([controller.stop(), controller.stop()]);

  expect(first).toBe(true);
  expect(second).toBe(true);
  expect(rpc.mock.calls.filter(([m]) => m === 'nflow.sim.stop')).toHaveLength(1);
  expect(controller.phase).toBe('idle');
});
//=============================================================================
test('stop when idle reports that nothing was running', async () => {
  const { controller, rpc } = makeController(controllableRun().run);
  await expect(controller.stop()).resolves.toBe(false);
  expect(rpc).not.toHaveBeenCalled();
});
//=============================================================================
test('an engine control failure does not block stop', async () => {
  const sim = controllableRun();
  const controller = createSimController({
    run: sim.run,
    rpc: vi.fn(async () => {
      throw new Error('engine gone');
    }),
  });
  controller.start();

  await expect(controller.stop()).resolves.toBe(true);
  expect(controller.phase).toBe('idle');
});
//=============================================================================
test('a failing run returns the controller to idle', async () => {
  const { controller } = makeController(async () => {
    throw new Error('boom');
  });

  await expect(controller.start()).rejects.toThrow('boom');
  expect(controller.phase).toBe('idle');
});
//=============================================================================
test('unsubscribe stops phase notifications', async () => {
  const sim = controllableRun();
  const controller = createSimController({ run: sim.run, rpc: vi.fn(async () => undefined) });
  const listener = vi.fn();
  const off = controller.subscribe(listener);
  off();

  controller.start();
  sim.finish({ status: 'done' });
  await Promise.resolve();
  expect(listener).not.toHaveBeenCalled();
});
//=============================================================================
