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
import { emitAppEvent, onAppEvent } from './app-events.js';
//=============================================================================
test('onAppEvent receives the typed detail of emitAppEvent', () => {
  const handler = vi.fn();
  const off = onAppEvent('nf-sim-time', handler);
  emitAppEvent('nf-sim-time', { time: 2, progress: 20, done: false });
  off();
  expect(handler).toHaveBeenCalledWith({ time: 2, progress: 20, done: false });
});
//=============================================================================
test('events without detail pass undefined', () => {
  const handler = vi.fn();
  const off = onAppEvent('nf-sim-start', handler);
  emitAppEvent('nf-sim-start');
  off();
  expect(handler).toHaveBeenCalledWith(undefined);
});
//=============================================================================
test('unsubscribe stops delivery', () => {
  const handler = vi.fn();
  const off = onAppEvent('nf-viewbox-changed', handler);
  off();
  emitAppEvent('nf-viewbox-changed');
  expect(handler).not.toHaveBeenCalled();
});
//=============================================================================
test('once listeners fire a single time', () => {
  const handler = vi.fn();
  onAppEvent('nf-ready', handler, { once: true });
  emitAppEvent('nf-ready');
  emitAppEvent('nf-ready');
  expect(handler).toHaveBeenCalledTimes(1);
});
//=============================================================================
test('the bus uses document so plain DOM listeners and dispatchers interoperate', () => {
  const domListener = vi.fn();
  document.addEventListener('nf-libraries-ready', domListener);
  emitAppEvent('nf-libraries-ready');
  document.removeEventListener('nf-libraries-ready', domListener);
  expect(domListener).toHaveBeenCalledTimes(1);

  const handler = vi.fn();
  const off = onAppEvent('nf-sim-state', handler);
  document.dispatchEvent(new CustomEvent('nf-sim-state', { detail: { running: true } }));
  off();
  expect(handler).toHaveBeenCalledWith({ running: true });
});
//=============================================================================
