//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests for status-service
//=============================================================================
import { vi } from 'vitest';
//=============================================================================
describe('status-service', () => {
  let pushStatus: (item: any) => string;
  let clearStatus: (id?: string) => void;
  let subscribeStatus: (fn: (item: any) => void) => () => void;
  let setStatusElement: (el: HTMLElement | null) => void;
  let setProgressElement: (el: HTMLElement | null) => void;
  let getCurrentStatus: () => any;
  let _resetForTests: () => void;

  beforeAll(async () => {
    const mod = await import('./status-service.ts');
    pushStatus = mod.pushStatus;
    clearStatus = mod.clearStatus;
    subscribeStatus = mod.subscribeStatus;
    setStatusElement = mod.setStatusElement;
    setProgressElement = mod.setProgressElement;
    getCurrentStatus = mod.getCurrentStatus;
    _resetForTests = mod._resetForTests;
  });

  beforeEach(() => {
    _resetForTests();
  });

  test('pushStatus returns a non-empty id string', () => {
    const id = pushStatus({ level: 'info', message: 'Hello' });
    expect(typeof id).toBe('string');
    expect(id.length).toBeGreaterThan(0);
  });

  test('getCurrentStatus reflects last pushed item', () => {
    pushStatus({ level: 'info', message: 'First' });
    pushStatus({ level: 'success', message: 'Second' });
    expect(getCurrentStatus()?.message).toBe('Second');
    expect(getCurrentStatus()?.level).toBe('success');
  });

  test('pushStatus sets createdAt close to Date.now()', () => {
    const before = Date.now();
    pushStatus({ level: 'info', message: 'Timestamp test' });
    const after = Date.now();
    expect(getCurrentStatus()?.createdAt).toBeGreaterThanOrEqual(before);
    expect(getCurrentStatus()?.createdAt).toBeLessThanOrEqual(after);
  });

  test('clearStatus with no argument clears the current status', () => {
    pushStatus({ level: 'info', message: 'to clear' });
    clearStatus();
    expect(getCurrentStatus()).toBeNull();
  });

  test('clearStatus with correct id clears the current status', () => {
    const id = pushStatus({ level: 'warn', message: 'warn message' });
    clearStatus(id);
    expect(getCurrentStatus()).toBeNull();
  });

  test('clearStatus with wrong id does NOT clear the current status', () => {
    pushStatus({ level: 'info', message: 'keep me' });
    clearStatus('nonexistent-id');
    expect(getCurrentStatus()?.message).toBe('keep me');
  });

  test('subscriber is called immediately with current status (null)', () => {
    const calls: any[] = [];
    subscribeStatus((item) => calls.push(item));
    expect(calls).toHaveLength(1);
    expect(calls[0]).toBeNull();
  });

  test('subscriber is called immediately with current item if one exists', () => {
    pushStatus({ level: 'info', message: 'Pre-existing' });
    const calls: any[] = [];
    subscribeStatus((item) => calls.push(item));
    expect(calls[0]?.message).toBe('Pre-existing');
  });

  test('subscriber is invoked on each push', () => {
    const calls: any[] = [];
    subscribeStatus((item) => calls.push(item));
    pushStatus({ level: 'info', message: 'A' });
    pushStatus({ level: 'error', message: 'B' });
    expect(calls).toHaveLength(3);
    expect(calls[1]?.message).toBe('A');
    expect(calls[2]?.message).toBe('B');
  });

  test('unsubscribe stops receiving updates', () => {
    const calls: any[] = [];
    const unsub = subscribeStatus((item) => calls.push(item));
    unsub();
    pushStatus({ level: 'info', message: 'after unsub' });
    expect(calls).toHaveLength(1);
  });

  test('subscriber is called on clearStatus', () => {
    pushStatus({ level: 'info', message: 'transient' });
    const calls: any[] = [];
    subscribeStatus((item) => calls.push(item));
    clearStatus();
    expect(calls[calls.length - 1]).toBeNull();
  });

  test('setStatusElement writes the pushed message to the DOM element', () => {
    const el = document.createElement('span');
    setStatusElement(el);
    pushStatus({ level: 'info', message: 'Visible text' });
    expect(el.textContent).toBe('Visible text');
  });

  test('setStatusElement clears the DOM element on clearStatus', () => {
    const el = document.createElement('span');
    setStatusElement(el);
    pushStatus({ level: 'info', message: 'some text' });
    clearStatus();
    expect(el.textContent).toBe('');
  });

  test('setStatusElement syncs existing status immediately on registration', () => {
    pushStatus({ level: 'success', message: 'Already here' });
    const el = document.createElement('span');
    setStatusElement(el);
    expect(el.textContent).toBe('Already here');
  });

  test('setProgressElement shows element and updates aria-valuenow on progress push', () => {
    const el = document.createElement('div');
    el.setAttribute('hidden', '');
    setProgressElement(el);
    pushStatus({ level: 'progress', message: 'Running...', progress: 42 });
    expect(el.hasAttribute('hidden')).toBe(false);
    expect(el.getAttribute('aria-valuenow')).toBe('42');
  });

  test('setProgressElement hides element when status is not progress', () => {
    const el = document.createElement('div');
    setProgressElement(el);
    pushStatus({ level: 'progress', message: 'Running...', progress: 80 });
    pushStatus({ level: 'success', message: 'Done' });
    expect(el.hasAttribute('hidden')).toBe(true);
  });

  test('setProgressElement hides element when status is cleared', () => {
    const el = document.createElement('div');
    setProgressElement(el);
    pushStatus({ level: 'progress', message: 'Running...', progress: 50 });
    clearStatus();
    expect(el.hasAttribute('hidden')).toBe(true);
  });

  test('status auto-clears after durationMs', async () => {
    vi.useFakeTimers();
    pushStatus({ level: 'info', message: 'Transient', durationMs: 500 });
    expect(getCurrentStatus()?.message).toBe('Transient');
    vi.advanceTimersByTime(500);
    expect(getCurrentStatus()).toBeNull();
    vi.useRealTimers();
  });

  test('newer push cancels auto-clear of previous item', async () => {
    vi.useFakeTimers();
    pushStatus({ level: 'info', message: 'First', durationMs: 200 });
    pushStatus({ level: 'info', message: 'Second', durationMs: 1000 });
    vi.advanceTimersByTime(300);
    // First would have expired at 200ms, but Second replaced it
    expect(getCurrentStatus()?.message).toBe('Second');
    vi.useRealTimers();
  });
});
//=============================================================================
describe('writeStatus dual-write helper', () => {
  let writeStatus: (
    el: HTMLElement | null | undefined,
    message: string,
    level?: string,
    durationMs?: number | null
  ) => void;
  let inferStatusLevel: (message: string) => string;
  let subscribeStatus: (fn: (item: any) => void) => () => void;
  let _resetForTests: () => void;

  beforeAll(async () => {
    const mod = await import('./status-service.ts');
    writeStatus = mod.writeStatus as any;
    inferStatusLevel = mod.inferStatusLevel;
    subscribeStatus = mod.subscribeStatus;
    _resetForTests = mod._resetForTests;
  });

  beforeEach(() => _resetForTests());

  test('writes to the given element AND notifies subscribers', () => {
    const el = document.createElement('div');
    const seen: any[] = [];
    const unsub = subscribeStatus((item) => item && seen.push(item));
    writeStatus(el, 'hello');
    unsub();
    expect(el.textContent).toBe('hello');
    expect(seen.length).toBe(1);
    expect(seen[0].message).toBe('hello');
  });

  test('notifies subscribers even without an element', () => {
    const seen: any[] = [];
    const unsub = subscribeStatus((item) => item && seen.push(item));
    writeStatus(null, 'no element');
    unsub();
    expect(seen.length).toBe(1);
    expect(seen[0].message).toBe('no element');
  });

  test('explicit level wins over inference', () => {
    const seen: any[] = [];
    const unsub = subscribeStatus((item) => item && seen.push(item));
    writeStatus(null, 'Save error: boom', 'warn');
    unsub();
    expect(seen[0].level).toBe('warn');
  });

  test('inferStatusLevel classifies common messages (en/fr)', () => {
    expect(inferStatusLevel('Save error: x')).toBe('error');
    expect(inferStatusLevel('Erreur de chargement')).toBe('error');
    expect(inferStatusLevel('Build failed')).toBe('error');
    expect(inferStatusLevel('Warning: something')).toBe('warn');
    expect(inferStatusLevel('Saved: model.nflow')).toBe('success');
    expect(inferStatusLevel('Build succeeded')).toBe('success');
    expect(inferStatusLevel('Running...')).toBe('info');
  });
});
//=============================================================================
