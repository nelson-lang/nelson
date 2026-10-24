//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// For-each authoring in the subsystem inspector: the Iterations
// and Partition-input-ports fields read from / write to params.forEach, and
// clearing the iterations (<= 1) reverts the block to an ordinary subsystem.
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { renderSubsystem } from './subsystem.js';
//=============================================================================
const makeDeps = () => {
  const inspectorBody = document.createElement('div');
  document.body.appendChild(inspectorBody);
  return {
    inspectorBody,
    callUpdate: vi.fn(),
    callResize: vi.fn(),
    renderInspector: vi.fn(),
    signalDiagramChanged: vi.fn(),
  } as never as Parameters<typeof renderSubsystem>[1];
};
//=============================================================================
const makeBlock = (params: Record<string, unknown> = {}) =>
  ({
    type: 'subsystem',
    params: { externalInputs: ['in1', 'in2'], externalOutputs: ['out1'], ...params },
  } as never);
//=============================================================================
const bodyOf = (deps: unknown) => (deps as { inspectorBody: HTMLElement }).inspectorBody;
const updatesOf = (deps: unknown) => (deps as { callUpdate: ReturnType<typeof vi.fn> }).callUpdate;
describe('subsystem for-each inspector', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
  });

  it('pre-fills the fields from an existing params.forEach', () => {
    const deps = makeDeps();
    const block = makeBlock({ forEach: { numIterations: 4, partition: [0, 2] } });
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    const iter = body.querySelector<HTMLInputElement>('[data-inspector-foreach-iterations]');
    const part = body.querySelector<HTMLInputElement>('[data-inspector-foreach-partition]');
    expect(iter?.value).toBe('4');
    expect(part?.value).toBe('0, 2');
  });

  it('writes params.forEach when iterations and partition are edited', () => {
    const deps = makeDeps();
    const block = makeBlock();
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    const iter = body.querySelector<HTMLInputElement>('[data-inspector-foreach-iterations]')!;
    const part = body.querySelector<HTMLInputElement>('[data-inspector-foreach-partition]')!;
    iter.value = '3';
    iter.dispatchEvent(new Event('input'));
    part.value = '0';
    part.dispatchEvent(new Event('input'));
    expect((block as { params: { forEach?: unknown } }).params.forEach).toEqual({
      numIterations: 3,
      partition: [0],
    });
    expect(updatesOf(deps)).toHaveBeenCalled();
  });

  it('removes params.forEach when iterations drops to 1 (ordinary subsystem)', () => {
    const deps = makeDeps();
    const block = makeBlock({ forEach: { numIterations: 2, partition: [0] } });
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    const iter = body.querySelector<HTMLInputElement>('[data-inspector-foreach-iterations]')!;
    iter.value = '1';
    iter.dispatchEvent(new Event('input'));
    expect((block as { params: Record<string, unknown> }).params.forEach).toBeUndefined();
  });
});
//=============================================================================
describe('subsystem mask-parameters inspector', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
  });

  it('renders one row per existing mask parameter', () => {
    const deps = makeDeps();
    const block = makeBlock({ maskParameters: { K: 5, Ti: 0.2 } });
    renderSubsystem(block, deps);
    const rows = bodyOf(deps).querySelectorAll('.mask-param-row');
    expect(rows.length).toBe(2);
    const names = Array.from(
      bodyOf(deps).querySelectorAll<HTMLInputElement>('.mask-param-name')
    ).map((i) => i.value);
    expect(names).toContain('K');
    expect(names).toContain('Ti');
  });

  it('adds a parameter and writes params.maskParameters', () => {
    const deps = makeDeps();
    const block = makeBlock();
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    body.querySelector<HTMLButtonElement>('[data-inspector-mask-add]')!.click();
    const row = body.querySelector('.mask-param-row')!;
    const name = row.querySelector<HTMLInputElement>('.mask-param-name')!;
    const value = row.querySelector<HTMLInputElement>('.mask-param-value')!;
    name.value = 'Kp';
    name.dispatchEvent(new Event('input'));
    value.value = '2.5';
    value.dispatchEvent(new Event('input'));
    expect((block as { params: { maskParameters?: unknown } }).params.maskParameters).toEqual({
      Kp: 2.5,
    });
    expect(updatesOf(deps)).toHaveBeenCalled();
  });

  it('removes a parameter and drops maskParameters when the last row goes', () => {
    const deps = makeDeps();
    const block = makeBlock({ maskParameters: { K: 5 } });
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    body.querySelector<HTMLButtonElement>('.mask-param-remove')!.click();
    expect((block as { params: Record<string, unknown> }).params.maskParameters).toBeUndefined();
  });
});
//=============================================================================
