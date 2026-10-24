//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// For / While Iterator authoring in the subsystem inspector: the
// Iteration fieldset reads from / writes to params.controlPort (forIterator /
// whileIterator). Choosing "None" clears only an iterator controlPort and leaves
// any other kind (enable, trigger, ...) untouched.
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
    params: { externalInputs: ['in1'], externalOutputs: ['out1'], ...params },
  } as never);
//=============================================================================
const bodyOf = (deps: unknown) => (deps as { inspectorBody: HTMLElement }).inspectorBody;
const updatesOf = (deps: unknown) => (deps as { callUpdate: ReturnType<typeof vi.fn> }).callUpdate;
const paramsOf = (block: unknown) => (block as { params: Record<string, unknown> }).params;
describe('subsystem iterator inspector', () => {
  beforeEach(() => {
    document.body.innerHTML = '';
  });

  it('pre-fills the fields from an existing For iterator controlPort', () => {
    const deps = makeDeps();
    const block = makeBlock({
      controlPort: { kind: 'forIterator', IterationSource: 'internal', IterationLimit: 5 },
    });
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    expect(body.querySelector<HTMLSelectElement>('[data-inspector-iterator-mode]')?.value).toBe(
      'for'
    );
    expect(body.querySelector<HTMLInputElement>('[data-inspector-iterator-limit]')?.value).toBe(
      '5'
    );
  });

  it('pre-fills the fields from an existing While iterator controlPort', () => {
    const deps = makeDeps();
    const block = makeBlock({ controlPort: { kind: 'whileIterator', MaxIterations: 42 } });
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    expect(body.querySelector<HTMLSelectElement>('[data-inspector-iterator-mode]')?.value).toBe(
      'while'
    );
    expect(body.querySelector<HTMLInputElement>('[data-inspector-iterator-max]')?.value).toBe('42');
  });

  it('writes a For iterator controlPort (internal) when the limit is edited', () => {
    const deps = makeDeps();
    const block = makeBlock();
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    const mode = body.querySelector<HTMLSelectElement>('[data-inspector-iterator-mode]')!;
    mode.value = 'for';
    mode.dispatchEvent(new Event('change'));
    const limit = body.querySelector<HTMLInputElement>('[data-inspector-iterator-limit]')!;
    limit.value = '4';
    limit.dispatchEvent(new Event('input'));
    expect(paramsOf(block).controlPort).toEqual({
      kind: 'forIterator',
      IterationSource: 'internal',
      IterationLimit: 4,
    });
    expect(updatesOf(deps)).toHaveBeenCalled();
  });

  it('omits IterationLimit for an external iteration source', () => {
    const deps = makeDeps();
    const block = makeBlock();
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    const mode = body.querySelector<HTMLSelectElement>('[data-inspector-iterator-mode]')!;
    mode.value = 'for';
    mode.dispatchEvent(new Event('change'));
    const source = body.querySelector<HTMLSelectElement>('[data-inspector-iterator-source]')!;
    source.value = 'external';
    source.dispatchEvent(new Event('change'));
    expect(paramsOf(block).controlPort).toEqual({
      kind: 'forIterator',
      IterationSource: 'external',
    });
  });

  it('writes a While iterator controlPort with a MaxIterations cap', () => {
    const deps = makeDeps();
    const block = makeBlock();
    renderSubsystem(block, deps);
    const body = bodyOf(deps);
    const mode = body.querySelector<HTMLSelectElement>('[data-inspector-iterator-mode]')!;
    mode.value = 'while';
    mode.dispatchEvent(new Event('change'));
    const max = body.querySelector<HTMLInputElement>('[data-inspector-iterator-max]')!;
    max.value = '25';
    max.dispatchEvent(new Event('input'));
    expect(paramsOf(block).controlPort).toEqual({ kind: 'whileIterator', MaxIterations: 25 });
  });

  it('clears an iterator controlPort when the mode is set to None', () => {
    const deps = makeDeps();
    const block = makeBlock({ controlPort: { kind: 'forIterator', IterationLimit: 3 } });
    renderSubsystem(block, deps);
    const mode = bodyOf(deps).querySelector<HTMLSelectElement>('[data-inspector-iterator-mode]')!;
    mode.value = 'none';
    mode.dispatchEvent(new Event('change'));
    expect(paramsOf(block).controlPort).toBeUndefined();
  });

  it('leaves a non-iterator controlPort (enable) untouched when None is chosen', () => {
    const deps = makeDeps();
    const block = makeBlock({ controlPort: { kind: 'enable' } });
    renderSubsystem(block, deps);
    const mode = bodyOf(deps).querySelector<HTMLSelectElement>('[data-inspector-iterator-mode]')!;
    expect(mode.value).toBe('none');
    mode.dispatchEvent(new Event('change'));
    expect(paramsOf(block).controlPort).toEqual({ kind: 'enable' });
  });
});
//=============================================================================
