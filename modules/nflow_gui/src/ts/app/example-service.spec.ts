//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, describe, it, expect, vi } from 'vitest';
//=============================================================================
const mocks = vi.hoisted(() => ({ rpc: vi.fn() }));
vi.mock('./native-rpc.js', () => ({
  canUseNFlowRpc: vi.fn(() => true),
  nflowRpc: mocks.rpc,
}));
//=============================================================================
import { listExamples } from './example-service.js';
//=============================================================================
beforeEach(() => {
  mocks.rpc.mockReset();
});
//=============================================================================
describe('listExamples', () => {
  it('returns the unified catalog and shields missing fields', async () => {
    mocks.rpc.mockResolvedValue({
      groups: [{ name: 'nflow:causal', title: 'Causal', source: 'nflow', examples: [] }],
    });
    const catalog = await listExamples({ refresh: true });
    expect(catalog.groups).toHaveLength(1);
    expect(catalog.groups[0].title).toBe('Causal');
    expect(catalog.diagnostics).toEqual([]);
    expect(catalog.locale).toBe('en_US');
    expect(mocks.rpc).toHaveBeenCalledWith('nflow.examples.list', {});
  });

  it('caches the catalog until a refresh is requested', async () => {
    mocks.rpc.mockResolvedValue({ groups: [], diagnostics: [], locale: 'fr_FR' });
    const first = await listExamples({ refresh: true });
    const second = await listExamples();
    expect(second).toBe(first);
    expect(first.locale).toBe('fr_FR');
    expect(mocks.rpc).toHaveBeenCalledTimes(1);
    await listExamples({ refresh: true });
    expect(mocks.rpc).toHaveBeenCalledTimes(2);
  });

  it('returns an empty catalog when the backend rejects', async () => {
    mocks.rpc.mockRejectedValue(new Error('nope'));
    const catalog = await listExamples({ refresh: true });
    expect(catalog).toEqual({ groups: [], diagnostics: [], locale: 'en_US' });
  });
});
//=============================================================================
