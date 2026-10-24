//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, expect, test, vi } from 'vitest';
//=============================================================================
const mocks = vi.hoisted(() => ({
  manifests: new Map<string, Record<string, unknown>>(),
  rpc: vi.fn(),
  status: vi.fn(),
}));
//=============================================================================
vi.mock('./library-loader.js', () => ({
  getDiscoveredManifests: () => mocks.manifests,
}));
//=============================================================================
vi.mock('./native-rpc.js', () => ({ nflowRpc: mocks.rpc }));
vi.mock('./status-service.js', () => ({ pushStatus: mocks.status }));
import { blockHelpTopic, openBlockHelp, openHelpTopic } from './block-help.js';
//=============================================================================
beforeEach(() => {
  mocks.manifests.clear();
  mocks.rpc.mockReset();
  mocks.status.mockReset();
});
//=============================================================================
test('builds a documentation topic from a block type', () => {
  expect(blockHelpTopic({ type: 'gain' })).toBe('block:gain');
  expect(blockHelpTopic({ type: 'dashboardScope' })).toBe('block:dashboardScope');
  expect(blockHelpTopic({ type: '' })).toBeNull();
  expect(blockHelpTopic({ type: "gain')" })).toBeNull();
});
//=============================================================================
test('resolves a namespaced type through its manifest', () => {
  mocks.manifests.set('builtin.math', { blocks: [{ type: 'gain' }] });
  expect(blockHelpTopic({ type: 'nflow.builtin.math.gain' })).toBe('block:gain');
});
//=============================================================================
test('opens block help through the dedicated RPC method', async () => {
  mocks.rpc.mockResolvedValue({ success: true, topic: 'block:gain' });

  await expect(openBlockHelp({ type: 'gain' })).resolves.toBe(true);

  expect(mocks.rpc).toHaveBeenCalledWith('nflow.help.open', { topic: 'block:gain' });
  expect(mocks.status).toHaveBeenCalledWith(
    expect.objectContaining({ level: 'success', message: 'Block help opened' })
  );
});
//=============================================================================
test('passes the canonical built-in help page for a discovered block', async () => {
  mocks.manifests.set('builtin.source', {
    _dir: 'source',
    blocks: [{ type: 'counterFreeRunning' }],
  });
  mocks.rpc.mockResolvedValue({ success: true, topic: 'block:counterFreeRunning' });

  await expect(openBlockHelp({ type: 'counterFreeRunning' })).resolves.toBe(true);

  expect(mocks.rpc).toHaveBeenCalledWith('nflow.help.open', {
    topic: 'block:counterFreeRunning',
    page: './nflow_blocks/source/counterFreeRunning.html',
  });
});
//=============================================================================
test('reports a rejected documentation request', async () => {
  mocks.rpc.mockRejectedValue(new Error('not available'));

  await expect(openBlockHelp({ type: 'gain' })).resolves.toBe(false);

  expect(mocks.status).toHaveBeenCalledWith(
    expect.objectContaining({ level: 'error', message: expect.stringContaining('not available') })
  );
});
//=============================================================================
test('opens a validated general help topic and rejects unsafe topics', async () => {
  mocks.rpc.mockResolvedValue({ success: true, topic: 'nflow_workspace' });

  await expect(openHelpTopic('nflow_workspace')).resolves.toBe(true);
  await expect(openHelpTopic("doc('unexpected')")).resolves.toBe(false);

  expect(mocks.rpc).toHaveBeenCalledTimes(1);
  expect(mocks.rpc).toHaveBeenCalledWith('nflow.help.open', { topic: 'nflow_workspace' });
});
//=============================================================================
