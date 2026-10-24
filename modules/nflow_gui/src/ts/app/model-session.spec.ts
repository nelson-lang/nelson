//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit tests for the live document-editor session front-end glue.
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
//=============================================================================
interface RpcCall {
  method: string;
  params: Record<string, unknown>;
}
//=============================================================================
const rpcCalls: RpcCall[] = [];
let rpcResponder: (method: string, params: Record<string, unknown>) => unknown = () => ({});
const pushHandlers: Record<string, Array<(p: unknown) => void>> = {};
vi.mock('./native-rpc.js', () => ({
  nflowRpc: async (method: string, params: Record<string, unknown>) => {
    rpcCalls.push({ method, params });
    return JSON.stringify(rpcResponder(method, params));
  },
  onNFlowPush: (channel: string, cb: (p: unknown) => void) => {
    (pushHandlers[channel] ||= []).push(cb);
    return () => {};
  },
  canUseNFlowRpc: () => true,
}));
//=============================================================================
import {
  getBoundModelName,
  isModelBound,
  fetchModelDiagram,
  commitModel,
  saveModel,
  initModelSession,
  flushPendingCommit,
} from './model-session.js';
//=============================================================================
beforeEach(() => {
  rpcCalls.length = 0;
  rpcResponder = () => ({});
  for (const k of Object.keys(pushHandlers)) delete pushHandlers[k];
  (window as unknown as { __nf_bootstrap?: unknown }).__nf_bootstrap = { modelName: 'demo' };
  (window as unknown as { __nfModelRevision?: number }).__nfModelRevision = 0;
});
//=============================================================================
describe('model-session identity', () => {
  it('reads the bound model name from the bootstrap', () => {
    expect(getBoundModelName()).toBe('demo');
    expect(isModelBound()).toBe(true);
  });
});
//=============================================================================
describe('model-session commit', () => {
  it('commits with the last applied revision and tracks the new revision', async () => {
    rpcResponder = (method) => {
      if (method === 'nflow.model.getDiagram') {
        return { success: true, revision: 3, filePath: 'C:/m.nflow', name: 'demo', content: '{}' };
      }
      if (method === 'nflow.model.commit') return { success: true, revision: 4 };
      return {};
    };
    // Loading establishes the base revision (3).
    const model = await fetchModelDiagram();
    expect(model?.revision).toBe(3);

    const outcome = await commitModel('{"blocks":[]}');
    expect(outcome).toBe('ok');
    const commit = rpcCalls.find((c) => c.method === 'nflow.model.commit');
    expect(commit?.params.baseRevision).toBe(3);
    expect(commit?.params.content).toBe('{"blocks":[]}');

    // The next commit uses the revision returned by the previous one (4).
    rpcResponder = () => ({ success: true, revision: 5 });
    await commitModel('{"blocks":[]}');
    const commit2 = rpcCalls.filter((c) => c.method === 'nflow.model.commit')[1];
    expect(commit2?.params.baseRevision).toBe(4);
  });

  it('reports a conflict without advancing the applied revision blindly', async () => {
    rpcResponder = () => ({ success: false, conflict: true, revision: 9 });
    const outcome = await commitModel('{}');
    expect(outcome).toBe('conflict');
  });
});
//=============================================================================
describe('model-session save', () => {
  it('signals needPath when the document has no file target', async () => {
    rpcResponder = () => ({ success: false, needPath: true });
    const res = await saveModel('{}');
    expect(res['needPath']).toBe(true);
  });

  it('passes the path and content to the host save', async () => {
    rpcResponder = () => ({ success: true, filePath: 'C:/x.nflow', revision: 2 });
    const res = await saveModel('{"a":1}', 'C:/x.nflow');
    expect(res['success']).toBe(true);
    const save = rpcCalls.find((c) => c.method === 'nflow.model.save');
    expect(save?.params.path).toBe('C:/x.nflow');
    expect(save?.params.content).toBe('{"a":1}');
  });
});
//=============================================================================
describe('model-session live wiring', () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  it('debounces a commit on diagramChanged and flushes on demand', async () => {
    vi.useFakeTimers();
    rpcResponder = () => ({ success: true, revision: 1 });
    initModelSession({ serialize: () => '{"edited":true}', onModelChanged: () => {} });

    window.dispatchEvent(new Event('diagramChanged'));
    window.dispatchEvent(new Event('diagramChanged'));
    expect(rpcCalls.filter((c) => c.method === 'nflow.model.commit')).toHaveLength(0);

    await vi.advanceTimersByTimeAsync(300);
    const commits = rpcCalls.filter((c) => c.method === 'nflow.model.commit');
    expect(commits).toHaveLength(1);
    expect(commits[0].params.content).toBe('{"edited":true}');

    vi.useRealTimers();
    await flushPendingCommit();
    expect(rpcCalls.filter((c) => c.method === 'nflow.model.commit')).toHaveLength(2);
  });

  it('pulls on a script-origin change and ignores its own gui echo', async () => {
    const onModelChanged = vi.fn();
    initModelSession({ serialize: () => '{}', onModelChanged });

    const handlers = pushHandlers['nflow.model.changed'] || [];
    expect(handlers.length).toBeGreaterThan(0);
    for (const h of handlers) h({ origin: 'gui' });
    expect(onModelChanged).not.toHaveBeenCalled();
    for (const h of handlers) h({ origin: 'script' });
    expect(onModelChanged).toHaveBeenCalledTimes(1);
  });

  it('pushes the current block to the host on a GUI selection', async () => {
    initModelSession({ serialize: () => '{}', onModelChanged: () => {} });
    document.dispatchEvent(new CustomEvent('nf-block-selected', { detail: { blockId: 'Gain' } }));
    await Promise.resolve();
    const call = rpcCalls.find((c) => c.method === 'nflow.context.set');
    expect(call?.params.name).toBe('demo');
    expect(call?.params.block).toBe('demo/Gain');
    rpcCalls.length = 0;
    document.dispatchEvent(new CustomEvent('nf-block-selected', { detail: { blockId: null } }));
    await Promise.resolve();
    const clear = rpcCalls.find((c) => c.method === 'nflow.context.set');
    expect(clear?.params.block).toBe('');
  });
});
//=============================================================================
