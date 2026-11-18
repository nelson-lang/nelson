//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit tests for the NFlow assistant backend adapter. The native bridge is
// mocked, so these run with NO AI model and NO engine (what CI has): they pin
// the transport contract (prompt escaping + skill pin, Stop, code run, event
// mapping, model round-trip over the push channel).
import { afterEach, beforeEach, expect, test, vi } from 'vitest';
//=============================================================================
type PushHandler = (payload: unknown) => void;
let pushHandlers: PushHandler[] = [];
const nflowRpc = vi.fn(async () => ({}));
const onNFlowPush = vi.fn((_channel: string, cb: PushHandler) => {
  pushHandlers.push(cb);
  return () => {
    pushHandlers = pushHandlers.filter((h) => h !== cb);
  };
});
//=============================================================================
vi.mock('./native-rpc.js', () => ({
  nflowRpc: (...args: unknown[]) => nflowRpc(...(args as [])),
  onNFlowPush: (channel: string, cb: PushHandler) => onNFlowPush(channel, cb),
}));
// The live block catalog is built elsewhere; mock it so these transport tests stay
// engine-free. catalogRef is hoisted so the factory (hoisted above imports) can read it.
const { catalogRef } = vi.hoisted(() => ({ catalogRef: { text: '' } }));
vi.mock('./block-catalog.js', () => ({ buildBlockCatalog: () => catalogRef.text }));
//=============================================================================
// Imported after the mock is registered.
import { nflowAssistantBackend } from './assistant-backend.js';
//=============================================================================
beforeEach(() => {
  pushHandlers = [];
  nflowRpc.mockClear();
  onNFlowPush.mockClear();
  nflowRpc.mockResolvedValue({});
  catalogRef.text = '';
});
afterEach(() => {
  vi.useRealTimers();
});
//=============================================================================
test('sendPrompt tags the run with the nflow surface and escapes the prompt', async () => {
  const result = await nflowAssistantBackend.sendPrompt("it's rlc", 'claude-agent-acp');
  expect(result).toEqual({ accepted: true });
  expect(nflowRpc).toHaveBeenCalledTimes(1);
  const [method, params] = nflowRpc.mock.calls[0] as [string, { code: string }];
  expect(method).toBe('nflow.callback.invoke');
  // The surface id selects the NFlow guide engine-side (no process-wide env pin
  // that would leak to the Qt dock / web pane). Single quotes in the prompt are
  // doubled for the Nelson char literal; the 'external' driver stops the engine
  // from starting a Nelson timer (the pane drives the poll through step()).
  expect(params.code).toContain(
    "nelson.ai.internal.assistantRun('it''s rlc', 'claude-agent-acp', 'external', 'nflow')"
  );
  expect(params.code).not.toContain('NELSON_AI_CONTEXT_SKILL');
});
//=============================================================================
test('sendPrompt prepends the live block catalog, keeping the user text', async () => {
  catalogRef.text = 'Live block catalog (...): - nflow/source/constant {Value} 0in/1out';
  await nflowAssistantBackend.sendPrompt('rlc please', 'claude-agent-acp');
  const [, params] = nflowRpc.mock.calls[0] as [string, { code: string }];
  // The catalog and the user request both reach the model prompt, delimited.
  expect(params.code).toContain('Live block catalog');
  expect(params.code).toContain('nflow/source/constant {Value} 0in/1out');
  expect(params.code).toContain('--- Request: rlc please');
});
//=============================================================================
test('sendPrompt falls back to the ACP backend when no model is given', async () => {
  await nflowAssistantBackend.sendPrompt('hello', '');
  const [, params] = nflowRpc.mock.calls[0] as [string, { code: string }];
  expect(params.code).toContain("'claude-agent-acp'");
});
//=============================================================================
test('interrupt raises the per-pane assistant stop', async () => {
  nflowAssistantBackend.interrupt();
  expect(nflowRpc).toHaveBeenCalledWith('nflow.ai.stop', {});
});
//=============================================================================
test('runCode runs the snippet through the callback bridge', () => {
  nflowAssistantBackend.runCode('disp(42)');
  expect(nflowRpc).toHaveBeenCalledWith('nflow.callback.invoke', {
    code: 'disp(42)',
    blockId: '',
  });
});
//=============================================================================
test('step advances the ACP poll for the nflow surface through the bridge', () => {
  nflowAssistantBackend.step?.();
  expect(nflowRpc).toHaveBeenCalledWith('nflow.callback.invoke', {
    code: "nelson.ai.internal.assistantStep('nflow');",
    blockId: '',
  });
});
//=============================================================================
test('subscribe maps push events to (channel, payload) and ignores malformed ones', () => {
  const handler = vi.fn();
  nflowAssistantBackend.subscribe(handler);
  expect(onNFlowPush).toHaveBeenCalledWith('assistant', expect.any(Function));

  pushHandlers[0]({ channel: 'token', payload: '{"text":"hi"}' });
  expect(handler).toHaveBeenCalledWith('token', '{"text":"hi"}');

  handler.mockClear();
  pushHandlers[0]({ payload: 'no channel' });
  expect(handler).not.toHaveBeenCalled();
});
//=============================================================================
test('listModels returns the providers the engine reports over the push channel', async () => {
  const pending = nflowAssistantBackend.listModels();
  // The adapter triggers a models query, then waits for the 'models' event.
  expect(nflowRpc).toHaveBeenCalledWith(
    'nflow.callback.invoke',
    expect.objectContaining({ code: expect.stringContaining('nelson.ai.models()') })
  );
  pushHandlers.forEach((h) => h({ channel: 'models', payload: '["ollama:llama3"]' }));
  await expect(pending).resolves.toEqual(['ollama:llama3']);
});
//=============================================================================
test('listModels resolves empty when no provider answers in time', async () => {
  vi.useFakeTimers();
  const pending = nflowAssistantBackend.listModels();
  await vi.advanceTimersByTimeAsync(5000);
  await expect(pending).resolves.toEqual([]);
});
//=============================================================================
