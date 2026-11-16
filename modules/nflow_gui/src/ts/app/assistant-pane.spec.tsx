//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Exercises the shared, transport-agnostic AssistantPane (hosted in the webview
// module, reused by NFlow) through the same `@nelson/assistant` import the pane
// uses. A fake backend replaces the live interpreter, so the full chat flow
// (prompt, streamed tokens, answer, Stop, model list) is covered with NO AI
// model and NO engine, which is what CI has.
import { expect, test, vi } from 'vitest';
import { createRoot } from 'react-dom/client';
import { act } from 'react-dom/test-utils';
import { AssistantPane, type AssistantBackend } from '@nelson/assistant';
//=============================================================================
const flush = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));
//=============================================================================
function makeFakeBackend(models: string[] = []): {
  backend: AssistantBackend;
  emit: (channel: string, payload: string) => void;
} {
  let emit: (channel: string, payload: string) => void = () => undefined;
  const backend: AssistantBackend = {
    listModels: vi.fn(async () => models),
    sendPrompt: vi.fn(async () => ({ accepted: true })),
    subscribe: vi.fn((handler) => {
      emit = handler;
      return () => undefined;
    }),
    interrupt: vi.fn(),
    runCode: vi.fn(),
  };
  return { backend, emit: (channel, payload) => emit(channel, payload) };
}
//=============================================================================
test('offers the ACP backend plus any provider the backend lists', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const { backend } = makeFakeBackend(['ollama:llama3']);

  await act(async () => {
    root.render(<AssistantPane backend={backend} />);
  });
  await act(async () => {
    await flush();
  });

  const options = Array.from(container.querySelectorAll('option')).map((o) => o.textContent);
  expect(options).toContain('Claude (ACP)');
  expect(options).toContain('ollama:llama3');

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
test('sends a prompt, streams tokens, then shows the final answer', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const { backend, emit } = makeFakeBackend();

  await act(async () => {
    root.render(<AssistantPane backend={backend} />);
  });
  await act(async () => {
    await flush();
  });

  // The empty state shows example chips; clicking one sends it as a prompt.
  const chip = Array.from(container.querySelectorAll('.assistant-panel__chip'))[0] as HTMLElement;
  expect(chip).toBeTruthy();
  const prompt = chip.textContent ?? '';
  await act(async () => {
    chip.dispatchEvent(new MouseEvent('click', { bubbles: true }));
  });

  expect(backend.sendPrompt).toHaveBeenCalledWith(prompt, expect.any(String));
  expect(container.textContent).toContain(prompt);
  expect(container.querySelector('.assistant-panel__busy')).toBeTruthy();

  await act(async () => {
    emit('token', JSON.stringify({ text: 'Hello ' }));
    emit('token', JSON.stringify({ text: 'world' }));
  });
  expect(container.querySelector('.assistant-panel__stream')?.textContent).toContain('Hello world');

  await act(async () => {
    emit('answer', JSON.stringify({ text: 'All done.' }));
  });
  const bubbles = Array.from(container.querySelectorAll('.assistant-panel__bubble'));
  expect(bubbles.some((b) => b.textContent?.includes('All done.'))).toBe(true);
  expect(container.querySelector('.assistant-panel__busy')).toBeNull();

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
test('Stop asks the backend to interrupt the running agent', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const { backend } = makeFakeBackend();

  await act(async () => {
    root.render(<AssistantPane backend={backend} />);
  });
  await act(async () => {
    await flush();
  });

  const chip = Array.from(container.querySelectorAll('.assistant-panel__chip'))[0] as HTMLElement;
  await act(async () => {
    chip.dispatchEvent(new MouseEvent('click', { bubbles: true }));
  });

  const stop = container.querySelector('.assistant-panel__stop') as HTMLElement;
  expect(stop).toBeTruthy();
  await act(async () => {
    stop.dispatchEvent(new MouseEvent('click', { bubbles: true }));
  });
  expect(backend.interrupt).toHaveBeenCalledTimes(1);
  // Stop returns the pane to idle at once, so it can never stay stuck in
  // "Thinking..." when no trailing 'error' event arrives from the backend.
  expect(container.querySelector('.assistant-panel__busy')).toBeNull();

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
test('Up recalls the last prompt and Down restores the live draft', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const { backend, emit } = makeFakeBackend();

  await act(async () => {
    root.render(<AssistantPane backend={backend} />);
  });
  await act(async () => {
    await flush();
  });

  // Send a prompt through the first example chip, then bring the pane back to
  // idle so the input is enabled again.
  const chip = Array.from(container.querySelectorAll('.assistant-panel__chip'))[0] as HTMLElement;
  const sent = chip.textContent ?? '';
  await act(async () => {
    chip.dispatchEvent(new MouseEvent('click', { bubbles: true }));
  });
  await act(async () => {
    emit('answer', JSON.stringify({ text: 'ok' }));
  });

  const textarea = container.querySelector('textarea') as HTMLTextAreaElement;
  expect(textarea.value).toBe('');

  // Arrow Up on the empty (first) line recalls the last sent prompt.
  await act(async () => {
    textarea.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowUp', bubbles: true }));
  });
  expect(textarea.value).toBe(sent);

  // Arrow Down past the newest entry restores the (empty) draft.
  await act(async () => {
    textarea.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown', bubbles: true }));
  });
  expect(textarea.value).toBe('');

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
test('a code action gets an ask helper that starts a new agent turn', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const { backend, emit } = makeFakeBackend();
  const run = vi.fn((_code: string, helpers: { ask: (p: string) => void }) => {
    helpers.ask('please fix it');
  });

  await act(async () => {
    root.render(<AssistantPane backend={backend} codeActions={[{ label: 'Build', run }]} />);
  });
  await act(async () => {
    await flush();
  });

  // An assistant answer with a code block renders the host code-action button.
  await act(async () => {
    emit('answer', JSON.stringify({ text: 'Here:\n```\ndisp(1)\n```' }));
  });
  const actionBtn = Array.from(
    container.querySelectorAll('.assistant-panel__code-actions button')
  ).find((b) => b.textContent === 'Build') as HTMLElement;
  expect(actionBtn).toBeTruthy();

  await act(async () => {
    actionBtn.dispatchEvent(new MouseEvent('click', { bubbles: true }));
  });

  // The action asked a follow-up, which the pane routed through send ->
  // backend.sendPrompt as a normal agent turn (auto-correction uses this).
  expect(run).toHaveBeenCalledTimes(1);
  expect(backend.sendPrompt).toHaveBeenCalledWith('please fix it', expect.any(String));

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
