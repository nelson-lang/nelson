//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { act } from 'react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { createRoot, type Root } from 'react-dom/client';
import { ConsolePanel } from '../../src/panels/ConsolePanel';
import { sendToConsoleInput } from '../../src/desktop/consoleBus';
//=============================================================================
type Call = (name: string, payload: string) => Promise<string>;
const g = window as unknown as { webui?: { call: Call } };
const mounted: Array<{ root: Root; host: HTMLDivElement }> = [];
let promptToken = 0;
let submittedLines: string[] = [];
function installBackend(): void {
  g.webui = {
    call: async (_name, payload) => {
      const request = JSON.parse(payload) as {
        id: number;
        method: string;
        params?: { line?: string };
      };
      if (request.method === 'term.input') {
        submittedLines.push(request.params?.line ?? '');
        const token = ++promptToken;
        window.setTimeout(() => {
          if (request.params?.line?.trim() === 'clc') {
            window.__nelsonPush?.('term.clear', '{}');
          }
          window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ', token }));
        }, 0);
      }
      const result =
        request.method === 'ping'
          ? { backend: 'test', version: '0' }
          : request.method === 'term.sync'
          ? { prompt: '>> ' }
          : request.method === 'history.list'
          ? { entries: [] }
          : {};
      return JSON.stringify({ id: request.id, result });
    },
  };
}
//=============================================================================
async function flush(): Promise<void> {
  for (let i = 0; i < 6; i += 1) {
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });
  }
}
//=============================================================================
async function renderConsole(): Promise<HTMLDivElement> {
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  mounted.push({ root, host });
  await act(async () => root.render(<ConsolePanel />));
  await flush();
  return host;
}
//=============================================================================
beforeEach(() => {
  promptToken = 0;
  submittedLines = [];
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  vi.stubGlobal(
    'ResizeObserver',
    class {
      observe(): void {}
      disconnect(): void {}
    }
  );
  vi.spyOn(window, 'requestAnimationFrame').mockImplementation((cb) =>
    window.setTimeout(() => cb(performance.now()), 0)
  );
  vi.spyOn(window, 'cancelAnimationFrame').mockImplementation((id) => window.clearTimeout(id));
  installBackend();
});
//=============================================================================
afterEach(() => {
  for (const { root, host } of mounted.splice(0)) {
    act(() => root.unmount());
    host.remove();
  }
  window.getSelection()?.removeAllRanges();
  delete g.webui;
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
});
//=============================================================================
describe('ConsolePanel selection', () => {
  it('selects the command window transcript with Ctrl+A in the prompt', async () => {
    const host = await renderConsole();
    await act(async () => {
      window.__nelsonPush?.('term.output', JSON.stringify({ text: 'answer = 42\n' }));
    });
    await flush();

    const input = host.querySelector<HTMLTextAreaElement>('.console-input');
    expect(input).not.toBeNull();
    input!.focus();
    act(() => {
      input!.dispatchEvent(
        new KeyboardEvent('keydown', {
          key: 'a',
          ctrlKey: true,
          bubbles: true,
          cancelable: true,
        })
      );
    });

    expect(window.getSelection()?.toString()).toContain('answer = 42');
  });
});
//=============================================================================
describe('ConsolePanel prompt lifecycle', () => {
  it('returns focus to the prompt after a submitted command completes', async () => {
    const host = await renderConsole();
    const input = host.querySelector<HTMLTextAreaElement>('.console-input');
    expect(input).not.toBeNull();

    input!.focus();
    await act(async () => {
      sendToConsoleInput('disp(1)');
    });
    await act(async () => {
      input!.dispatchEvent(
        new KeyboardEvent('keydown', {
          key: 'Enter',
          bubbles: true,
          cancelable: true,
        })
      );
    });
    await flush();

    const nextInput = host.querySelector<HTMLTextAreaElement>('.console-input');
    expect(submittedLines).toContain('disp(1)');
    expect(document.activeElement).toBe(nextInput);
  });

  it('resets the transcript scroll when clc clears the command window', async () => {
    const host = await renderConsole();
    const transcript = host.querySelector<HTMLDivElement>('.console-host');
    const input = host.querySelector<HTMLTextAreaElement>('.console-input');
    expect(transcript).not.toBeNull();
    expect(input).not.toBeNull();

    await act(async () => {
      window.__nelsonPush?.('term.output', JSON.stringify({ text: `${'\n'.repeat(40)}bottom\n` }));
    });
    await flush();
    transcript!.scrollTop = 240;

    await act(async () => {
      window.__nelsonPush?.('term.clear', '{}');
      window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ', token: ++promptToken }));
    });
    await flush();

    expect(transcript!.scrollTop).toBe(0);
    expect(host.textContent).not.toContain('bottom');
  });
});
//=============================================================================
