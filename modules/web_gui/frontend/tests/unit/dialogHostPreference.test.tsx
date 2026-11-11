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
import { DialogHost } from '../../src/components/DialogHost';
//=============================================================================
type Call = (name: string, payload: string) => Promise<string>;
const g = window as unknown as { webui?: { call: Call } };
const mounted: Array<{ root: Root; host: HTMLDivElement }> = [];
let replies: Array<Record<string, unknown>> = [];
//=============================================================================
function installBackend(): void {
  g.webui = {
    call: async (_name, payload) => {
      const request = JSON.parse(payload) as {
        id: number;
        method: string;
        params?: Record<string, unknown>;
      };
      if (request.method === 'dialog.reply') {
        replies.push(request.params ?? {});
      }
      return JSON.stringify({ id: request.id, result: {} });
    },
  };
}
//=============================================================================
async function flush(): Promise<void> {
  for (let i = 0; i < 4; i += 1) {
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });
  }
}
//=============================================================================
async function showPreferenceDialog(helpString: string): Promise<HTMLDivElement> {
  const host = document.createElement('div');
  document.body.appendChild(host);
  const root = createRoot(host);
  mounted.push({ root, host });
  await act(async () => root.render(<DialogHost />));
  await act(async () => {
    window.__nelsonPush?.(
      'dialog.show',
      JSON.stringify({
        id: 7,
        kind: 'preference',
        title: 'Title',
        message: 'Question',
        icon: 'question',
        fields: [],
        buttons: ['Yes label', 'No label'],
        buttonValues: ['yesValue', 'noValue'],
        extraButtons: ['Cancel'],
        defaultButton: 'Yes label',
        checkboxVisible: true,
        checkboxText: 'Remember',
        checkboxState: false,
        helpString,
      })
    );
  });
  await flush();
  return host;
}
//=============================================================================
function buttonByText(host: HTMLElement, text: string): HTMLButtonElement | undefined {
  return Array.from(host.querySelectorAll('button')).find((b) => b.textContent === text);
}
//=============================================================================
beforeEach(() => {
  replies = [];
  vi.stubGlobal('IS_REACT_ACT_ENVIRONMENT', true);
  installBackend();
});
//=============================================================================
afterEach(() => {
  for (const { root, host } of mounted.splice(0)) {
    act(() => root.unmount());
    host.remove();
  }
  delete g.webui;
  vi.unstubAllGlobals();
});
//=============================================================================
describe('DialogHost preference dialog', () => {
  it('shows no help button without a help string', async () => {
    const host = await showPreferenceDialog('');
    expect(buttonByText(host, 'Yes label')).toBeDefined();
    expect(host.querySelectorAll('.dialog-card__buttons button')).toHaveLength(3);
  });

  it('replies with help and the check box state when help is clicked', async () => {
    const host = await showPreferenceDialog('More info');
    const checkbox = host.querySelector('input[type="checkbox"]') as HTMLInputElement;
    await act(async () => checkbox.click());
    const help = buttonByText(host, 'More info');
    expect(help).toBeDefined();
    await act(async () => help?.click());
    await flush();
    expect(replies).toEqual([{ id: 7, ok: true, help: true, checkboxChecked: true }]);
  });

  it('replies with the value of the clicked preference button', async () => {
    const host = await showPreferenceDialog('More info');
    await act(async () => buttonByText(host, 'No label')?.click());
    await flush();
    expect(replies).toEqual([{ id: 7, ok: true, button: 'noValue', checkboxChecked: false }]);
  });
});
//=============================================================================
