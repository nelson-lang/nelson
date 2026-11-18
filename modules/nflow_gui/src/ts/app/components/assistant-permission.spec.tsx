//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Unit tests for the NFlow assistant permission UI. The native bridge is mocked,
// so these run with NO AI model and NO engine (what CI has): they pin the bridge
// contract — reading/writing the session-wide permission mode and answering a
// tool-permission prompt through nelson.ai.acp.permissionMode /
// nelson.ai.internal.assistantPermissionReply.
import { afterEach, beforeEach, expect, test, vi } from 'vitest';
import { createRoot } from 'react-dom/client';
import { act } from 'react-dom/test-utils';
//=============================================================================
type PushHandler = (payload: unknown) => void;
let pushHandlers: PushHandler[] = [];
const nflowRpc = vi.fn(async (): Promise<unknown> => ({}));
const onNFlowPush = vi.fn((_channel: string, cb: PushHandler) => {
  pushHandlers.push(cb);
  return () => {
    pushHandlers = pushHandlers.filter((h) => h !== cb);
  };
});
//=============================================================================
vi.mock('../native-rpc.js', () => ({
  nflowRpc: (...args: unknown[]) => nflowRpc(...(args as [])),
  onNFlowPush: (channel: string, cb: PushHandler) => onNFlowPush(channel, cb),
}));
//=============================================================================
// Imported after the mock is registered.
import {
  AssistantPermissionModeSelect,
  AssistantPermissionDialog,
} from './assistant-permission.js';
//=============================================================================
const flush = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));
//=============================================================================
beforeEach(() => {
  pushHandlers = [];
  nflowRpc.mockClear();
  onNFlowPush.mockClear();
  nflowRpc.mockResolvedValue({});
});
afterEach(() => {
  vi.restoreAllMocks();
});
//=============================================================================
test('mode selector reads the engine mode and writes changes back', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);

  await act(async () => {
    root.render(<AssistantPermissionModeSelect />);
  });
  await act(async () => {
    await flush();
  });

  // On mount it asked the engine for the current session-wide mode.
  expect(nflowRpc).toHaveBeenCalledWith(
    'nflow.callback.invoke',
    expect.objectContaining({
      code: expect.stringContaining('nelson.ai.acp.permissionMode()'),
    })
  );

  // The engine answers over the push channel; the select reflects it.
  await act(async () => {
    pushHandlers.forEach((h) => h({ channel: 'permissionMode', payload: '"auto"' }));
  });
  const select = container.querySelector('select') as HTMLSelectElement;
  expect(select.value).toBe('auto');

  // Changing it writes the new mode back to the engine.
  nflowRpc.mockClear();
  await act(async () => {
    select.value = 'full';
    select.dispatchEvent(new Event('change', { bubbles: true }));
  });
  expect(nflowRpc).toHaveBeenCalledWith(
    'nflow.callback.invoke',
    expect.objectContaining({
      code: expect.stringContaining("nelson.ai.acp.permissionMode('full')"),
    })
  );

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
test('permission prompt appears on a request and replies with the decision', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);

  await act(async () => {
    root.render(<AssistantPermissionDialog />);
  });
  await act(async () => {
    await flush();
  });

  // Nothing is shown until the engine pushes a request.
  expect(container.querySelector('.assistant-permission-card')).toBeNull();

  await act(async () => {
    pushHandlers.forEach((h) =>
      h({
        channel: 'permission',
        payload: JSON.stringify({
          id: 7,
          tool: 'read',
          path: 'C:/x.txt',
          title: 'Claude wants to read C:/x.txt',
        }),
      })
    );
  });
  const card = container.querySelector('.assistant-permission-card');
  expect(card).toBeTruthy();
  expect(card?.textContent).toContain('C:/x.txt');

  // Deny sends the request id and 'reject', then closes the prompt.
  const denyBtn = Array.from(
    container.querySelectorAll('.assistant-permission-card__buttons button')
  ).find((b) => b.textContent === 'Deny') as HTMLElement;
  expect(denyBtn).toBeTruthy();
  await act(async () => {
    denyBtn.dispatchEvent(new MouseEvent('click', { bubbles: true }));
  });
  expect(nflowRpc).toHaveBeenCalledWith(
    'nflow.callback.invoke',
    expect.objectContaining({
      code: expect.stringContaining(
        "nelson.ai.internal.assistantPermissionReply('nflow', 7, 'reject')"
      ),
    })
  );
  expect(container.querySelector('.assistant-permission-card')).toBeNull();

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
test('permission prompt remembers this file with allow_always_path', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);

  await act(async () => {
    root.render(<AssistantPermissionDialog />);
  });
  await act(async () => {
    await flush();
  });

  await act(async () => {
    pushHandlers.forEach((h) =>
      h({
        channel: 'permission',
        payload: JSON.stringify({ id: 5, tool: 'write', path: 'C:/y.txt', title: 'x' }),
      })
    );
  });

  const alwaysBtn = Array.from(
    container.querySelectorAll('.assistant-permission-card__buttons button')
  ).find((b) => b.textContent === 'Always for this file') as HTMLElement;
  expect(alwaysBtn).toBeTruthy();
  await act(async () => {
    alwaysBtn.dispatchEvent(new MouseEvent('click', { bubbles: true }));
  });
  expect(nflowRpc).toHaveBeenCalledWith(
    'nflow.callback.invoke',
    expect.objectContaining({
      code: expect.stringContaining(
        "nelson.ai.internal.assistantPermissionReply('nflow', 5, 'allow_always_path')"
      ),
    })
  );
  expect(container.querySelector('.assistant-permission-card')).toBeNull();

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
test('permission prompt closes when the run ends', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);

  await act(async () => {
    root.render(<AssistantPermissionDialog />);
  });
  await act(async () => {
    await flush();
  });

  await act(async () => {
    pushHandlers.forEach((h) =>
      h({ channel: 'permission', payload: JSON.stringify({ id: 1, title: 'x' }) })
    );
  });
  expect(container.querySelector('.assistant-permission-card')).toBeTruthy();

  // A trailing answer/error/reset must never leave the prompt orphaned.
  await act(async () => {
    pushHandlers.forEach((h) => h({ channel: 'answer', payload: '{}' }));
  });
  expect(container.querySelector('.assistant-permission-card')).toBeNull();

  await act(async () => {
    root.unmount();
  });
});
//=============================================================================
