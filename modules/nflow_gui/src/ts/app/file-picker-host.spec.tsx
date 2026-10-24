//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, expect, test, vi } from 'vitest';
import { act } from 'react-dom/test-utils';
import { createRoot } from 'react-dom/client';
import { FILE_PICKER_EVENT } from './components/modal-events.js';
import type { FilePickerRequestDetail } from './file-dialogs.js';
//=============================================================================
const { rpc } = vi.hoisted(() => ({
  rpc: vi.fn(async (method: string) => {
    if (method === 'nflow.fs.list') {
      return JSON.stringify({
        cwd: '/remembered',
        entries: [{ name: 'model.nflow', path: '/remembered/model.nflow', isDir: false, size: 12 }],
      });
    }
    if (method === 'nflow.fs.dialogAccept') return JSON.stringify({ ok: true });
    throw new Error(`unexpected RPC: ${method}`);
  }),
}));
//=============================================================================
vi.mock('./native-rpc.js', () => ({ nflowRpc: rpc }));
import { FilePickerHost } from './components/file-picker-host.js';
//=============================================================================
const flush = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));
afterEach(() => {
  rpc.mockClear();
  document.body.replaceChildren();
});
//=============================================================================
async function openPicker(resolve: (path: string | null) => void) {
  const event = new CustomEvent<FilePickerRequestDetail>(FILE_PICKER_EVENT, {
    cancelable: true,
    detail: { mode: 'open', defaultPath: '/remembered', resolve },
  });
  await act(async () => {
    document.dispatchEvent(event);
    await flush();
  });
  return event;
}
//=============================================================================
test('records an accepted HTML picker result before resolving it', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const resolve = vi.fn();
  await act(async () => root.render(<FilePickerHost />));
  const event = await openPicker(resolve);
  expect(event.defaultPrevented).toBe(true);

  const file = container.querySelector('[role="option"]');
  expect(file?.textContent).toContain('model.nflow');
  await act(async () => {
    file!.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
    await flush();
  });

  expect(rpc).toHaveBeenCalledWith('nflow.fs.dialogAccept', {
    mode: 'open',
    path: '/remembered/model.nflow',
  });
  expect(resolve).toHaveBeenCalledWith('/remembered/model.nflow');
  await act(async () => root.unmount());
});
//=============================================================================
test('does not update dialog history when the picker is cancelled', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);
  const resolve = vi.fn();
  await act(async () => root.render(<FilePickerHost />));
  await openPicker(resolve);

  const cancel = Array.from(container.querySelectorAll('button')).find(
    (button) => button.textContent === 'Cancel'
  );
  await act(async () => cancel!.click());

  expect(resolve).toHaveBeenCalledWith(null);
  expect(rpc.mock.calls.some(([method]) => method === 'nflow.fs.dialogAccept')).toBe(false);
  await act(async () => root.unmount());
});
//=============================================================================
