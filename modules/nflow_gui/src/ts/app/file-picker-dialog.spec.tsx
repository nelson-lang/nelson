//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Exercises the shared, Qt-free FilePickerDialog (hosted in the webview module)
// through the same `@nelson/dialogs` import the editor uses.
import { expect, test } from 'vitest';
import { createRoot } from 'react-dom/client';
import { act } from 'react-dom/test-utils';
import { FilePickerDialog, type FilePickerListResult } from '@nelson/dialogs';
//=============================================================================
const flush = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));
test('renders directory entries and submits the picked file on double-click', async () => {
  const container = document.createElement('div');
  document.body.appendChild(container);
  const root = createRoot(container);

  const listDir = async (): Promise<FilePickerListResult> => ({
    cwd: '/d',
    entries: [
      { name: 'sub', path: '/d/sub', isDir: true, size: 0 },
      { name: 'file.nflow', path: '/d/file.nflow', isDir: false, size: 12 },
    ],
  });
  let picked: string | null = null;

  await act(async () => {
    root.render(
      <FilePickerDialog
        mode="open"
        listDir={listDir}
        onSubmit={(p) => {
          picked = p;
        }}
        onCancel={() => undefined}
      />
    );
  });
  await act(async () => {
    await flush();
  });

  const options = Array.from(container.querySelectorAll('[role="option"]'));
  expect(options.some((el) => el.textContent?.includes('sub'))).toBe(true);
  const fileItem = options.find((el) => el.textContent?.includes('file.nflow'));
  expect(fileItem).toBeTruthy();

  await act(async () => {
    fileItem!.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
  });
  expect(picked).toBe('/d/file.nflow');

  await act(async () => {
    root.unmount();
  });
  container.remove();
});
//=============================================================================
