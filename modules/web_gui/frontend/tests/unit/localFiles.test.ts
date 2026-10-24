//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it, vi } from 'vitest';
import { uploadLocalFiles } from '../../src/desktop/localFiles';
//=============================================================================
function deps(options: { nflow?: boolean } = {}) {
  const rpc = vi.fn(async (_method: string, params: { path?: string; data?: string }) => ({
    path: params.path,
    size: atob(params.data ?? '').length,
  }));
  return {
    currentDirectory: () => ({ cwd: '/workspace', separator: '/' }),
    openEditor: vi.fn(),
    rpc,
    runCommand: vi.fn(async () => undefined),
    showStatusMessage: vi.fn(),
    nflowAvailable: () => options.nflow === true,
  };
}
//=============================================================================
describe('local browser files', () => {
  it('uploads files into the current Nelson folder', async () => {
    const d = deps();
    const result = await uploadLocalFiles([new File(['hello'], 'notes.txt')], undefined, d);

    expect(d.rpc).toHaveBeenCalledWith('fs.upload', {
      path: '/workspace/notes.txt',
      data: btoa('hello'),
    });
    expect(result).toEqual([
      { name: 'notes.txt', path: '/workspace/notes.txt', size: 5, action: 'workspace' },
    ]);
    expect(d.showStatusMessage).toHaveBeenCalledWith('Uploaded notes.txt to /workspace');
  });

  it('opens uploaded Nelson scripts in the shared editor', async () => {
    const d = deps();
    await uploadLocalFiles([new File(['x = 1'], 'demo.m')], '/workspace', d);

    expect(d.openEditor).toHaveBeenCalledWith('/workspace/demo.m');
  });

  it('opens NFlow after uploading a model when the capability is available', async () => {
    const d = deps({ nflow: true });
    const result = await uploadLocalFiles([new File(['{}'], 'logic.nflow')], '/workspace', d);

    expect(d.runCommand).toHaveBeenCalledWith('nflow()');
    expect(result[0].action).toBe('nflow');
  });

  it('sanitizes dropped filenames before writing to the virtual filesystem', async () => {
    const d = deps();
    await uploadLocalFiles([new File(['x'], '../bad:name.wav')], '/workspace', d);

    expect(d.rpc).toHaveBeenCalledWith(
      'fs.upload',
      expect.objectContaining({ path: '/workspace/bad_name.wav' })
    );
  });
});
//=============================================================================
