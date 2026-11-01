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
import {
  hasFormat,
  saveNotebookDocument,
  withFormat,
  type NotebookSaveDeps,
} from '../../src/notebook/saveDocument';
import { bytesToBase64 } from '../../src/notebook/nlxFormat';
//=============================================================================
function deps(overrides: Partial<NotebookSaveDeps> = {}) {
  const calls: { method: string; params?: Record<string, unknown> }[] = [];
  const base: NotebookSaveDeps = {
    rpc: vi.fn(async (method: string, params?: Record<string, unknown>) => {
      calls.push({ method, params });
      return { ok: true, path: params?.path };
    }),
    pickPath: vi.fn(async () => 'C:/work/picked'),
    download: vi.fn(),
    browserOnly: () => false,
    ...overrides,
  };
  return { base, calls };
}
const bytes = new Uint8Array([80, 75, 3, 4]);
//=============================================================================
describe('saveNotebookDocument', () => {
  it('writes in place when the document has a path of that format', async () => {
    const { base, calls } = deps();
    const result = await saveNotebookDocument(
      { payload: bytes, format: 'nlx', path: 'C:/work/demo.nlx', saveAs: false, baseName: 'demo' },
      base
    );
    expect(result).toEqual({ kind: 'saved', path: 'C:/work/demo.nlx' });
    expect(base.pickPath).not.toHaveBeenCalled();
    expect(calls[0].method).toBe('fs.upload');
    expect(calls[0].params).toEqual({ path: 'C:/work/demo.nlx', data: bytesToBase64(bytes) });
  });

  it('asks for a path when there is none, on Save As, and when the format differs', async () => {
    const { base } = deps();
    expect(
      await saveNotebookDocument(
        { payload: bytes, format: 'nlx', saveAs: false, baseName: 'notebook' },
        base
      )
    ).toEqual({ kind: 'saved', path: 'C:/work/picked.nlx' });
    expect(base.pickPath).toHaveBeenLastCalledWith(
      expect.objectContaining({ suggestedName: 'notebook.nlx', filters: ['.nlx'] })
    );
    await saveNotebookDocument(
      { payload: bytes, format: 'nlx', path: 'C:/work/demo.nlx', saveAs: true, baseName: 'demo' },
      base
    );
    expect(base.pickPath).toHaveBeenLastCalledWith(
      expect.objectContaining({ suggestedName: 'demo.nlx', defaultPath: 'C:/work' })
    );
    await saveNotebookDocument(
      { payload: 'a = 1;', format: 'm', path: 'C:/work/demo.nlx', saveAs: false, baseName: 'demo' },
      base
    );
    expect(base.pickPath).toHaveBeenLastCalledWith(
      expect.objectContaining({ suggestedName: 'demo.m', filters: ['.m'] })
    );
    expect(base.pickPath).toHaveBeenCalledTimes(3);
  });

  it('does nothing when the picker is canceled', async () => {
    const { base, calls } = deps({ pickPath: vi.fn(async () => null) });
    const result = await saveNotebookDocument(
      { payload: bytes, format: 'nlx', saveAs: true, baseName: 'notebook' },
      base
    );
    expect(result).toEqual({ kind: 'canceled' });
    expect(calls).toHaveLength(0);
  });

  it('downloads in the browser build, which has no host file system', async () => {
    const { base, calls } = deps({ browserOnly: () => true });
    const result = await saveNotebookDocument(
      { payload: 'a = 1;', format: 'm', saveAs: false, baseName: 'notebook' },
      base
    );
    expect(result).toEqual({ kind: 'downloaded', filename: 'notebook.m' });
    expect(base.download).toHaveBeenCalledWith('notebook.m', expect.anything(), 'text/plain');
    expect(base.pickPath).not.toHaveBeenCalled();
    expect(calls).toHaveLength(0);
  });

  it('reports a write failure', async () => {
    const { base } = deps({
      rpc: vi.fn(async () => ({ error: { message: 'cannot write file' } })),
    });
    const result = await saveNotebookDocument(
      { payload: bytes, format: 'nlx', path: 'C:/ro/demo.nlx', saveAs: false, baseName: 'demo' },
      base
    );
    expect(result).toEqual({ kind: 'error', message: 'cannot write file' });
  });

  it('keeps or adds the extension', () => {
    expect(hasFormat('a/b.NLX', 'nlx')).toBe(true);
    expect(hasFormat('a/b.m', 'nlx')).toBe(false);
    expect(hasFormat(undefined, 'm')).toBe(false);
    expect(withFormat('a/b', 'm')).toBe('a/b.m');
    expect(withFormat('a/b.m', 'm')).toBe('a/b.m');
  });
});
//=============================================================================
