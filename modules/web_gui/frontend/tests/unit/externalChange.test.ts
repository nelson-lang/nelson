//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import {
  contentStamp,
  externalChangeAction,
  rpcFailureReason,
} from '../../src/editor/externalChange';
import { MockEngine } from '../../src/rpc/mockEngine';
//=============================================================================
const stamp = (version: string, exists = true) => ({ exists, version, size: version.length });
//=============================================================================
describe('externalChangeAction', () => {
  it('does nothing while the file on disk is the one the buffer came from', () => {
    expect(externalChangeAction(stamp('a'), stamp('a'), false)).toBe('none');
    expect(externalChangeAction(stamp('a'), stamp('a'), true)).toBe('none');
  });

  it('reloads a clean buffer and asks before replacing unsaved edits', () => {
    expect(externalChangeAction(stamp('a'), stamp('b'), false)).toBe('reload');
    expect(externalChangeAction(stamp('a'), stamp('b'), true)).toBe('ask');
  });

  it('keeps the text of a deleted file without any question', () => {
    expect(externalChangeAction(stamp('a'), stamp('', false), false)).toBe('forget');
    expect(externalChangeAction(stamp('a'), stamp('', false), true)).toBe('forget');
    expect(externalChangeAction(stamp('', false), stamp('', false), true)).toBe('none');
  });

  it('offers the file again once it is created back', () => {
    expect(externalChangeAction(stamp('', false), stamp('a'), false)).toBe('reload');
    expect(externalChangeAction(stamp('', false), stamp('a'), true)).toBe('ask');
  });

  it('ignores a buffer opened without a stamp or a failed check', () => {
    expect(externalChangeAction(null, stamp('a'), false)).toBe('none');
    expect(externalChangeAction(stamp('a'), null, false)).toBe('none');
  });
});
//=============================================================================
describe('rpcFailureReason', () => {
  it('drops the transport prefix of an engine error', () => {
    expect(
      rpcFailureReason(new Error('nelson-webview RPC "editor.save": The file is read-only.'))
    ).toBe('The file is read-only.');
  });

  it('keeps a plain message', () => {
    expect(rpcFailureReason(new Error('disk full'))).toBe('disk full');
    expect(rpcFailureReason('oops')).toBe('oops');
  });
});
//=============================================================================
describe('contentStamp', () => {
  it('changes with the content and marks a missing file', () => {
    expect(contentStamp('abc')).toEqual(contentStamp(new TextEncoder().encode('abc')));
    expect(contentStamp('abc').version).not.toBe(contentStamp('abd').version);
    expect(contentStamp('')).toMatchObject({ exists: true, size: 0 });
    expect(contentStamp(null)).toEqual({ exists: false, version: '', size: 0 });
  });
});
//=============================================================================
describe('mock engine editor.stat', () => {
  it('follows the saved content', async () => {
    const engine = new MockEngine(() => undefined);
    await engine.handle('editor.save', { path: 'stat_probe.m', content: 'x = 1;' });
    const opened = (await engine.handle('editor.open', { path: 'stat_probe.m' })) as {
      stamp: unknown;
    };
    const first = await engine.handle('editor.stat', { path: 'stat_probe.m' });
    expect(first).toEqual(opened.stamp);
    await engine.handle('editor.save', { path: 'stat_probe.m', content: 'x = 2;' });
    expect(await engine.handle('editor.stat', { path: 'stat_probe.m' })).not.toEqual(first);
    expect(await engine.handle('editor.stat', { path: 'missing_probe.m' })).toMatchObject({
      exists: false,
    });
  });
});
//=============================================================================
