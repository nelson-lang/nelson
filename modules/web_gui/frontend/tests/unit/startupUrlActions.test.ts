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
  applyStartupActions,
  applyStartupUrlActions,
  parseStartupUrlActions,
} from '../../src/desktop/startupUrlActions';
//=============================================================================
function base64Url(value: string): string {
  return btoa(value).replaceAll('+', '-').replaceAll('/', '_').replace(/=+$/, '');
}
//=============================================================================
describe('WebAssembly startup URL actions', () => {
  it('parses a startup command', () => {
    expect(parseStartupUrlActions('http://localhost/?run=surf(peaks)')).toEqual({
      openPath: null,
      script: null,
      command: 'surf(peaks)',
    });
  });

  it('sanitizes generated script file names', () => {
    expect(
      parseStartupUrlActions('http://localhost/?script=disp(1)&file=../demo file')
    ).toMatchObject({
      openPath: null,
      script: { file: 'demo_file.m', content: 'disp(1)' },
      command: '',
    });
  });

  it('decodes compact script payloads', () => {
    const script = 'x = 1;\ndisp(x)';
    expect(
      parseStartupUrlActions(`http://localhost/?script64=${base64Url(script)}`).script
    ).toEqual({
      file: 'startup-url.m',
      content: script,
    });
  });

  it('ignores unsafe open paths', () => {
    expect(parseStartupUrlActions('http://localhost/?open=https://example.test/demo.m')).toEqual({
      openPath: null,
      script: null,
      command: '',
    });
    expect(parseStartupUrlActions('http://localhost/?open=../demo.m')).toEqual({
      openPath: null,
      script: null,
      command: '',
    });
  });

  it('applies URL actions through shared desktop services', async () => {
    const rpc = vi.fn(async () => ({ ok: true }));
    const openEditor = vi.fn();
    const runCommand = vi.fn(async () => undefined);

    await applyStartupUrlActions('http://localhost/?script=disp(1)&file=demo.m&run=surf(peaks)', {
      openEditor,
      rpc,
      runCommand,
    });

    expect(rpc).toHaveBeenCalledWith('fs.create', {
      path: '/workspace/demo.m',
      kind: 'file',
      content: 'disp(1)',
    });
    expect(openEditor).toHaveBeenCalledWith('/workspace/demo.m');
    expect(runCommand).toHaveBeenCalledWith('surf(peaks)');
  });

  it('sanitizes direct startup actions before applying them', async () => {
    const rpc = vi.fn(async () => ({ ok: true }));
    const openEditor = vi.fn();
    const runCommand = vi.fn(async () => undefined);

    await applyStartupActions(
      {
        openPath: '../bad.m',
        script: { file: '../demo file', content: 'x = 1' },
        command: '',
      },
      { openEditor, rpc, runCommand }
    );

    expect(openEditor).toHaveBeenCalledTimes(1);
    expect(openEditor).toHaveBeenCalledWith('/workspace/demo_file.m');
    expect(rpc).toHaveBeenCalledWith('fs.create', {
      path: '/workspace/demo_file.m',
      kind: 'file',
      content: 'x = 1',
    });
  });
});
//=============================================================================
