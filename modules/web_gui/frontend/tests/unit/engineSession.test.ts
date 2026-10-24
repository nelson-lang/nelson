//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { afterEach, describe, expect, it, vi } from 'vitest';
import {
  isEngineBusy,
  onEngineReady,
  runCommand,
  submitCommand,
  subscribeEngine,
} from '../../src/desktop/engineSession';
//=============================================================================
type Call = (name: string, payload: string) => Promise<string>;
const g = window as unknown as { webui?: { call: Call } };
const ok: Call = async (_n, p) => JSON.stringify({ id: JSON.parse(p).id, result: null });
const prompt = () => window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ' }));
afterEach(() => {
  prompt();
  delete g.webui;
});
//=============================================================================
describe('engine session', () => {
  it('a submitted command is Busy until the next prompt', async () => {
    g.webui = { call: vi.fn(ok) };
    const states: boolean[] = [];
    const off = subscribeEngine((busy) => states.push(busy));

    await submitCommand('x = 1');
    expect(isEngineBusy()).toBe(true);
    prompt();
    off();

    expect(isEngineBusy()).toBe(false);
    expect(states).toEqual([true, false]);
  });

  it('an empty line does not make the engine Busy', async () => {
    g.webui = { call: vi.fn(ok) };
    await submitCommand('   ');
    expect(isEngineBusy()).toBe(false);
  });

  it('tracks commands started outside the command window', () => {
    window.__nelsonPush?.('term.busy', '{}');
    expect(isEngineBusy()).toBe(true);

    prompt();
    expect(isEngineBusy()).toBe(false);
  });

  it('ignores stale prompts from an older browser evaluation cycle', () => {
    window.__nelsonPush?.('term.busy', JSON.stringify({ token: 1 }));
    window.__nelsonPush?.('term.busy', JSON.stringify({ token: 2 }));
    window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ', token: 1 }));
    expect(isEngineBusy()).toBe(true);

    window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ', token: 2 }));
    expect(isEngineBusy()).toBe(false);
  });

  it('ignores an un-tokened prompt while a tokened evaluation is busy', () => {
    window.__nelsonPush?.('term.busy', JSON.stringify({ token: 3 }));
    window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ' }));
    expect(isEngineBusy()).toBe(true);

    window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ', token: 3 }));
    expect(isEngineBusy()).toBe(false);
  });

  it('defers an un-tokened prompt while a submitted command is still pending', async () => {
    let release!: () => void;
    g.webui = {
      call: vi.fn(
        () =>
          new Promise<string>((resolve) => {
            release = () => resolve(JSON.stringify({ id: 1, result: null }));
          })
      ),
    };

    const submitted = submitCommand('pause(1)');
    expect(isEngineBusy()).toBe(true);
    window.__nelsonPush?.('term.prompt', JSON.stringify({ prompt: '>> ' }));
    expect(isEngineBusy()).toBe(true);

    release();
    await submitted;
    expect(isEngineBusy()).toBe(false);
  });

  it('ready subscribers run after every prompt, once the engine is Ready', async () => {
    g.webui = { call: vi.fn(ok) };
    const seen: Array<{ busy: boolean; prompt: unknown }> = [];
    const off = onEngineReady((p) => seen.push({ busy: isEngineBusy(), prompt: p }));

    await submitCommand('disp(1)');
    prompt();
    off();
    prompt();

    expect(seen).toEqual([{ busy: false, prompt: { prompt: '>> ' } }]);
  });

  it('a transport failure rejects and does not leave the engine Busy', async () => {
    g.webui = { call: vi.fn<Call>().mockRejectedValue(new Error('transport down')) };

    await expect(submitCommand('x = 1')).rejects.toThrow('transport down');
    expect(isEngineBusy()).toBe(false);
  });

  it('runCommand swallows transport failures', async () => {
    g.webui = { call: vi.fn<Call>().mockRejectedValue(new Error('transport down')) };
    await expect(runCommand('clc')).resolves.toBeUndefined();
    expect(isEngineBusy()).toBe(false);
  });
});
//=============================================================================
