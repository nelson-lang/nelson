//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { beforeEach, describe, expect, it, vi } from 'vitest';
//=============================================================================
const { rpcMock } = vi.hoisted(() => ({ rpcMock: vi.fn() }));
vi.mock('../../src/rpc/client', () => ({ rpc: rpcMock }));
//=============================================================================
const fetchMock = vi.fn();
//=============================================================================
// A fresh module per test: helpIndex caches the language and search index at
// module scope, and these tests assert exactly how that cache is invalidated.
async function freshHelpIndex() {
  vi.resetModules();
  return import('../../src/desktop/helpIndex');
}
//=============================================================================
function indexResponse() {
  return { ok: true, json: async () => [{ title: 'sin', url: './sin.html' }] };
}
//=============================================================================
describe('refreshHelpLang', () => {
  beforeEach(() => {
    rpcMock.mockReset();
    fetchMock.mockReset();
    fetchMock.mockResolvedValue(indexResponse());
    globalThis.fetch = fetchMock as unknown as typeof fetch;
  });

  it('drops the cached search index when the language changed', async () => {
    rpcMock.mockResolvedValue({ lang: 'en_US' });
    const help = await freshHelpIndex();
    // Prime the English language and index.
    expect(await help.resolveHelpTarget('sin')).toEqual({ open: './sin.html' });
    expect(help.resolveHelpTargetSync('sin')).toEqual({ open: './sin.html' });

    // The engine language switched to French since the first probe.
    rpcMock.mockResolvedValue({ lang: 'fr_FR' });
    expect(await help.refreshHelpLang()).toBe('fr_FR');
    expect(help.getHelpLangSync()).toBe('fr_FR');
    // The English index was dropped, so the synchronous resolve must re-prime.
    expect(help.resolveHelpTargetSync('sin')).toBeNull();
  });

  it('keeps the cached index when the language is unchanged', async () => {
    rpcMock.mockResolvedValue({ lang: 'en_US' });
    const help = await freshHelpIndex();
    await help.resolveHelpTarget('sin');

    expect(await help.refreshHelpLang()).toBe('en_US');
    expect(help.resolveHelpTargetSync('sin')).toEqual({ open: './sin.html' });
  });

  it('keeps the known language when the probe fails', async () => {
    rpcMock.mockResolvedValue({ lang: 'en_US' });
    const help = await freshHelpIndex();
    await help.getHelpLang();

    rpcMock.mockRejectedValue(new Error('offline'));
    expect(await help.refreshHelpLang()).toBe('en_US');
  });
});
//=============================================================================
