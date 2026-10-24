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
import { WasmPackageManagerServices } from '../../src/wasm/wasmPackageManagerServices';
//=============================================================================
describe('WebAssembly Package Manager browser services', () => {
  beforeEach(() => {
    localStorage.clear();
    vi.unstubAllGlobals();
  });

  it('loads versioned locale catalogs for the shared embedded panel', async () => {
    vi.stubGlobal('navigator', { language: 'fr-FR' });
    const fetchMock = vi.fn(async (input: string | URL | Request) => {
      const url = String(input);
      if (url.includes('nmm-ui-en_US.json')) return new Response('{"title":"Packages"}');
      if (url.includes('nmm-ui-fr.json')) return new Response('{"title":"Paquets"}');
      return new Response('', { status: 404 });
    });
    vi.stubGlobal('fetch', fetchMock);
    const services = new WasmPackageManagerServices(
      'https://example.test/nmm/index.html?build=abc123'
    );

    await expect(services.prepare('i18n', {})).resolves.toMatchObject({
      handled: true,
      result: { ok: true, locale: 'fr' },
    });
    expect(fetchMock).toHaveBeenCalledWith(
      'https://example.test/nmm/locale/nmm-ui-en_US.json?build=abc123'
    );
    expect(fetchMock).toHaveBeenCalledWith(
      'https://example.test/nmm/locale/nmm-ui-fr.json?build=abc123'
    );
  });

  it('persists panel preferences and injects the browser registry', async () => {
    const services = new WasmPackageManagerServices('https://example.test/nmm/index.html');

    await services.prepare('savePrefs', { prefs: { filter: 'updates' } });
    await expect(services.prepare('prefs', {})).resolves.toMatchObject({
      result: { filter: 'updates' },
    });
    services.complete('setRegistry', { url: 'https://packages.example.test/registry.json' });
    await expect(services.prepare('snapshot', { fresh: true })).resolves.toMatchObject({
      handled: false,
      params: {
        fresh: true,
        browserRegistryUrl: 'https://packages.example.test/registry.json',
      },
    });
  });

  it('restricts external links to HTTP and HTTPS', async () => {
    const openExternal = vi.fn();
    const services = new WasmPackageManagerServices(
      'https://example.test/nmm/index.html',
      openExternal
    );

    await expect(
      services.prepare('openExternal', { url: 'https://example.test/docs' })
    ).resolves.toMatchObject({ result: { opened: true } });
    expect(openExternal).toHaveBeenCalledWith('https://example.test/docs');
    await expect(services.prepare('openExternal', { url: 'file:///tmp/data' })).rejects.toThrow(
      'Invalid external URL'
    );
  });
});
//=============================================================================
