//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { resolveVersionedAssetUrl } from './runtimeUrls';
//=============================================================================
const PREFS_KEY = 'nelson-nmm-gui-preferences';
const REGISTRY_KEY = 'nelson-nmm-registry-url';
const MAX_DOCUMENT_BYTES = 1024 * 1024;
interface PreparedRequest {
  handled: boolean;
  result?: unknown;
  params: Record<string, unknown>;
}
//=============================================================================
function safeObject(value: unknown): Record<string, unknown> {
  return value && typeof value === 'object' && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : {};
}
//=============================================================================
function localeCandidates(language: string): string[] {
  const normalized = language.replace('-', '_') || 'en_US';
  const languageOnly = normalized.split('_')[0];
  return [...new Set([normalized, languageOnly, 'en_US'])];
}
//=============================================================================
function repositoryBase(value: unknown): string {
  const url = String(value ?? '')
    .trim()
    .replace(/(?:\.git)?\/?$/, '');
  let match = url.match(/^https:\/\/github\.com\/([^/]+)\/([^/]+)$/i);
  if (match) return `https://raw.githubusercontent.com/${match[1]}/${match[2]}/HEAD/`;
  match = url.match(/^https:\/\/gitlab\.com\/(.+)$/i);
  return match ? `https://gitlab.com/${match[1]}/-/raw/HEAD/` : '';
}
//=============================================================================
export class WasmPackageManagerServices {
  constructor(
    private readonly baseUrl: string,
    private readonly openExternal?: (url: string) => void,
    private readonly storage: Storage | null = globalThis.localStorage ?? null
  ) {}
  //=============================================================================
  async prepare(method: string, params: Record<string, unknown>): Promise<PreparedRequest> {
    if (method === 'i18n') {
      return { handled: true, result: await this.translations(), params };
    }
    if (method === 'prefs') {
      return { handled: true, result: this.readStoredObject(PREFS_KEY), params };
    }
    if (method === 'savePrefs') {
      const merged = { ...this.readStoredObject(PREFS_KEY), ...safeObject(params.prefs) };
      this.writeStoredObject(PREFS_KEY, merged);
      return { handled: true, result: merged, params };
    }
    if (method === 'pendingDeepLink') {
      return { handled: true, result: {}, params };
    }
    if (method === 'openExternal') {
      const url = String(params.url ?? '');
      if (!/^https?:\/\//i.test(url)) throw new Error('Invalid external URL');
      this.openExternal?.(url);
      return { handled: true, result: { opened: true }, params };
    }
    if (method === 'doc' && !String(params.path ?? '')) {
      return { handled: true, result: await this.repositoryDocument(params), params };
    }
    return {
      handled: false,
      params: { ...params, browserRegistryUrl: this.readStorage(REGISTRY_KEY) },
    };
  }
  //=============================================================================
  complete(method: string, params: Record<string, unknown>): void {
    if (method !== 'setRegistry') return;
    const url = String(params.url ?? '').trim();
    if (url) this.writeStorage(REGISTRY_KEY, url);
    else this.removeStorage(REGISTRY_KEY);
  }
  //=============================================================================
  private async translations(): Promise<Record<string, unknown>> {
    const englishUrl = resolveVersionedAssetUrl('./locale/nmm-ui-en_US.json', this.baseUrl);
    const englishResponse = await fetch(englishUrl);
    const english = englishResponse.ok ? await englishResponse.text() : '{}';
    for (const locale of localeCandidates(globalThis.navigator?.language ?? 'en_US')) {
      const url = resolveVersionedAssetUrl(`./locale/nmm-ui-${locale}.json`, this.baseUrl);
      const response = await fetch(url);
      if (!response.ok) continue;
      return { ok: true, locale, en_US: english, strings: await response.text() };
    }
    return { ok: true, locale: 'en_US', en_US: english, strings: english };
  }
  //=============================================================================
  private async repositoryDocument(params: Record<string, unknown>): Promise<unknown> {
    const base = repositoryBase(params.repository);
    if (!base) return { ok: true, text: '' };
    const candidates =
      String(params.kind).toLowerCase() === 'changelog'
        ? ['CHANGELOG.md', 'changelog.md']
        : ['README.md', 'readme.md'];
    for (const name of candidates) {
      try {
        const response = await fetch(new URL(name, base));
        if (!response.ok) continue;
        const text = await response.text();
        if (new TextEncoder().encode(text).byteLength > MAX_DOCUMENT_BYTES) {
          throw new Error('Package document exceeds the browser size limit');
        }
        return { ok: true, text };
      } catch {
        // Package documentation is optional; try its alternate conventional name.
      }
    }
    return { ok: true, text: '' };
  }
  //=============================================================================
  private readStoredObject(key: string): Record<string, unknown> {
    try {
      return safeObject(JSON.parse(this.readStorage(key) || '{}'));
    } catch {
      return {};
    }
  }
  //=============================================================================
  private writeStoredObject(key: string, value: Record<string, unknown>): void {
    this.writeStorage(key, JSON.stringify(value));
  }
  //=============================================================================
  private readStorage(key: string): string {
    try {
      return this.storage?.getItem(key) ?? '';
    } catch {
      return '';
    }
  }
  //=============================================================================
  private writeStorage(key: string, value: string): void {
    try {
      this.storage?.setItem(key, value);
    } catch {
      // The package manager remains usable when persistent browser storage is denied.
    }
  }
  //=============================================================================
  private removeStorage(key: string): void {
    try {
      this.storage?.removeItem(key);
    } catch {
      // The package manager remains usable when persistent browser storage is denied.
    }
  }
}
//=============================================================================
