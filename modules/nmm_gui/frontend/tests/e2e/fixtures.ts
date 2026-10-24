//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import type { Page } from '@playwright/test';
//=============================================================================
const here = dirname(fileURLToPath(import.meta.url));
const locale = (name: string) => readFileSync(join(here, '..', '..', '..', 'locale', `nmm-ui-${name}.json`), 'utf8');
// One installed package with an update (morse 0.8.0 -> 1.0.0, a major one)
// and one binary registry package (com_engine, proprietary license).
export const registryPackages = [
  {
    name: 'com_engine', version: '1.0.0', summary: 'Windows COM/ActiveX automation for Nelson.',
    description: 'actxserver, actxcontrollist and Excel helpers.', license: 'Proprietary',
    repository: 'https://github.com/NelsonModules/com_engine.git', keywords: ['com', 'activex'],
    categories: ['Interoperability'], package_type: 'binary', platforms: ['win64'], nelson: '>=2.0.0',
    dependencies: {}, size: 4718592,
    artifacts: { win64: { source: 'https://github.com/NelsonModules/com_engine/releases/download/v1.0.0/com_engine-win64-1.0.0.nmz', checksum: '5548d7ec0caf77d77d3007a71c632f9cda28d6d8f94ccb82e5719c1d27324ef1' } },
  },
  {
    name: 'morse', version: '1.0.0', title: 'Morse code', summary: 'International Morse code: encode, decode, audio and a trainer.',
    description: 'Farnsworth spacing and an interactive practice trainer.', license: 'LGPL-3.0-or-later',
    repository: 'https://github.com/NelsonModules/morse', keywords: ['telegraph', 'audio'],
    categories: ['Signal processing', 'Education'], package_type: 'source', platforms: ['all'], nelson: '>=2.0.0',
    dependencies: {}, source: 'https://github.com/NelsonModules/morse/releases/download/v1.0.0/morse-all-1.0.0.nmz',
    checksum: 'f850303b1d4f40550bd9c48fffdc6724a026f440fea3f8f5e50d6c038c47128f',
    changelog: '# Changelog\n\n## 1.0.0\n- First public release\n\n## 0.9.0\n- Farnsworth timing\n\n## 0.8.0\n- Initial import\n',
  },
];
//=============================================================================
export interface MockOptions {
  locale?: 'en_US' | 'fr_FR';
  prefs?: Record<string, unknown>;
  offline?: boolean;
  snapshotDelayMs?: number;
  // registry that is not configured (the search fails with that identifier)
  noRegistry?: boolean;
  // link parked by nmm_gui(action, name) before the page loads
  deepLink?: { action: string; name: string; version?: string };
}
//=============================================================================
// Opens the window on the mocked backend. window.__calls records every
// request as [method, params].
export async function openWindow(page: Page, options: MockOptions = {}): Promise<void> {
  const data = {
    locale: options.locale || 'en_US',
    en: locale('en_US'),
    fr: locale('fr_FR'),
    prefs: options.prefs || {},
    offline: !!options.offline,
    delay: options.snapshotDelayMs || 0,
    packages: registryPackages,
    noRegistry: !!options.noRegistry,
    deepLink: options.deepLink || null,
  };
  await page.addInitScript(d => {
    const installed = [{ name: 'morse', version: '0.8.0', path: 'C:/pkgs/morse/0.8.0/', load: true, active: true, pinned: '',
      versions: ['0.8.0'], bytes: 1234567, installed_at: '2026-09-20 14:05' }];
    const registry = { ok: true, registry: 'https://raw.githubusercontent.com/nelson-lang/nelson-modules-registry/master/registry.json',
      signature_ok: true, signature_key: '4113b7e0c4091683ca17524057769d3f', offline: d.offline, cached_file: 'C:/cache/registry.json' };
    const previews: Record<string, unknown> = {
      morse: { ok: true, plan: { install_plan: { package: 'morse', version: '1.0.0', action: 'install',
        dependencies: { package: 'edf', version: '0.3.0', action: 'install', dependencies: {} } } },
      package: { name: 'morse', version: '1.0.0', package_type: 'source', license: 'LGPL-3.0-or-later' } },
      com_engine: { ok: true, plan: { install_plan: { package: 'com_engine', version: '1.0.0', action: 'install', dependencies: {} } },
      package: { name: 'com_engine', version: '1.0.0', package_type: 'binary', license: 'Proprietary', size: 4718592 } },
    };
    const w = window as unknown as Record<string, unknown>;
    const calls: unknown[] = [];
    w.__calls = calls;
    const answers: Record<string, (p: Record<string, unknown>) => unknown> = {
      i18n: () => ({ ok: true, locale: d.locale, en_US: d.en, strings: d.locale === 'fr_FR' ? d.fr : '' }),
      prefs: () => d.prefs,
      snapshot: () => ({ ok: true, arch: 'win64', registry, list: { ok: true, installed },
        search: d.noRegistry ? { ok: false, error: 'Package registry is not configured.', error_id: 'Nelson:nmm:RegistryNotConfigured' }
          : { ok: true, packages: d.packages } }),
      preview: p => previews[String(p.name)] || { ok: false, error: 'unknown package' },
      versions: () => ({ ok: true, versions: ['1.0.0', '0.9.0'] }),
      doc: () => ({ ok: true, text: '# Readme\nFetched from the repository.' }),
      freeze: () => ({ ok: true, manifest: 'morse 0.8.0' }),
      pendingDeepLink: () => { const link = d.deepLink || { action: '', name: '', version: '' }; d.deepLink = null; return link; },
      tree: () => ({ ok: true, tree: { package: 'morse', version: '0.8.0', installed: true,
        dependencies: { package: 'edf', version: '0.3.0', action: 'install', dependencies: {} } } }),
      verifyInstalled: () => ({ ok: false, issues: ['module.json is missing.'] }),
      cache: () => ({ ok: true, dir: 'C:/cache', archives: 1, bytes: 2048, entries: [{ name: 'morse', version: '1.0.0', has_sha256: true, bytes: 2048 }] }),
    };
    w.nmmGuiRpc = async (text: string) => {
      const q = JSON.parse(text);
      calls.push([q.method, q.params]);
      if (q.method === 'snapshot' && d.delay) await new Promise(r => setTimeout(r, d.delay));
      const answer = answers[q.method];
      return JSON.stringify({ id: q.id, result: answer ? answer(q.params || {}) : { ok: true } });
    };
  }, data);
  await page.goto('/index.html');
}
//=============================================================================
export async function calls(page: Page, method: string): Promise<Record<string, unknown>[]> {
  return page.evaluate(m => ((window as unknown as { __calls: [string, Record<string, unknown>][] }).__calls)
    .filter(c => c[0] === m).map(c => c[1]), method);
}
//=============================================================================
// A backend notification (progress, changed, failed, deeplink).
export async function push(page: Page, channel: string, payload: unknown = ''): Promise<void> {
  await page.evaluate(([c, p]) => window.nmmGuiPush?.(c as string, typeof p === 'string' ? p : JSON.stringify(p)), [channel, payload]);
}
//=============================================================================
