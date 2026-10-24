//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The window strings (locale/nmm-ui-<locale>.json, Crowdin) and the sources
// stay in sync: every key is used, every t('key') exists, and every catalog
// carries exactly the keys of the English reference.
import { describe, expect, it } from 'vitest';
import { readdirSync, readFileSync, statSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { parseManifest } from '../../src/components/ToolDialogs';
//=============================================================================
const here = dirname(fileURLToPath(import.meta.url));
const localeDir = join(here, '..', '..', '..', 'locale');
const srcDir = join(here, '..', '..', 'src');
function sources(dir: string): string {
  return readdirSync(dir).map(name => {
    const path = join(dir, name);
    return statSync(path).isDirectory() ? sources(path) : /\.(ts|tsx)$/.test(name) ? readFileSync(path, 'utf8') : '';
  }).join('\n');
}
//=============================================================================
const catalog = (locale: string): Record<string, string> => JSON.parse(readFileSync(join(localeDir, `nmm-ui-${locale}.json`), 'utf8'));
describe('catalogs', () => {
  const english = catalog('en_US');
  const code = sources(srcDir);
  it('use every key and define every key the page asks for', () => {
    const unused = Object.keys(english).filter(k => !k.startsWith('steps.') && !code.includes(`'${k}'`) && !code.includes(`"${k}"`));
    expect(unused).toEqual([]);
    const asked = [...code.matchAll(/\bt\('([A-Za-z]+)'/g)].map(m => m[1]);
    expect(asked.filter(k => !(k in english))).toEqual([]);
  });
  it('translate exactly the English keys', () => {
    for (const file of readdirSync(localeDir).filter(f => /^nmm-ui-.*\.json$/.test(f))) {
      const keys = Object.keys(JSON.parse(readFileSync(join(localeDir, file), 'utf8'))).sort();
      expect(keys, file).toEqual(Object.keys(english).sort());
    }
  });
});
//=============================================================================
describe('environment manifest', () => {
  it('reads one "name version" per line, skipping comments', () => {
    expect(parseManifest('# saved\nmorse 1.0.0\n\nedf@0.3.0\n% note\nngen')).toEqual([
      { name: 'morse', version: '1.0.0' }, { name: 'edf', version: '0.3.0' }, { name: 'ngen', version: '' },
    ]);
  });
});
//=============================================================================
