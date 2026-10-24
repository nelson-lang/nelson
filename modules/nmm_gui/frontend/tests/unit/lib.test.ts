//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import { describe, expect, it } from 'vitest';
import { searchScore, highlightParts } from '../../src/lib/search';
import { changesSince, cmp, isMajorUpdate, licenseStatus, planDependencies, shortChecksum, artifactOf } from '../../src/lib/info';
import { escapeHtml, renderMarkdown } from '../../src/lib/markdown';
//=============================================================================
const morse = { name: 'morse', summary: 'International Morse code', description: 'Farnsworth spacing' };
const kw = ['telegraph', 'audio'];
describe('search', () => {
  it('scores the name first, then texts, letters in order and one typo', () => {
    expect(searchScore(morse, 'morse', kw)).toBe(120);
    expect(searchScore(morse, 'orse', kw)).toBe(100);
    expect(searchScore(morse, 'farnsworth', kw)).toBe(40);
    expect(searchScore(morse, 'mrse', kw)).toBe(20);
    expect(searchScore(morse, 'tlegraph', kw)).toBe(10);
    expect(searchScore(morse, 'zzz', kw)).toBe(0);
    expect(searchScore(morse, 'morse zzz', kw)).toBe(0);
    expect(searchScore(morse, '', kw)).toBe(1);
  });
  it('splits a text into highlighted parts', () => {
    expect(highlightParts('International Morse code', 'morse inter')).toEqual([
      { text: 'Inter', mark: true }, { text: 'national ', mark: false }, { text: 'Morse', mark: true }, { text: ' code', mark: false },
    ]);
    expect(highlightParts('abc', '')).toEqual([{ text: 'abc', mark: false }]);
  });
});
//=============================================================================
describe('package facts', () => {
  it('compares versions and detects a major update', () => {
    expect(cmp('1.10.0', '1.9.0')).toBe(1);
    expect(cmp('1.0', '1.0.0')).toBe(0);
    expect(isMajorUpdate('0.8.0', '1.0.0')).toBe(true);
    expect(isMajorUpdate('1.2.0', '1.3.0')).toBe(false);
  });
  it('rates license compatibility with LGPL-3.0 / GPL-3.0', () => {
    expect(licenseStatus('MIT')).toBe('ok');
    expect(licenseStatus('LGPL-3.0-or-later')).toBe('ok');
    expect(licenseStatus('GPL-2.0-or-later')).toBe('ok');
    expect(licenseStatus('GPL-2.0-only')).toBe('warn');
    expect(licenseStatus('Proprietary')).toBe('warn');
    expect(licenseStatus('MIT OR Proprietary')).toBe('ok');
    expect(licenseStatus('GPL-3.0-only WITH Classpath-exception-2.0')).toBe('ok');
    expect(licenseStatus('')).toBe('unknown');
  });
  it('keeps the changelog entries of the new versions only', () => {
    const text = '# Changelog\n## 1.0.0\n- a\n## 0.9.0\n- b\n## 0.8.0\n- c';
    expect(changesSince(text, '0.8.0', '1.0.0')).toBe('## 1.0.0\n- a\n## 0.9.0\n- b');
    expect(changesSince(text, '1.0.0', '1.0.0')).toBe('');
  });
  it('flattens the dependencies an install plan adds', () => {
    const plan = { install_plan: { package: 'a', dependencies: { package: 'b', version: '2.0', action: 'install',
      dependencies: [{ package: 'c', version: '1.0', action: 'install', dependencies: {} }, { package: 'd', version: '1', action: 'installed' }] } } };
    expect(planDependencies(plan)).toEqual([{ name: 'b', version: '2.0' }, { name: 'c', version: '1.0' }]);
  });
  it('picks the artifact of this machine and shortens its checksum', () => {
    const p = { artifacts: { linux64: { source: 'l.nmz', checksum: 'x' }, win64: { source: 'w.nmz', checksum: 'y' } } };
    expect(artifactOf(p, 'win64')).toEqual({ source: 'w.nmz', checksum: 'y', arch: 'win64' });
    expect(shortChecksum('5548d7ec0caf77d77d3007a71c632f9cda28d6d8f94ccb82e5719c1d27324ef1')).toBe('5548d7ec0caf77d7...27324ef1');
  });
});
//=============================================================================
describe('markdown', () => {
  it('escapes HTML before rebuilding the supported subset', () => {
    expect(renderMarkdown('# T\n- a **b**\n<script>x</script>')).toBe('<h1>T</h1><ul><li>a <strong>b</strong></li></ul><p>&lt;script&gt;x&lt;/script&gt;</p>');
    expect(renderMarkdown('[x](javascript:alert(1))')).not.toContain('href');
    expect(escapeHtml('"<&>\'')).toBe('&quot;&lt;&amp;&gt;&#39;');
  });
});
//=============================================================================
