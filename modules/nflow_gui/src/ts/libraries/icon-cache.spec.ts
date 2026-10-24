//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Tests for the shared block-icon cache
//=============================================================================
import { vi, beforeEach, afterEach } from 'vitest';
import {
  getIcon,
  getIconSync,
  cloneIconSvg,
  prewarmIcons,
  toExportsUrl,
  loadBuiltinSprite,
  _resetIconCache,
} from './icon-cache.js';
//=============================================================================
const SVG = '<svg xmlns="http://www.w3.org/2000/svg" fill="#123456" stroke="#abcdef"><rect/></svg>';
beforeEach(() => {
  _resetIconCache();
});
//=============================================================================
afterEach(() => {
  vi.restoreAllMocks();
});
//=============================================================================
describe('toExportsUrl', () => {
  test('rewrites a root-level library icon to its exports/ path', () => {
    expect(toExportsUrl('http://h/x/libraries/math/gain.svg')).toBe(
      'http://h/x/libraries/math/exports/gain.svg'
    );
  });

  test('leaves an already-exports path unchanged', () => {
    const u = 'http://h/libraries/math/exports/gain.svg';
    expect(toExportsUrl(u)).toBe(u);
  });

  test('leaves data URIs and non-matching paths unchanged', () => {
    expect(toExportsUrl('data:image/svg+xml,<svg/>')).toBe('data:image/svg+xml,<svg/>');
    expect(toExportsUrl('http://h/libraries/math/icons/deep/gain.svg')).toBe(
      'http://h/libraries/math/icons/deep/gain.svg'
    );
  });

  test('preserves a query suffix', () => {
    expect(toExportsUrl('http://h/libraries/math/gain.svg?v=2')).toBe(
      'http://h/libraries/math/exports/gain.svg?v=2'
    );
  });
});
//=============================================================================
describe('getIcon', () => {
  test('fetches once and parses colors; getIconSync returns the cached entry', async () => {
    const fetchMock = vi.fn().mockResolvedValue({ ok: true, text: async () => SVG });
    (global as any).fetch = fetchMock;

    expect(getIconSync('u1')).toBeUndefined();
    const a = await getIcon('u1');
    expect(a.ok).toBe(true);
    expect(a.svgEl).not.toBeNull();
    expect(a.colors).toEqual({ fill: '#123456', stroke: '#abcdef' });

    const cached = getIconSync('u1');
    expect(cached).toBe(a);

    await getIcon('u1');
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

  test('dedupes concurrent in-flight requests for the same URL', async () => {
    let resolve!: (v: unknown) => void;
    const fetchMock = vi.fn().mockReturnValue(
      new Promise((r) => {
        resolve = r;
      })
    );
    (global as any).fetch = fetchMock;

    const p1 = getIcon('u2');
    const p2 = getIcon('u2');
    resolve({ ok: true, text: async () => SVG });
    await Promise.all([p1, p2]);
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

  test('caches a failed fetch without throwing', async () => {
    (global as any).fetch = vi.fn().mockResolvedValue({ ok: false });
    const r = await getIcon('bad');
    expect(r.ok).toBe(false);
    expect(r.svgEl).toBeNull();
    expect(cloneIconSvg('bad')).toBeNull();
  });
});
//=============================================================================
describe('loadBuiltinSprite', () => {
  const SPRITE =
    '<svg xmlns="http://www.w3.org/2000/svg" style="display:none">' +
    '<symbol id="math__gain" data-lib="math" data-name="gain.svg" viewBox="0 0 80 60" fill="#111111"><path/></symbol>' +
    '<symbol id="source__step" data-lib="source" data-name="step.svg" viewBox="0 0 72 72"><rect id="s1" stroke="#222222"/></symbol>' +
    '</svg>';

  test('fetches once and pre-populates the cache at exports/ URLs', async () => {
    const fetchMock = vi.fn().mockResolvedValue({ ok: true, text: async () => SPRITE });
    (global as any).fetch = fetchMock;

    await loadBuiltinSprite('http://h/app/../libraries/_builtin-icons.sprite.svg');

    const gain = getIconSync('http://h/libraries/math/exports/gain.svg');
    expect(gain?.ok).toBe(true);
    expect(gain?.svgEl?.getAttribute('viewBox')).toBe('0 0 80 60');
    expect(gain?.colors).toEqual({ fill: '#111111' });

    const step = getIconSync('http://h/libraries/source/exports/step.svg');
    expect(step?.svgEl?.querySelector('#s1')).not.toBeNull();
    expect(step?.colors).toEqual({ stroke: '#222222' });

    await loadBuiltinSprite('http://h/libraries/_builtin-icons.sprite.svg');
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });
});
//=============================================================================
describe('prewarmIcons', () => {
  test('fetches every unique URL once', async () => {
    const fetchMock = vi.fn().mockResolvedValue({ ok: true, text: async () => SVG });
    (global as any).fetch = fetchMock;

    await prewarmIcons(['a', 'b', 'a', 'b', 'c']);
    expect(fetchMock).toHaveBeenCalledTimes(3);
    expect(getIconSync('a')?.svgEl).not.toBeNull();
    expect(cloneIconSvg('c')).not.toBeNull();
  });
});
//=============================================================================
