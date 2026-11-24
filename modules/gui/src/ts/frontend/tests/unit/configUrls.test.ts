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
import { DEFAULT_CONFIG_URLS } from '../../src/desktop/configUrls';
import { getConfigUrls, setDesktopCapabilities } from '../../src/desktop/capabilities';
//=============================================================================
describe('config URLs', () => {
  // DEFAULT_CONFIG_URLS is built at build time from etc/defaults.conf (?raw), so
  // this also guards that file's key names and values against drift.
  it('derives the bundled defaults from etc/defaults.conf', () => {
    expect(DEFAULT_CONFIG_URLS).toEqual({
      website: 'https://nelson-lang.github.io/nelson-website/',
      issues: 'https://github.com/nelson-lang/nelson/issues',
      docbook: 'https://nelson-lang.github.io/nelson-gitbook/releases',
      update: 'https://api.github.com/repos/nelson-lang/nelson/releases/latest',
      newsfeed: 'https://nelson-lang.github.io/nelson-website/feed.xml',
      releases: 'https://github.com/nelson-lang/nelson/releases.atom',
    });
  });

  it('falls back to the bundled defaults until capabilities arrive', () => {
    expect(getConfigUrls()).toEqual(DEFAULT_CONFIG_URLS);
  });

  it('overrides with engine-provided URLs and keeps them across polls', () => {
    const urls = { ...DEFAULT_CONFIG_URLS, website: 'https://staging.example/' };
    setDesktopCapabilities({ nflow: false, urls });
    expect(getConfigUrls().website).toBe('https://staging.example/');
    // A later poll that omits urls must not clobber the ones already learned.
    setDesktopCapabilities({ nflow: true });
    expect(getConfigUrls().website).toBe('https://staging.example/');
  });
});
//=============================================================================
