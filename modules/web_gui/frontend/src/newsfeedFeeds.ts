//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Feed URLs shared by the newsfeed panel and the WebAssembly newsfeed service.
export const DEFAULT_FEED = 'https://nelson-lang.github.io/nelson-website/feed.xml';
export const RELEASES_FEED = 'https://github.com/nelson-lang/nelson/releases.atom';
export const DEFAULT_FEEDS: readonly string[] = [DEFAULT_FEED, RELEASES_FEED];
//=============================================================================
