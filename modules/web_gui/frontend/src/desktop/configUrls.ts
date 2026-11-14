//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import type { ConfigUrls } from '../rpc/types';
// Single source of truth: the engine's etc/defaults.conf, inlined at build time
// (Vite ?raw; its fs sandbox already allows the repository root). At runtime the
// engine-authoritative values arrive through app.capabilities; this fallback only
// covers the window before the first poll resolves.
import defaultsConf from '../../../../../etc/defaults.conf?raw';
//=============================================================================
const defaults = JSON.parse(defaultsConf) as Record<string, string>;
//=============================================================================
export const DEFAULT_CONFIG_URLS: ConfigUrls = {
  website: defaults.website_url,
  issues: defaults.issues_url,
  docbook: defaults.docbook_url,
  update: defaults.update_url,
  newsfeed: defaults.newsfeed_url,
  releases: defaults.releases_url,
};
//=============================================================================
