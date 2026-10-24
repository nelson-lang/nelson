//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// NFlow product version, injected at build time from the single source of
// truth (modules/nflow_engine/src/include/NFlowVersion.hpp) via the `define`
// block in vite.config.ts (Vite build, dev server, and Vitest all apply it).
// The fallback only appears if the sources are consumed outside that pipeline.
//=============================================================================
declare const __NFLOW_VERSION__: string;
export const NFLOW_VERSION: string =
  typeof __NFLOW_VERSION__ !== 'undefined' ? __NFLOW_VERSION__ : '0.0.0-dev';
//=============================================================================
