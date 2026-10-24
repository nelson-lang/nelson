//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
import dashboardManifest from '../../../../../nflow_blocks/libraries/dashboard/library.json';
import dashboardContract from '../../../../../nflow_blocks/libraries/dashboard/contract.json';
import { buildRegistryFromManifest } from '../generic-block-renderer.js';
import type { LibraryManifest } from '../block-descriptor.js';
import { dashboardRenderer } from './dashboard.js';
//=============================================================================
export { dashboardContract, dashboardManifest };
const manifestDefs = buildRegistryFromManifest(
  dashboardManifest as unknown as LibraryManifest
).getDefinitions();
//=============================================================================
export const blockDefinitions = Object.fromEntries(
  Object.keys(manifestDefs).map((type) => [type, { render: dashboardRenderer }])
);
//=============================================================================
