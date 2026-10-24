//=============================================================================
// Copyright (c) 2024-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
export function buildTitleName(diagramName?: unknown): string {
  const appName = 'NFlow';
  try {
    const name = diagramName && String(diagramName).trim() ? String(diagramName).trim() : '';
    return name ? `${appName} - ${name}` : appName;
  } catch (_e) {
    return appName;
  }
}
//=============================================================================
