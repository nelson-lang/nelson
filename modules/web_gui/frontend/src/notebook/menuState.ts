//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================

export type NotebookMenu = 'save' | 'insert' | 'export';

/** Keep the toolbar menus mutually exclusive while preserving toggle behavior. */
export function toggleNotebookMenu(
  current: NotebookMenu | null,
  requested: NotebookMenu
): NotebookMenu | null {
  return current === requested ? null : requested;
}
