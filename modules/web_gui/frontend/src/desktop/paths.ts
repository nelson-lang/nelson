//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Host paths come from the engine with either separator; nothing here touches the file system.
//=============================================================================
/** The last component of a path (the path itself when it has no separator). */
export function baseName(path: string): string {
  return path.split(/[\\/]/).pop() || path;
}
//=============================================================================
/** The directory part of a path ('' when it has no separator). */
export function dirName(path: string): string {
  const cut = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
  return cut > 0 ? path.slice(0, cut) : '';
}
//=============================================================================
