// ============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// ============================================================================
// This file is part of Nelson.
// ============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
// ============================================================================

export function inlineStylesheetImport(stylesheet, importRule, importedStylesheet) {
  const occurrences = stylesheet.split(importRule).length - 1;
  if (occurrences !== 1) {
    throw new Error(`Expected exactly one shared stylesheet import, found ${occurrences}.`);
  }
  return stylesheet.replace(importRule, importedStylesheet.trimEnd());
}
